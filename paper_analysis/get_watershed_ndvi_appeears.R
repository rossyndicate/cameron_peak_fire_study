# Watershed annual peak NDVI from NASA AppEEARS
#
# Pulls MODIS MOD13Q1 16-day NDVI statistics for watershed polygons using the
# NASA AppEEARS API. For each watershed and year, the script:
# 1. Requests watershed-average NDVI for each 16-day MODIS observation
# 2. Identifies the date with the highest mean watershed NDVI
# 3. Summarizes peak annual NDVI across each year
# 4. Saves the time series, annual peaks, summary statistics, and a plot

library(sf)
library(httr2)
library(askpass)
library(jsonlite)
library(tidyverse)

# AppEEARS API settings

APPEEARS_API <- "https://appeears.earthdatacloud.nasa.gov/api"

# HTTP errors that are often temporary:
TRANSIENT_CODES <- c(408, 425, 429, 500, 502, 503, 504)

# Return x unless it is NULL/empty; otherwise return y.
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

# API pull functions:

# Add individual path components to an httr2 request.
add_path <- function(req, path) {
  for (x in path) req <- req_url_path_append(req, x)
  req
}


# Perform an API request with automatic retries for temporary server/network
# failures. HTTP errors are returned as responses so they can be handled below.
perform_request <- function(req) {
  req %>%
    req_retry(
      max_tries = 8,
      retry_on_failure = TRUE,
      is_transient = function(resp) resp_status(resp) %in% TRANSIENT_CODES
    ) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
}


# AppEEARS authentication

# Log into AppEEARS using NASA Earthdata credentials and get a token.
# Username can be stored in the EARTHDATA_USERNAME environment variable;
# the password is requested interactively so it is not stored in the script.
appeears_login <- function(username = Sys.getenv("EARTHDATA_USERNAME")) {
  
  if (!nzchar(username)) username <- readline("NASA Earthdata username: ")
  password <- askpass::askpass("NASA Earthdata password: ")
  
  resp <- request(APPEEARS_API) %>%
    req_url_path_append("login") %>%
    req_auth_basic(username, password) %>%
    req_method("POST") %>%
    req_body_raw(raw(0)) %>%
    req_timeout(60) %>%
    perform_request()
  
  if (resp_status(resp) != 200) {
    stop("AppEEARS login failed: ", resp_body_string(resp))
  }
  
  token <- resp_body_json(resp, simplifyVector = TRUE)$token %||% ""
  
  if (!nzchar(token)) stop("No AppEEARS token returned.")
  
  token
}


# Make an authenticated GET request to an AppEEARS API endpoint.
appeears_get <- function(path, token, timeout = 60) {
  
  req <- add_path(request(APPEEARS_API), path) %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    req_timeout(timeout)
  
  perform_request(req)
}

# Prep watershed polygons

prepare_watersheds <- function(watersheds, id_col = "site_code") {
  
  stopifnot(inherits(watersheds, "sf"), id_col %in% names(watersheds))
  
  # Keep only the watershed ID and geometry, repair invalid polygons, and
  # transform to geographic coordinates required for GeoJSON/AppEEARS.
  watersheds <- watersheds %>%
    select(all_of(id_col)) %>%
    st_make_valid() %>%
    st_transform(4326) %>%
    filter(!st_is_empty(.)) %>%
    arrange(.data[[id_col]])
  
  # AppEEARS returns area statistics using an internal polygon ID ("aid").
  # Create stable IDs here so returned statistics can be joined to site_code.
  watersheds$appeears_aid <- sprintf("aid%04d", seq_len(nrow(watersheds)))
  
  watersheds
}


# Convert an sf object into the nested R-list representation of GeoJSON
# expected in the AppEEARS task request.
as_geojson <- function(x) {
  
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  
  st_write(x,
           path,
           driver = "GeoJSON",
           delete_dsn = TRUE,
           quiet = TRUE,
           layer_options = "RFC7946=YES")
  
  fromJSON(paste(readLines(path, warn = FALSE), collapse = "\n"),
           simplifyVector = FALSE)
}


# Grab annual NDVI
submit_ndvi_task <- function(watersheds, token, year,
                             season_start = "01-01",
                             season_end = "12-31") {
  
  # Request MOD13Q1 Collection 6.1, which provides NDVI at 250-m resolution
  # every 16 days. AppEEARS calculates area statistics for each watershed.
  body <- list(task_type = "area",
               task_name = paste0("watershed-ndvi-", year),
               params = list(
                 # Process one calendar year at a time.
                 dates = list(list(startDate = paste0(season_start, "-", year),
                                   endDate = paste0(season_end, "-", year),
                                   recurring = FALSE)),
                 
                 # MODIS Terra Vegetation Indices 16-Day L3 Global 250 m product.
                 layers = list(list(product = "MOD13Q1.061",
                                    layer = "_250m_16_days_NDVI")),
                 
                 # NetCDF is requested as the primary output, but AppEEARS also generates
                 # statistics CSV files that are downloaded and used below.
                 output = list(format = list(type = "netcdf4"),
                               projection = "native"),
                 
                 # Watershed polygons over which AppEEARS calculates summary statistics.
                 geo = as_geojson(watersheds)))
  
  resp <- request(APPEEARS_API) %>%
    req_url_path_append("task") %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    req_body_json(body, auto_unbox = TRUE) %>%
    req_timeout(120) %>%
    perform_request()
  
  if (!resp_status(resp) %in% c(200, 201, 202)) {
    stop("Task submission failed: ", resp_body_string(resp))
  }
  
  task_id <- resp_body_json(resp, simplifyVector = TRUE)$task_id %||% ""
  
  if (!nzchar(task_id)) {
    stop("Task submitted but no task_id was returned.")
  }
  
  message("Submitted ", year, ": ", task_id)
  
  task_id
}

# Wait for an AppEEARS task to finish
wait_for_task <- function(task_id, token, poll_seconds = 60) {
  
  repeat {
    
    resp <- appeears_get(c("task", task_id), token)
    
    # AppEEARS tokens expire, so stop with an informative message rather than
    # repeatedly querying with an invalid token.
    if (resp_status(resp) == 401) {
      stop("Token expired. Log in again and rerun with the same task_id.")
    }
    
    if (resp_status(resp) != 200) {
      stop("Could not retrieve task status: ",
           resp_body_string(resp))
    }
    
    task <- resp_body_json(resp,
                           simplifyVector = FALSE)
    
    status <- tolower(task$status %||% "unknown")
    
    message("AppEEARS status: ", status)
    
    # Finished successfully.
    if (status == "done") return(task)
    
    # Stop immediately if AppEEARS reports a terminal failure.
    if (status %in% c("error", "failed", "cancelled", "expired", "deleted")) {
      stop("AppEEARS task ended with status: ", status)
    }
    
    # Otherwise wait before checking task status again.
    Sys.sleep(poll_seconds)
  }
}

# Download AppEEARS watershed statistics
download_statistics <- function(task_id, token, output_dir) {
  
  dir.create(output_dir,
             recursive = TRUE,
             showWarnings = FALSE)
  
  # AppEEARS occasionally reports a task as complete before the downloadable
  # bundle is fully available, so retry until the bundle can be listed.
  repeat {
    
    resp <- appeears_get(c("bundle", task_id), token)
    
    if (resp_status(resp) == 200) break
    
    if (!resp_status(resp) %in% c(404, 409, TRANSIENT_CODES)) {
      stop("Could not list task files: ", resp_body_string(resp))
    }
    
    Sys.sleep(30)
  }
  
  # Identify only the AppEEARS area-statistics CSV files from the complete
  # output bundle. The NetCDF output itself is not needed for this workflow.
  files <- resp_body_json(resp, simplifyVector = FALSE)$files %>%
    map_dfr(~tibble(file_id = .x$file_id,
                    file_name = .x$file_name)) %>%
    filter(grepl(
      "statistics\\.csv$",
      file_name,
      ignore.case = TRUE))
  
  if (!nrow(files)) {
    stop("No AppEEARS statistics CSV was returned.")
  }
  
  # Download each statistics file and return its local path.
  map2_chr(files$file_id, files$file_name, function(file_id, file_name) {
    
    resp <- appeears_get(c("bundle", task_id, file_id), token, timeout = 180)
    
    if (resp_status(resp) != 200) {
      stop("Download failed: ", file_name)
    }
    
    path <- file.path(output_dir, basename(file_name))
    
    writeBin(resp_body_raw(resp), path)
    
    path
  }
  )
}

# Read and clean AppEEARS NDVI statistics
read_ndvi_statistics <- function(files, aid_map) {
  
  # Combine statistics files in case AppEEARS returns more than one.
  x <- map_dfr(files, ~read_csv(.x, show_col_types = FALSE))
  
  # Standardize returned AppEEARS column names.
  names(x) <- names(x) %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  # These fields are required to reconstruct the watershed NDVI time series.
  required <- c("aid", "date", "mean")
  
  if (!all(required %in% names(x))) {
    stop("Expected columns not found. Returned columns: ", paste(names(x), collapse = ", "))
  }
  
  x %>%
    
    # Convert AppEEARS area IDs back to the aid format created above and retain
    # the mean NDVI reported for each watershed/date.
    transmute(appeears_aid = sprintf(
      "aid%04d",
      as.integer(gsub("[^0-9]", "", aid))),
      date = as.Date(date),
      mean_ndvi = as.numeric(mean)) %>%
    
    # MOD13Q1 NDVI is commonly stored as an integer with a 0.0001 scale factor.
    # Apply the scale factor only when values indicate they are still unscaled.
    mutate(mean_ndvi = if_else(abs(mean_ndvi) > 2,
                               mean_ndvi * 0.0001,
                               mean_ndvi)) %>%
    
    # Keep only valid NDVI values.
    filter(!is.na(date),
           between(mean_ndvi, -1, 1)) %>%
    
    # Restore the original watershed/site identifier.
    left_join(aid_map, by = "appeears_aid") %>%
    
    # Collapse duplicate rows if AppEEARS returns multiple entries for a
    # watershed/date combination.
    group_by(site_code, date) %>%
    summarize(mean_ndvi = mean(mean_ndvi, na.rm = TRUE), .groups = "drop")
}

#Calculate annual peak NDVI and save outputs
save_ndvi_outputs <- function(series, output_dir) {
  
  # For each watershed and year, identify the 16-day MODIS observation with
  # the highest mean NDVI across the watershed.
  yearly <- series %>%
    mutate(year = as.integer(format(date, "%Y"))) %>%
    group_by(site_code, year) %>%
    slice_max(mean_ndvi, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(site_code,
              year,
              peak_date = date,
              peak_ndvi = mean_ndvi)
  
  # Summarize the distribution of annual peak NDVI across all available years
  # for each watershed.
  summary <- yearly %>%
    group_by(site_code) %>%
    summarize(years_available = n(),
              average_peak_ndvi = mean(peak_ndvi),
              sd_peak_ndvi = sd(peak_ndvi),
              minimum_peak_ndvi = min(peak_ndvi),
              maximum_peak_ndvi = max(peak_ndvi),
              .groups = "drop")
  
  dir.create(output_dir,
             recursive = TRUE,
             showWarnings = FALSE)
  
  # Save the complete 16-day watershed NDVI series.
  write_csv(series,
            file.path(output_dir, "ndvi_16day_watershed_means.csv"))
  
  # Save one annual peak NDVI value per watershed/year.
  write_csv(yearly, file.path(output_dir, "ndvi_yearly_peaks.csv"))
  
  # Save across-year peak NDVI summary statistics.
  write_csv(summary, file.path(output_dir, "ndvi_average_peak_summary.csv"))
  
  # Plot annual peak NDVI through time for each watershed.
  plot <- ggplot(yearly, aes(year, peak_ndvi, color = site_code)) +
    geom_line() +
    geom_point() +
    labs(x = NULL, y = "Peak NDVI", color = "Site") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "ndvi_yearly_peaks.png"),
         plot,
         width = 9,
         height = 6,
         dpi = 300)
  
  # Return all derived objects for immediate use
  list(series = series,
       yearly = yearly,
       summary = summary,
       plot = plot)
}

# Finish processing an already-submitted AppEEARS task
# Useful if a task was submitted previously or the original R session ended
# while AppEEARS was still processing the request.
finish_ndvi_task <- function(task_id,
                             watersheds,
                             id_col = "site_code",
                             output_dir = "data/raw/spatial/res_study/ndvi",
                             poll_seconds = 60) {
  
  watersheds <- prepare_watersheds(watersheds, id_col)
  
  # Lookup table linking AppEEARS polygon IDs to original watershed IDs.
  aid_map <- st_drop_geometry(watersheds) %>%
    transmute(appeears_aid,
              site_code = as.character(.data[[id_col]]))
  
  token <- appeears_login()
  
  # Wait for the existing task to finish, download the watershed statistics,
  # reconstruct the NDVI series, and create the final outputs.
  wait_for_task(task_id, token, poll_seconds)
  
  files <- download_statistics(task_id, token, file.path(output_dir, "appeears_tasks", task_id))
  
  series <- read_ndvi_statistics(files, aid_map)
  
  save_ndvi_outputs(series, output_dir)
}

# Main NDVI workflow
get_watershed_peak_ndvi <- function(watersheds,
                                    id_col = "site_code",
                                    start_year = 2016,
                                    end_year = 2024,
                                    season_start = "01-01",
                                    season_end = "12-31",
                                    output_dir = "data/raw/spatial/res_study/ndvi",
                                    poll_seconds = 60) {
  
  watersheds <- prepare_watersheds(watersheds, id_col)
  
  # Preserve the relationship between AppEEARS polygon IDs and site codes.
  aid_map <- st_drop_geometry(watersheds) %>%
    transmute(appeears_aid,
              site_code = as.character(.data[[id_col]]))
  
  # Log in once and reuse the same token across annual requests.
  token <- appeears_login()
  
  # Submit and process each year separately. This keeps individual AppEEARS
  # tasks smaller and makes failures easier to isolate/restart.
  series <- map_dfr(seq.int(start_year, end_year),
                    function(year) {
                      
                      # Submit the annual watershed NDVI request.
                      task_id <- submit_ndvi_task(watersheds, token, year, season_start, season_end)
                      
                      # Wait until AppEEARS finishes processing the request.
                      wait_for_task(task_id, token, poll_seconds)
                      
                      # Download the area-statistics output.
                      files <- download_statistics(task_id,
                                                   token,
                                                   file.path(output_dir, "appeears_tasks", task_id))
                      
                      # Convert the AppEEARS output into a watershed/date NDVI series.
                      read_ndvi_statistics(files, aid_map)
                    }) %>%
    # Remove any duplicated watershed/date observations that may occur across
    # downloaded task outputs.
    distinct(site_code,
             date,
             .keep_all = TRUE) %>%
    arrange(site_code, date)
  
  # Calculate annual peaks, save files, and return the results.
  save_ndvi_outputs(series, output_dir)
}


# Run workflow for CPF watersheds
# Read the watershed shapefile and retain the seven CPF study watersheds.
watersheds <- st_read("data/raw/spatial/res_study/cpf_watersheds.shp") %>%
  filter(site_code %in% c("BRNR", "CBRR", "COMR", "HORR", "JOER", "LNGR", "PTRR"))


# Retrieve MODIS watershed-average NDVI for 2016-2024 and calculate the annual
# maximum (peak greenness) for each watershed.
result <- get_watershed_peak_ndvi(watersheds = watersheds,
                                  start_year = 2016,
                                  end_year = 2024) %>%
  filter()


# View across-year summary statistics for each watershed.
result$summary

# View annual peak NDVI through time.
result$plot