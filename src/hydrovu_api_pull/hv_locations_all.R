#' Return the list of locations for the given client from HydroVu
#'
#' @param client a valid OAuth2 token such as returned from \code{hv_auth()}
#' @param url HydroVu url that lists the locations
#'
#' @return a dataframe listing all the locations visible to the client
#' @export
#'
#' @examples
#' \dontrun{
#' locs <- hv_locations(client)
#' }

hv_locations_all <- function(client,
                         url = "https://www.hydrovu.com/public-api/v1/locations/list") {

  req <- httr2::request(url)

  try({
  resp <-  req %>% httr2::req_oauth_client_credentials(token) %>% httr2::req_perform()
  locs <- list(resp %>% httr2::resp_body_json())
  h <- resp %>% httr2::resp_headers()

  while (!is.null(h[["X-ISI-Next-Page"]]))
  {
    resp2 <- req %>%
      httr2::req_headers("X-ISI-Start-Page" = h[["X-ISI-Next-Page"]]) %>%
      httr2::req_oauth_client_credentials(client) %>%
      httr2::req_perform()
    locs <- c(locs, list(resp2 %>% httr2::resp_body_json()))
    h <- resp2 %>% httr2::resp_headers()
  }
  # collapse the paginated date and clean up
  df <- flatten_df(locs) %>%
    select(-gps) %>%
    filter(!duplicated(.))
  return(df)
  })
}

