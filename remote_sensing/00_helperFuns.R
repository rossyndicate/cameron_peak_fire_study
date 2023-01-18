
# Base ggplot2 theme ------------------------------------------------------

plot_theme <- function(...) {
  theme_pubr() +
    cowplot::theme_half_open(12) +
    cowplot::background_grid()+
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 16),
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10, l = -10)),
      axis.title.x = element_text(margin = margin(t = 10, b = -10)),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 16
      ),
      axis.text.y = element_text(size = 16),
      # strip.text = element_text(size=8),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16),
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 16,
        face = "bold",
        color = "#000000",
        fill = "#FFFFFF",
        box.color = "#000000",
        halign = 0.5,
        linetype = 1,
        r = unit(2, "pt"),
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0),
        margin = margin(3, 3, 3, 3)
      ),
      
      #additional settings passed to theme()
      ...
    )
  
}

dark_theme <- function(...) {
  theme_pubr() +
    cowplot::theme_half_open(12) +
    cowplot::background_grid()+
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 16),
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10, l = -10)),
      axis.title.x = element_text(margin = margin(t = 10, b = -10)),
      # axis.text.x = element_text(
      #   angle = 45,
      #   hjust = 1,
      #   size = 16
      # ),
      axis.text.y = element_text(size = 16),
      # strip.text = element_text(size=8),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16),
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 16,
        face = "bold",
        color = "#FFFFFF",
        fill = "#000000",
        box.color = "#000000",
        halign = 0.5,
        linetype = 1,
        r = unit(2, "pt"),
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0),
        margin = margin(3, 3, 3, 3)
      ),
      
      #additional settings passed to theme()
      ...
    )
  
}



#Make default theme for all .R and .Rmd files
# theme_set(dark_theme())
theme_set(plot_theme())

# Base ggplot2 theme for maps ------------------------------------------------------

map_theme <- function(caption.hjust=1, caption.vjust=0, ...) {
  theme_void() + theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
    plot.title = element_text(
      size=8,
      face="bold"
    ),
    plot.caption = element_text(
      size=8,
      hjust = caption.hjust,
      vjust = caption.vjust
    ),
    plot.subtitle = element_text(
      size=8
    ),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size=8
    ),
    legend.title = element_text(
      size=8,
      face="bold"
    ),
    legend.text = element_text(
      size=8
    ),
    strip.text = element_text(
      size=8,
      face="bold"
    ),
    #additional settings passed to theme()
    ...
  )
}

#NOT IN 
`%nin%` <- Negate(`%in%`)


rescale_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

squish <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    TRUE ~ x
  )
}

# FUI - Create Forel-Ule Color table --------------------------------------


#Function for calculating DWL from red, green, and blue bands

fui.hue <- function(R, G, B) {
  
  # Convert R,G, and B spectral reflectance to dominant wavelength based
  # on CIE chromaticity color space
  
  # see Wang et al 2015. MODIS-Based Radiometric Color Extraction and
  # Classification of Inland Water With the Forel-Ule
  # Scale: A Case Study of Lake Taihu
  
  require(colorscience)
  # chromaticity.diagram.color.fill()
  Xi <- 2.7689*R + 1.7517*G + 1.1302*B
  Yi <- 1.0000*R + 4.5907*G + 0.0601*B
  Zi <- 0.0565*G + 5.5943*B
  
  # calculate coordinates on chromaticity diagram
  x <-  Xi / (Xi + Yi +  Zi)
  y <-  Yi / (Xi + Yi +  Zi)
  z <-  Zi / (Xi + Yi +  Zi)
  
  # calculate hue angle
  alpha <- atan2((x - 0.33), (y - 0.33)) * 180/pi
  
  # make look up table for hue angle to wavelength conversion
  cie <- cccie31 %>%
    mutate(a = atan2( (x - 0.33), (y - 0.33)) * 180/pi) %>%
    dplyr::filter(wlnm <= 700) %>%
    dplyr::filter(wlnm >=380)
  
  # find nearest dominant wavelength to hue angle
  wl <- cie[as.vector(sapply(alpha,function(x) which.min(abs(x - cie$a)))), 'wlnm']
  
  #out <- cbind(as.data.frame(alpha), as.data.frame(wl))
  
  return(wl)
}

#Connect dWL to the forel ule index for visualization
#The Forel-Ule Index (FUI) is a useful comprehensive indicator to show the water colour variability and water quality change in both inland waters and oceans.
fui.lookup <- tibble(dWL = c(471:583), fui = NA)
fui.lookup$fui[fui.lookup$dWL <= 583] = 21
fui.lookup$fui[fui.lookup$dWL <= 581] = 20
fui.lookup$fui[fui.lookup$dWL <= 579] = 19
fui.lookup$fui[fui.lookup$dWL <= 577] = 18
fui.lookup$fui[fui.lookup$dWL <= 575] = 17
fui.lookup$fui[fui.lookup$dWL <= 573] = 16
fui.lookup$fui[fui.lookup$dWL <= 571] = 15
fui.lookup$fui[fui.lookup$dWL <= 570] = 14
fui.lookup$fui[fui.lookup$dWL <= 569] = 13
fui.lookup$fui[fui.lookup$dWL <= 568] = 12
fui.lookup$fui[fui.lookup$dWL <= 567] = 11
fui.lookup$fui[fui.lookup$dWL <= 564] = 10
fui.lookup$fui[fui.lookup$dWL <= 559] = 9
fui.lookup$fui[fui.lookup$dWL <= 549] = 8
fui.lookup$fui[fui.lookup$dWL <= 530] = 7
fui.lookup$fui[fui.lookup$dWL <= 509] = 6
fui.lookup$fui[fui.lookup$dWL <= 495] = 5
fui.lookup$fui[fui.lookup$dWL <= 489] = 4
fui.lookup$fui[fui.lookup$dWL <= 485] = 3
fui.lookup$fui[fui.lookup$dWL <= 480] = 2
fui.lookup$fui[fui.lookup$dWL <= 475 & fui.lookup$dWL >470] = 1


# Actual Forel-Ule Colors
fui.colors <- tibble(color = c(
  "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660","#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04"),
  fui = 1:21)


# Coefficient of variation (cv) -------------------------------------------


cv <- function(x, na.rm = TRUE)  {
  sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}



# Convert to ts object ----------------------------------------------------


#  function for converting to time series
ts_conv <- function(data){
  data = data %>%
    arrange(date) %>%

    na.trim() %>%
    as.data.frame(.)
  dat_ts = xts(x = data[, "value"],
               order.by = data[, "date"])
  dat_ts = na.trim(na_interpolation(dat_ts, option = "stine"))
}


# Get time indices related to datetime ------------------------------------


# function to get time indices related to datetimes
index_fun <- function(ts_data){
  ts_data = tibble(date = index(ts_data),
                   timeindex = row_number(date),
                   value = as.numeric(ts_data))
}


# select = dplyr::select --------------------------------------------------


#Override select issue
select <- dplyr::select

#Over map from some other package that's driving me crazy
map <- purrr::map
