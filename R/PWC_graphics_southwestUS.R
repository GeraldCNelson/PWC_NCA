source("R/basicSpatialVariables.R")
library(terra)
library(geodata)
library(ggplot2)
#library(data.table)
dir.create("graphics/cts/stressDaysRatiobyCrop/pwc_wbgt_out", F, F)
dir.create("data/wbgt", F, F)
dir.create("climdata", F, F)
path <- "data/wbgt/"
sspChoices <- c("ssp126", "ssp585")
startYearChoices <- c(2041, 2081)
graphicType <- "pdf"
defaultWidth <- 12
country_US <- gadm(country = "USA", level = 1, path = "data-raw/gadm/", resolution = 2)
states_abbr_SW <- c("CA", "AZ", "NM", "CO", "NV", "UT")
states_SW <- subset(country_US,country_US$HASC_1 %in% paste0("US.", states_abbr_SW))
ext_states_SW <- align(ext(states_SW), rast(res = 0.5), "out")
states_SW_sf <- sf::st_as_sf(states_SW)

legendSwitch <- TRUE
stressValue = 75
runlength <- 1
breaks <-   c(0, 10, 20, 30, 40, 50, 60, 70 , 90, 100)
regionChoice <- "states_SW"
yearRange = 19
qChoice = "q3"; qName <- "JAS"
outChoice <- "pwc_wbgt_out"
waterSource <- "rf"
palName <- "YlOrRd"
# palName_NCA <- "BuPu"
palCt <- 9
f_palette <- function(palName, palCt) {
  colorList <- (RColorBrewer::brewer.pal(palCt, palName)) 
}
colorList <- f_palette(palName, palCt)
colorValues <- colorList

f_quarterFancyName <- function(qName) {
  if (qName == "JFM") return("January to March")
  if (qName == "AMJ") return("April to June")
  if (qName == "JAS") return("July to September")
  if (qName == "OND") return("October to December")
  
  # if no name
  print(paste0("No quarter name like ", qName, ". Check the list"))
}

f_stressMetricText <- function(outChoice) {
  if (outChoice %in% c("pwc_wbgt_out", "wbgt_out")) stressMetric <- "WBGT"
  return(stressMetric)
}
f_waterSourceText <- function(waterSource) {
  if (waterSource == "ir") waterSourceText <- "irrigated"
  if (waterSource == "rf") waterSourceText <- "rainfed"
  return(waterSourceText)
}

f_sspText <- function(k) {
  if (k == "historical") kText <- ""
  if (k == "ssp126") kText <- "SSP1-2.6"
  if( k == "ssp585") kText <- "SSP5-8.5"
  return(kText)
}

f_periodFancyName <- function(yearSpan) {
  if (yearSpan == "1991_2010") yearSpanText <- "historical (1991-2010)"
  if (yearSpan == "2041_2060") yearSpanText <- "mid century"
  if (yearSpan == "2081_2100") yearSpanText <- "end century"
  return(yearSpanText)
}

f_regionFancyName <- function(regionChoice) {
  if (regionChoice == "states_SW") return("Southwest US")
}

f_h <- function(regionExtent, defaultWidth) {
  x <- regionExtent[2] - regionExtent[1]
  y <- regionExtent[4] - regionExtent[3] 
  h <- defaultWidth/(x/y)
  return(h)
}

f_thi_cts_crop_graphing <- function(k, l, stressValue, qChoice, waterSource, outChoice, regionChoice) {
  yearSpan <- paste0(l, "_", l + yearRange)
  regionExtent <- ext_states_SW
  cropMask_region <- crop(cropMask_globe, regionExtent) # 1-0, locations 
  qName <-"JAS"
  border <- get(regionChoice)
  border <- crop(border, regionExtent)
  border_sf <- sf::st_as_sf(border)
  border_proj_sf <- sf::st_transform(border_sf, proj_to_use)
  
  crop_loc_globe <- cropMask_region
  crop_loc_region <- any(crop(crop_loc_globe, regionExtent))
  crop_loc_region[crop_loc_region == 0] <- NA
  if (is.na(minmax(crop_loc_region)[1])) stop(paste0("crop area of ", qName, " in region ", regionChoice, " is zero."))
  # convert crop_loc_region to polygons and to sf
  crop_loc_region_p <- as.polygons(crop_loc_region, dissolve = TRUE, na.rm = TRUE)
  crop_loc_region_p_proj <- project(crop_loc_region_p, proj_to_use)
  crop_loc_region_p_proj_sf <- sf::st_as_sf(crop_loc_region_p_proj)
  
  filename_PWC_daily_mean_in <- paste0(path, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif")
  PWCvals <- rast(filename_PWC_daily_mean_in)
  PWCvals_region <- crop(PWCvals, regionExtent)
  PWCvals_region_masked <- mask(PWCvals_region, cropMask_region, maskvalues = NA)
  # PWCvals_region_masked <- mask(PWCvals_region, border)
  PWCvals_region_masked <- mask(PWCvals_region_masked, coastline, inverse = TRUE, touches = FALSE) #  removes coastal pixels ------
  
  stress <- 1 * (PWCvals_region_masked < stressValue) # daily locations where PWC is less than the stress cutoff, TRUE/FALSE
  if (qChoice %in% c("q1", "q2", "q3", "q4")) {
    dayCt_quarters <- c(90, 91, 92, 92) # days in each nonleap year quarter - https://www.google.com/search?client = safari&rls = en&q = number+of+days+in+a+quarter&ie = UTF-8&oe = UTF-8
    qnum <- as.numeric(gsub("q", "", qChoice))
    maturity_days <- cumsum(dayCt_quarters)
    plant_days <- maturity_days - dayCt_quarters + 1
    gsl <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = dayCt_quarters[qnum], ext = regionExtent)
    plant <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = plant_days[qnum], ext = regionExtent)
    maturity <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = maturity_days[qnum], ext = regionExtent)
  } else {
    calendar_in <- paste0("data-raw/crops/cropCalendars_ggcmi/", qChoice, "_", waterSource, "_ggcmi_crop_calendar_phase3_v1.01.nc4")
    temp <- rast(calendar_in)
    temp <- crop(temp, regionExtent)
    plant <- temp$planting_day
    maturity <- temp$maturity_day
    gsl <- temp$growing_season_length
  }
  gsl_region <- crop(gsl, regionExtent)
  #   gsl_region_masked <- mask(gsl_region_masked, border)
  gsl_region_masked <- mask(gsl_region, cropMask_region, maskvalues = NA)
  gsl_region_masked <- mask(gsl_region_masked, coastline, inverse = TRUE, touches = FALSE)
  fileName_out_stress_days <- paste0(path, "crops/", regionChoice, "_", qChoice, "_", waterSource, "_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".tif")
  print(system.time(r_stress <- rapp(stress, plant, maturity, sum, na.rm = TRUE, circular = TRUE, filename = fileName_out_stress_days, overwrite = TRUE)))
  names(r_stress) <- "stressCts"
  stress_max <- as.numeric(global(r_stress, max, na.rm = TRUE)) +5 # adding 5 bumps the maximum range which deals with some of the seq issues for some locations
  gsl_max <- as.numeric(global(gsl_region_masked, max, na.rm = TRUE)) + 1 # adding 1 bumps the maximum range
  #breaks <- ceiling(seq(from = 0, to = gsl_max, length.out = palCt+1)) 
  
  # prepare for graphing with proj_to_use
  r_stress_p <- as.polygons(r_stress, dissolve = TRUE, na.rm = TRUE)
  r_stress_p_proj <- project(r_stress_p, proj_to_use)
  r_stress_p_proj_sf <- sf::st_as_sf(r_stress_p_proj)
  r_stress_p_proj_sf <- sf::st_intersection(r_stress_p_proj_sf, border_proj_sf)
  r_stress_p_proj_sf$stressCts_disc <- cut(r_stress_p_proj_sf$stressCts, breaks = breaks, include.lowest = TRUE, right = TRUE)
  r_stress_p_proj_sf$stressCts_disc_f <- as.factor(r_stress_p_proj_sf$stressCts_disc)
  
  legendTitle <- paste0("Days with PWC below ", stressValue, " (%)")
  yearSpanText <- f_periodFancyName(yearSpan)
  sspText <- f_sspText(k)
  regionFancyName <- f_regionFancyName(regionChoice) 
  waterSourceText <- f_waterSourceText(waterSource)
  stressMetric <- f_stressMetricText(outChoice) # returns WBGT or UTCI for use in captions, etc.
  waterSourceText_upper <- stringr::str_to_title(waterSourceText)
  titleText_start <- paste0(waterSourceText_upper, " " , qName, ", growing season days with PWC below ", stressValue, " percent, ")
  titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
  if (qChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
    titleText <- paste0("Days with PWC below ", stressValue, " percent, ", f_quarterFancyName(qName), ", ",  sspText, ", ", yearSpanText, ", ", regionFancyName)
    caption_land_area <- "Land areas in white had no crops in early 21st century." #Rectangles show 1/2 degree grid."  
    caption <- caption_land_area   
  }
  if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
  
  gridLineSize <- 0.1; borderLineSize <- 0.2
  
  # share of stress days in gsl days -------------------------
  gsl_ratio <- 100 * r_stress/gsl_region_masked
  gsl_ratio[gsl_ratio > 100] <- 100 # to deal with a few rare cases
  names(gsl_ratio) <- "stressShare"
  
  gsl_ratio_p <- as.polygons(gsl_ratio, dissolve = TRUE, na.rm = TRUE)
  gsl_ratio_p_proj <- project(gsl_ratio_p, proj_to_use)
  gsl_ratio_p_proj_sf <- sf::st_as_sf(gsl_ratio_p_proj)
   gsl_ratio_p_proj_sf$stressShare_disc <- cut(gsl_ratio_p_proj_sf$stressShare, breaks = breaks, include.lowest = TRUE, right = TRUE)
  gsl_ratio_p_proj_sf <- sf::st_intersection(gsl_ratio_p_proj_sf, border_proj_sf)
  gsl_ratio_p_proj_sf$stressShare_disc_f <- as.factor(gsl_ratio_p_proj_sf$stressShare_disc)
  legendTitle <- paste0("Period share (%), days with\nPWC below ", stressValue, "%")
  titleText_start <- paste0(waterSourceText_upper, " " , qName, ", share of days in period with PWC below\n", stressValue, " percent, ")
  titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
  if (qChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
    titleText_start <- paste0(f_quarterFancyName(qName), ", share of days in period with PWC below\n ", stressValue, " percent, ")
    titleText <- paste0(titleText_start, sspText, ", ", yearSpanText, ", ", regionFancyName)
  }
  if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
  
  caption <- caption_land_area
  
  g <- ggplot(gsl_ratio_p_proj_sf, aes(fill = stressShare_disc_f)) +
    scale_fill_manual(values = colorValues, drop = FALSE) +
    geom_sf(show.legend = legendSwitch, size = gridLineSize) +
    labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) +
    theme_bw() +
    theme(
      legend.text.align = 1,
       axis.ticks = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(hjust = 0, vjust = 3.0, size = 8),
      legend.margin = margin(-20, 0, 0, 0)
    ) +
    theme(legend.position = 'bottom') +
    geom_sf(data = border_proj_sf,
            color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
    theme(legend.margin = margin(-20, 0, 0, 0)) +
    theme(legend.text=element_text(size=9)) +
    theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
  print(g)
  # browser()
  h <- f_h(regionExtent, defaultWidth)
  fileName_stressDaysRatio_out <- paste0("graphics/cts/stressDaysRatiobyCrop/", outChoice, "/stressDaysGSShare_", palName, "_", palCt, "_", regionChoice, "_", waterSource, "_", outChoice, "_daysCts_", runlength,  "_stressValue_", stressValue, "_", k, "_", yearSpan, ".", graphicType)
  ggsave(filename = fileName_stressDaysRatio_out, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300) 
  # https://pdfcrop.sourceforge.net - source of pdfcrop. Open source
  system2('pdfcrop', c(fileName_stressDaysRatio_out, fileName_stressDaysRatio_out)) # gets rid of white space around the figure in the pdf
  print(paste0("fileName out: ", fileName_stressDaysRatio_out))
}

dir.create("graphics/cts/stressDaysRatiobyCrop/pwc_wbgt_out/", F, F)


#test data
k = "historical"
l = 1991
f_thi_cts_crop_graphing(k, l, stressValue, qChoice, waterSource, outChoice, regionChoice)

for (k in sspChoices) {
  for (l in startYearChoices) {
    f_thi_cts_crop_graphing(k, l, stressValue, qChoice, waterSource, outChoice, regionChoice)
  }
}
