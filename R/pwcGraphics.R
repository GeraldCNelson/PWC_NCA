# create PWC figures and tables
# data creation is done in final_wbgt_clean.R
# Note to self: First crop to the extent of the region, then mask out the stuff you don't want
{
  library(terra)
  library(data.table)
  library(ggplot2)
  woptList <- list(gdal = c("COMPRESS = DEFLATE", "PREDICTOR = 2", "ZLEVEL = 6", "NUM_THREADS = ALL_CPUS", names = c("runStarts", "runLength")))
  terraOptions(ncopies = 1, progress = 10, tempdir = "data/ISIMIP", verbose = FALSE)
  source("R/basicVariables.R")
  source("R/basicSpatialVariables.R")
  # regionChoices -----. Note: these are all in lat long.
  regionChoices <- c("states_SW") 
  cropmasks_complete <- c("almond", "apple", "barley", "bean", "blueberry", "broadbean", "cassava", "cherry", "chickpea", "cotton", "cowpea", "grape", "groundnut", "lentil", "maize", "millet", "mustard", "oats", "pea", "potato", "rapeseed", "rice", "rye", "sorghum", "soybean", "sugarbeet", "sunflower", "sweetpotato", "tomato", "walnut", "wheat", "yam")
  cropmasks_perennial <- c("almond", "apple", "blueberry", "cherry", "grape")
  cropmasks_annual <- cropmasks_complete[!cropmasks_complete %in% cropmasks_perennial]
  cropChoices <- c( "mai", "ri1",  "wwh") #"mil", "cas", "bar", "nut", "sgc",
  waterSources <- c("ir", "rf")
  waterSources <- c("rf") # just look at rainfed locations
  
  breaks_percent <- c(0, 50, 75, 100)
  breaks_percent_annual_4 <- c(0, 50, 75, 100)
  breaks_percent_annual_5 <- c(0, 50, 65, 80, 100)
  breaks_cts <- c(0, 1, 25, 50, 75, 100)
  breaks_PWC <- c(0, 100, 200, 250, 300, 366)
  breaks_runLength <- c(0, 20, 50, 75, 100, 150, 200, 250, 366)
  testbreaks_8 <-   c(0, 50, 70, 75, 80, 85, 90, 95, 100)
  testbreaks <-   c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  colorList_disc_10_RdYlGn <- (RColorBrewer::brewer.pal(10, "RdYlGn")) # green to red
  colorList_disc_8_RdYlGn <- (RColorBrewer::brewer.pal(8, "RdYlGn")) # green to red, 8 colors
  # temp for testing
  colorList_disc_10_RdYlGn <- (RColorBrewer::brewer.pal(10, "RdYlBu")) # green to red
  caption_land_area <- "Land areas in white had no crops in early 21st century." #Rectangles show 1/2 degree grid."   
  caption_detailed <- "Half degree rectangles show early century harvested area (http://www.earthstat.org).\nGrowing season is from early 21st century  (Jägermeyr, et. al, 2021)."
  graphicType <- "pdf"
  defaultWidth <- 12 # changes the width for all graphs
  # used to turn the legend on or off
  legendSwitch <- TRUE
  
  cropMask_globe <- rast("data/crop_mask/rasterMask_noCrops_175.tif") # locations with no crops = 0
  # aggregate to 1/2 degree
  cropMask_globe <- aggregate(cropMask_globe, fact = 6, fun = "mean", na.rm = FALSE)
  minCt = .5 # at least .5 of the pixels in the aggregated pixel have at least one crop grown there
  cropMask_globe[cropMask_globe < minCt] <- 0 
  cropMask_globe[cropMask_globe >= minCt] <- 1 
  cropMask_globe[cropMask_globe == 0] <- NA 
  
  rasterMaskLoc <- "data/crop_mask/rasterMask_"
  # tifFileLoc <- "data-raw/crops/HarvestedAreaYield175Crops_Geotiff/GeoTiff/"
  # cropStats_global <- as.data.table(read.csv(paste0(tifFileLoc,"combined_global.csv")))
  # setnames(cropStats_global, old = "X", new = "cropName")
  outChoices <- c("pwc_wbgt_out") #,  "pwc_utci_out", "pwc_wbgt_out")
  # lookup table for the crop names in the masks and the abbreviations used in the growing season netcdfs
  # cropNameLookup <- as.data.table(readxl::read_excel("/Volumes/ExtremeSSD2/heatstress/data/crop_mask/cropMask_calendarLookup.xlsx"))
  
  # outChoice <- "pwc_wbgt_out" # choices are pwc_utci_out, pwc_wbgt_out, wbgt_out, utci_out, Tg_out
  
  # set colorlist choice here
  palName_PWC <- "YlOrRd"
  # palName_NCA <- "BuPu"
  palCt <- 9
  colorList <- f_palette(palName, palCt)
  
  runlengthChoices <- c(1) #, 5, 10
  runlength <- 1
  #stressValues <- c(40, 50, 60)
  stressValues <- c(50, 75)
  # the CIESIN data are now in the geodata package
  # pop <- rast("data/gpw_v4_population_count_rev11_30_min.nc") # source is CIESIN
  # # layer 4 is 2015
  # popCtGlobal <- pop[[4]]
  #The following line downloads the data the first time and can be a bit slow. After that it uses a downloaded version and is quick
  # popCtGlobal <- geodata::population(2015, 5, path = "data-raw/population/") # units are 10 minutes
  # Need to convert to 1/2 degree to get to cmip6 cell size
  # 5 min = 0.0833333 degree
  # 30 min = 0.5 degree
  # popCtGlobal <- aggregate(popCtGlobal, fact = 6, fun = "sum")
  
  # # new ISIMIP population data -----
  # fileName_in_ISIPop <- "data-raw/population/population_2015soc_30arcmin_annual_1901_2021.nc"
  # popCtGlobal_ISIMIP <- rast(fileName_in_ISIPop) # contains yearly population from 1901-2021, urban, rural, total
  # popCtGlobal_ISIMIP <- crop(popCtGlobal_ISIMIP, extent_noAntarctica)
  # popCtGlobal_total <- popCtGlobal_ISIMIP$`total-population_115`
  # popCtGlobal_rural <- popCtGlobal_ISIMIP$`rural-population_115`
  # popCtGlobal_urban <- popCtGlobal_ISIMIP$`urban-population_115`
  # names(popCtGlobal_total) <- "popTot2015"
  # names(popCtGlobal_rural) <- "popRural2015"
  # names(popCtGlobal_urban) <- "popUrban2015"
}

{
  #  test data
  l <- 2081
  k <- "ssp585"
  modelChoice <- "MRI-ESM2-0" 
  waterSource <- "rf"
  cropChoice <- "mai"
  regionChoice <- "states_SW"
  border <-states_SW
  stressValue <- 50
  
  f_periodFancyName <- function(yearSpan) {
    if (yearSpan == "1991_2010") yearSpanText <- "historical (1991-2010)"
    if (yearSpan == "2041_2060") yearSpanText <- "mid century"
    if (yearSpan == "2081_2100") yearSpanText <- "end century"
    return(yearSpanText)
  }
  
  f_regionExtentLookup <- function(regionChoice) { # all in lat/lon with xmin, xmax, ymin, ymax order
    #   browser()
    ext_globe <- ext(-180, 180, -60, 90)
    ext_midEast <- ext(31, 62.0, 10.5, 37)
    ext_SouthAsia <- ext(65, 89, 6, 35)
    ext_SEAsia <- ext(89, 148, -18, 26)
    ext_NamCentAm <- ext(-121, -68, 10, 34)
    #    ext_states_SW <- floor(ext(states_SW)) # use of floor is an attempt to expand the boundaries to the largest/smallest integer that includes all the relevant locations
    ext_states_SW <- align(ext(states_SW), rast(res = 0.5), "out")
  #  ext_EastAfrica <- ext(30, 45, -27, 3)
    #    align(e, rast(res = 0.5), "out")
    # ext_countries_Africa <- ext(countries_Africa)
    # ext_countries_WestAfrica <- align(ext(countries_WestAfrica), rast(res = 0.5), "out")
    # ext_GP_gadm <- floor(ext(GP_gadm))
    # ext_country_Kenya <- align(ext(country_Kenya), rast(res = 0.5), "out")
    # ext_country_India <- align(ext(country_India), rast(res = 0.5), "out")
    # ext_country_Brazil <- align(ext(country_Brazil), rast(res = 0.5), "out")
    # ext_countries_Africa <- align(ext(countries_Africa), rast(res = 0.5), "out")
    # ext_country_US <- align(ext(country_US), rast(res = 0.5), "out")
    # ext_countries_AfricaHorn <- align(ext(countries_AfricaHorn), rast(res = 0.5), "out")
    # if (regionChoice == "globe") return(ext_globe)
    # if (regionChoice == "South_Asia") return(ext_SouthAsia)
    # if (regionChoice == "SE_Asia") return(ext_SEAsia)
    # if (regionChoice == "East_Africa") return(ext_EastAfrica)
    # if (regionChoice == "Mideast") return(ext_midEast)
    # if (regionChoice == "NamCentAm") return(ext_NamCentAm)
     if (regionChoice == "states_SW") return(ext_states_SW) 
    # if (regionChoice == "countries_Africa") return(ext_countries_Africa)
    # if (regionChoice == "countries_WestAfrica") return(ext_countries_WestAfrica)
    # if (regionChoice == "GP_gadm") return(ext_GP_gadm)
    # if (regionChoice == "country_Kenya") return(ext_country_Kenya)
    # if (regionChoice == "country_India") return(ext_country_India)
    # if (regionChoice == "country_Brazil") return(ext_country_Brazil)
    # if (regionChoice == "country_US") return(ext_country_US)
    # if (regionChoice == "countries_AfricaHorn") return(ext_countries_AfricaHorn)
    # if no name
    print(paste0("No region choice like ", regionChoice, ". Check the list"))
  }
  
  f_regionFancyName <- function(regionChoice) {
    # if (regionChoice == "globe") return("global")
    # if (regionChoice == "South_Asia") return("South Asia")
    # if (regionChoice == "SE_Asia") return("SE Asia and Northern Australia")
    # if (regionChoice == "East_Africa") return("East Africa")
    # if (regionChoice == "Mideast") return("Middle East")
    # if (regionChoice == "NamCentAm") return("Southern US and Central America")
    if (regionChoice == "states_SW") return("Southwest US")
    # if (regionChoice == "countries_Africa") return("Africa")
    # if (regionChoice == "countries_WestAfrica") return("West Africa")
    # if (regionChoice == "GP_gadm") return("Ganges River Plain")
    # if (regionChoice == "country_Kenya") return("Kenya")
    # if (regionChoice == "country_India") return("India")
    # if (regionChoice == "country_Brazil") return("Brazil")
    # if (regionChoice == "country_US") return("United States")
    # if (regionChoice == "countries_AfricaHorn") return("Horn of Africa")
    
    # if no name
    print(paste0("No region name like ", regionChoice, ". Check the list"))
  }
  
  f_quarterFancyName <- function(cropName) {
    if (cropName == "JFM") return("January to March")
    if (cropName == "AMJ") return("April to June")
    if (cropName == "JAS") return("July to September")
    if (cropName == "OND") return("October to December")
    
    # if no name
    print(paste0("No quarter name like ", cropName, ". Check the list"))
  }
  
  f_stressMetricText <- function(outChoice) {
    if (outChoice %in% c("pwc_utci_out", "utci_out")) stressMetric <- "UTCI"
    if (outChoice %in% c("pwc_wbgt_out", "wbgt_out")) stressMetric <- "WBGT"
    return(stressMetric)
  }
  f_waterSourceText <- function(waterSource) {
    if (waterSource == "ir") waterSourceText <- "irrigated"
    if (waterSource == "rf") waterSourceText <- "rainfed"
    return(waterSourceText)
  }
  
  f_getStressValue <- function(stressLevel) {
    if (stressLevel == "extremeStress") stressValue <- 40
    if (stressLevel == "moderateStress") stressValue <- 50 
    if (stressLevel == "noStress") stressValue <- 60
    return(stressValue)
  }
  
  f_sspText <- function(k) {
    if (k == "historical") kText <- ""
    if (k == "ssp126") kText <- "SSP1-2.6"
    if( k == "ssp585") kText <- "SSP5-8.5"
    return(kText)
  }
  
  f_h <- function(regionExtent, defaultWidth) {
    x <- regionExtent[2] - regionExtent[1]
    y <- regionExtent[4] - regionExtent[3] 
    h <- defaultWidth/(x/y)
    return(h)
  }
  
  f_runs <- function(x, runlength, test_logic) {
    #  print(paste0("test_logic: ", test_logic))
    # runResult <- c(NA, NA)
    # if (is.nan(x[1][1])) {
    #   return(runResult)
    # }
    seqLengthCode <- paste0("1{", runlength, ",}") #A regular expression  to get the first item of gregexpr. It says look for  runlength times See http://xenon.stanford.edu/~xusch/regexp/
    g <- gregexpr(seqLengthCode, paste(+eval(parse(text = test_logic)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
    #  print(paste0("g1: ", g[1]))
    if ((g[1] == -1)) { # no need to write to growing season if g returns -1, return 0,0
      runResult <- c(0, 0) 
      #    print("no runs")
    } else {
      startDays <- unlist(g)
      runLengths <- sum(as.numeric(attributes(g)$match.length))
      print(paste0("runLengths: ", runLengths))
      runResult <- c(length(startDays), runLengths)
    }
    return(runResult)
  }
  
  f_gs_stressDays <- function(stress, plant_SH, plant_NH, stressValue) {
    runResult <- c(NA, NA) 
    if (is.nan(stress[1])) {
      return(runResult)
    }
    stress_subset <- stress[gs_start]:stress[gs_end]
    stressCt <- sum(stress_subset < stresslevel, na.rm = TRUE)
    return(stressCt)
  }
  
  f_runs_calculator <- function(k, l, runlengthChoice, stressValue, outChoice, regionChoice) {
    yearSpan <- paste0(l, "_", l + yearRange)
    regionExtent <- f_regionExtentLookup(regionChoice)
    yearSpanText <- f_periodFancyName(yearSpan)
    logicDirection <- "<"
    if (logicDirection == "<") ldtext <- "lt"
    test_logic <- paste0("x ", logicDirection, " ", stressValue)
    fileName_in <- paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif")
    r <- rast(fileName_in)
    r_cropped <- crop(r, regionExtent)
    fileName_out <- paste0(locOfDataFiles_pwc, regionChoice,"_", outChoice, "_ensemble_daily_mean_run_", runlength, "_lim_", logicDirection, stressValue, "_", k, "_", yearSpan,  ".tif")
    print(system.time(r_cropped_runs <- app(r_cropped, f_runs, runlength, test_logic, filename = fileName_out,  overwrite = TRUE, wopt = woptList))) #names = c("runStarts","runLength"),
    stressMetric <- f_stressMetricText(outChoice)
    print(fileName_out)
    if (k == "historical") {
      kcleanup <- " "
    } else {kcleanup <- f_sspText(k)
    }
    mainrl <- paste0("PWC ", logicDirection, " ", stressValue, ", \nlongest num. of days in a min. run of ", runlength, " days, ", kcleanup, ", ", yearSpanText)
    # mainct <- paste0("PWC (7 hour, based on  ", stressMetric, ") ", logicDirection, " ", stressValue, "\nrun minimum length is ", runlength, " days, ", kcleanup, yearSpanText)
    mainct <- paste0("PWC ", logicDirection, " ", stressValue, ",\nrun minimum length is ", runlength, " days, ", kcleanup, ", ", yearSpanText)
    r_cropped_runs_cts <- r_cropped_runs$lyr.1
    r_cropped_runs_rl <- r_cropped_runs$lyr.2
    plot(r_cropped_runs_cts, main = mainct)
    plot(r_cropped_runs_rl, main = mainrl)
  }
  
  f_getsum <- function(r) {
    totalNum_r <- as.numeric(global(r, fun = "sum", na.rm = TRUE)[1])/1000000
    totalNum_r <- round(totalNum_r, 1)
  }
  
  f_getArea <- function(r, layer) {
    r_sub <- subset(r, layer)
    r_sub[r_sub > 1] <- 1 # anyplace that has a positive value in r_sub becomes 1
    r_sub[r_sub < 1 | r_sub > 1] <- NA # keep only locations with value of 1
    
    r_sub_area <- (expanse(r_sub, unit = "km"))/1000 # convert to 1000 sq km
    r_sub_area <- round(r_sub_area, 0)
  }
  
  f_dt_stressCts <- function(k, l, r_runs_cts_mask, runlength, regionChoice, logicDirection, stressValue, outChoice, popCtGlobal_urban, popCtGlobal_rural) {
    # returns counts of people in locations where there are stress days
    yearSpan <- paste0(l, "_", l + yearRange)
    regionExtent <- f_regionExtentLookup(regionChoice)
    cropMask_region <- crop(cropMask_globe, regionExtent)
    
    r_pop_region_tot <- crop(popCtGlobal_total, regionExtent, snap = "near")
    r_pop_region_urban <- crop(popCtGlobal_urban, regionExtent, snap = "near")
    r_pop_region_rural <- crop(popCtGlobal_rural, regionExtent, snap = "near")
    
    totalNum_region_tot <- f_getsum(r_pop_region_tot)
    totalNum_region_urban <- f_getsum(r_pop_region_urban)
    totalNum_region_rural <- f_getsum(r_pop_region_rural)
    
    r_pop_region_urban_ag <- mask(r_pop_region_urban, cropMask_region, maskvalues = NA) 
    totalNum_region_urban_ag <- f_getsum(r_pop_region_urban_ag)
    
    r_pop_region_rural_ag <- mask(r_pop_region_rural, cropMask_region, maskvalues = NA) 
    totalNum_region_rural_ag <- f_getsum(r_pop_region_rural_ag)
    
    stresslocs_region <- crop(r_runs_cts_mask, regionExtent)
    stresslocs_region[stresslocs_region < runlength] <- NA 
    stresslocs_region[stresslocs_region >= runlength] <- 1 #locations where there is at least one run as long as the minimum run
    
    stresslocs_region_ag <- mask(stresslocs_region, cropMask_region, maskvalues = NA)
    stresslocs_region[stresslocs_region < runlength] <- NA 
    stresslocs_region[stresslocs_region >= runlength] <- 1 #locations where there is at least one run as long as the minimum run
    
    r_pop_region_stressed_rural <- mask(r_pop_region_rural, stresslocs_region, maskvalues = NA)
    r_pop_region_stressed_urban <- mask(r_pop_region_urban, stresslocs_region, maskvalues = NA)
    totalNum_region_stressed_rural <- f_getsum(r_pop_region_stressed_rural)
    totalNum_region_stressed_urban <- f_getsum(r_pop_region_stressed_urban)
    
    # regions with crops with PWC value less than stressValue
    r_pop_region_stressed_rural_ag <- mask(r_pop_region_rural_ag, stresslocs_region_ag, maskvalues = NA)
    r_pop_region_stressed_urban_ag <- mask(r_pop_region_urban_ag, stresslocs_region_ag, maskvalues = NA)
    totalNum_region_stressed_rural_ag <- f_getsum(r_pop_region_stressed_rural_ag)
    totalNum_region_stressed_urban_ag <- f_getsum(r_pop_region_stressed_urban_ag)
    
    # r_pop_region_stressed_urban <- mask(r_pop_region_urban, stresslocs, maskvalues = NA) 
    # r_pop_region_stressed_rural <- mask(r_pop_region_rural, stresslocs, maskvalues = NA)
    # r_pop_region_stressed_urban_ag <- mask(r_pop_region_stressed_urban, cropMask_region, maskvalues = NA) 
    # r_pop_region_stressed_rural_ag <- mask(r_pop_region_stressed_rural, cropMask_region, maskvalues = NA)
    # 
    # totalNum_stressed_urban <- f_getsum(r_pop_region_stressed_urban)
    # totalNum_stressed_urban_ag <- f_getsum(r_pop_region_stressed_urban_ag)
    # 
    
    # get stressed area, in 1000 sq km
    stressed_area_crop_rural_ag <- f_getArea(r_pop_region_stressed_rural_ag, 1)
    stressed_area_crop_urban_ag <- f_getArea(r_pop_region_stressed_urban_ag, 1)
    area_crop_rural_ag <- f_getArea(r_pop_region_rural_ag, 1)
    area_crop_urban_ag <- f_getArea(r_pop_region_urban_ag, 1)
    regionFancyName <- f_regionFancyName(regionChoice) 
    cts_urban <- list(outChoice, k, l, runlength, stressValue, "urban", totalNum_region_stressed_urban, totalNum_region_stressed_urban_ag, totalNum_region_urban_ag, totalNum_region_urban, stressed_area_crop_urban_ag,  area_crop_urban_ag, regionFancyName)
    cts_rural <- list(outChoice, k, l, runlength, stressValue, "rural", totalNum_region_stressed_rural, totalNum_region_stressed_rural_ag, totalNum_region_rural_ag, totalNum_region_rural, stressed_area_crop_rural_ag, area_crop_rural_ag, regionFancyName)
    cts <- list(cts_urban, cts_rural)
    return(cts)
  } 
  
  f_dt_stressCts_crop <- function(k, l, regionChoice, runlength, logicDirection, stressValue, outChoice, popCtGlobal_urban, popCtGlobal_rural, cropChoice) {
    # returns counts of people in locations in a region where a crop is grown and there are stress days
    border <- get(regionChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    regionExtent <- f_regionExtentLookup(regionChoice)
    cropMask_region <- crop(cropMask_globe, regionExtent)
    
    r_pop_region_tot <- crop(popCtGlobal_total, regionExtent, snap = "near")
    r_pop_region_urban <- crop(popCtGlobal_urban, regionExtent, snap = "near")
    r_pop_region_rural <- crop(popCtGlobal_rural, regionExtent, snap = "near")
    
    totalNum_region_tot <- f_getsum(r_pop_region_tot)
    totalNum_region_urban <- f_getsum(r_pop_region_urban)
    totalNum_region_rural <- f_getsum(r_pop_region_rural)
    
    fileName_in <- paste0(locOfDataFiles_pwc, regionChoice, "_", outChoice, "_ensemble_daily_mean_run_", runlength, "_lim_", logicDirection, stressValue, "_", k, "_", yearSpan, ".tif")
    r <- rast(fileName_in)
    r_stressDays <- r[[2]] # locations with stress days, number of days
    r_stressDays_region <- crop(r_stressDays, regionExtent)
    
    # work on crop-specific stuff
    cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
    crop_loc_globe <- rast(paste0(rasterMaskLoc, cropName, ".tif")) # 1/0 for location of crop area, globally
    crop_loc_region <- crop(crop_loc_globe, regionExtent)
    crop_area_region <- round(as.numeric(global(crop_loc_region, fun = "sum", na.rm = TRUE))/1000, 1)
    
    r_pop_region_tot_croploc <- mask(r_pop_region_tot, crop_loc_region, maskvalues = 0); names(r_pop_region_tot_croploc) <- "popTotCroploc"
    r_pop_region_rural_croploc <- mask(r_pop_region_rural, crop_loc_region, maskvalues = 0); names(r_pop_region_rural_croploc) <- "popRuralCroploc"
    r_pop_region_urban_croploc <- mask(r_pop_region_urban, crop_loc_region, maskvalues = 0); names(r_pop_region_urban_croploc) <- "popUrbanCroploc"
    
    totalNum_region_tot_croploc <- round(as.numeric(global(r_pop_region_tot_croploc, fun = "sum", na.rm = TRUE)[1])/1000000, 1)
    totalNum_region_rural_croploc <- round(as.numeric(global(r_pop_region_rural_croploc, fun = "sum", na.rm = TRUE)[1])/1000000, 1)
    totalNum_region_urban_croploc <- round(as.numeric(global(r_pop_region_urban_croploc, fun = "sum", na.rm = TRUE)[1])/1000000, 1)
    
    
    
    r_stressDays_region_ag <- mask(r_stressDays_region, cropMask_region, maskvalues = NA) # locations in the region where the crop is grown and stress days are positive, globally
    crop_area_region_stressed <- round(as.numeric(global(r_stressDays_region_cropMask, fun = "sum", na.rm = TRUE))/1000, 1)
    crop_area_region_aveRunLength <- round(as.numeric(global(r_stressDays_region_cropMask, fun = "mean", na.rm = TRUE)), 1)
    
    # pop counts for locations where stress is positive
    r_pop_region_stressed_tot <- mask(r_pop_region_tot_croploc, r_stressDays_region_cropMask, maskvalues = 0); names(r_pop_region_stressed_tot) <- "popTotStressedCropLoc"
    r_pop_region_stressed_rural <- mask(r_pop_region_rural_croploc, r_stressDays_region_cropMask, maskvalues = 0); names(r_pop_region_stressed_rural) <- "popRuralStressedCropLoc"
    r_pop_region_stressed_urban <- mask(r_pop_region_urban_croploc, r_stressDays_region_cropMask, maskvalues = 0); names(r_pop_region_stressed_urban) <- "popUrbanStressedCropLoc"
    
    totalNum_stressed_tot <- round(as.numeric(global(r_pop_region_stressed_tot, fun = "sum", na.rm = TRUE)[1])/1000000, 1)
    totalNum_stressed_rural <- round(as.numeric(global(r_pop_region_stressed_rural, fun = "sum", na.rm = TRUE)[1])/1000000, 1)
    totalNum_stressed_urban <- round(as.numeric(global(r_pop_region_stressed_urban, fun = "sum", na.rm = TRUE)[1])/1000000, 1)
    
    # totalNum <- as.numeric(global(popCtGlobal, fun = "sum", na.rm = TRUE)[1])/1000000
    # totalNum <- round(totalNum, 1)
    cts <- list(outChoice, regionFancyName, cropName, k, l, runlength, crop_area_region_aveRunLength, stressValue, 
                crop_area_region, crop_area_region_stressed, 
                totalNum_region_tot_croploc, totalNum_region_rural_croploc, totalNum_region_urban_croploc,
                totalNum_stressed_tot, totalNum_stressed_rural, totalNum_stressed_urban)
    return(cts)
  }
  
  #   f_thi_cts_crop_graphing <- function(k, l, stressValue, cropChoice, waterSource, outChoice, regionChoice, breaks) {
  #     #  two things going on in here
  #     # count the number of days in the gsl that are less than a stress value cutoff
  #     # calculate the ratio of stressed days to total days in the gsl
  #     
  #     # stress days in gsl -----
  #     yearSpan <- paste0(l, "_", l + yearRange)
  #     regionExtent <- f_regionExtentLookup(regionChoice)
  #     cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
  #     yearSpanText <- f_periodFancyName(yearSpan)
  #     sspText <- f_sspText(k)
  #     regionFancyName <- f_regionFancyName(regionChoice) 
  #     waterSourceText <- f_waterSourceText(waterSource)
  #     
  #     border <- get(regionChoice)
  #     crop_loc_globe <- rast(paste0(rasterMaskLoc, cropName, ".tif")) # 1/0 for location of crop area, globally
  #     if (cropChoice %in% c("q1", "q2", "q3", "q4")) {
  #       crop_loc_globe <- rast(paste0(rasterMaskLoc, cropChoice, ".tif")) # dummy to say all locations are included in the crop mask
  #     } else {
  #       crop_loc_globe <- rast(paste0(rasterMaskLoc, cropName, ".tif")) # 1/0 for location of crop area, globally
  #     }
  #     crop_loc_region <- any(crop(crop_loc_globe, regionExtent))
  #     crop_loc_region[crop_loc_region == 0] <- NA
  #     PWCvals <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
  #     PWCvals_cropped <- crop(PWCvals, regionExtent)
  #     PWCvals_cropped <- mask(PWCvals_cropped, crop_loc_region)
  #     PWCvals_cropped <- mask(PWCvals_cropped, border, inverse = FALSE, touches = FALSE) # remove coastal pixels
  #     
  #     stress <- PWCvals_cropped < stressValue # daily locations where PWC is less than the stress cutoff, TRUE/FALSE
  #     if (cropChoice %in% c("q1", "q2", "q3", "q4")) {
  #       dayCt_quarters <- c(90, 91, 92, 92) # days in each nonleap year quarter - https://www.google.com/search?client = safari&rls = en&q = number+of+days+in+a+quarter&ie = UTF-8&oe = UTF-8
  #       qnum <- as.numeric(gsub("q", "", cropChoice))
  #       maturity_days <- cumsum(dayCt_quarters)
  #       plant_days <- maturity_days - dayCt_quarters + 1
  #       gsl <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = dayCt_quarters[qnum], ext = regionExtent)
  #       plant <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = plant_days[qnum], ext = regionExtent)
  #       maturity <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = maturity_days[qnum], ext = regionExtent)
  #     } else {
  #       calendar_in <- paste0("data-raw/crops/cropCalendars_ggcmi/", cropChoice, "_", waterSource, "_ggcmi_crop_calendar_phase3_v1.01.nc4")
  #       temp <- rast(calendar_in)
  #       temp <- crop(temp, regionExtent)
  #       plant <- temp$planting_day
  #       maturity <- temp$maturity_day
  #       gsl <- temp$growing_season_length
  #     }
  #     # r_stress - days in the cropped area between planting and maturity  where the stress value is met or exceeded
  #     fileName_out_stress_days <- paste0(locOfDataFiles_pwc, "crops/", outChoice, "/", regionChoice, "_", cropChoice, "_", waterSource, "_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".tif")
  #     
  #     print(system.time(r_stress <- rapp(stress, plant, maturity, sum, na.rm = TRUE, circular = TRUE, filename = fileName_out_stress_days, overwrite = TRUE, wopt = woptList))) 
  #     names(r_stress) <- "stressCts"
  # #    stress_max <- as.numeric(global(r_stress, max, na.rm = TRUE)) + 5 # adding 5 bumps the maximum range which deals with some of the seq issues for some locations
  #     gsl_max <- as.numeric(global(gsl, max, na.rm = TRUE)) + 1 # adding 5 bumps the maximum range
  #     
  #     breaks <- ceiling(seq(from = 0, to = gsl_max, length.out = 9)) # 9 is the value when doing 8 colors
  #     # prepare for graphing with proj_to_use
  #     r_stress_p <- as.polygons(r_stress, dissolve = TRUE, na.rm = TRUE)
  #     r_stress_p_proj <- project(r_stress_p, proj_to_use)
  #     r_stress_p_proj_sf <- sf::st_as_sf(r_stress_p_proj)
  #     r_stress_p_proj_sf$stressCts_disc <- cut(r_stress_p_proj_sf$stressCts, breaks = breaks, include.lowest = TRUE, right = TRUE)
  #     r_stress_p_proj_sf$stressCts_disc_f <- as.factor(r_stress_p_proj_sf$stressCts_disc)
  #     
  #     border_sf <- sf::st_as_sf(border)
  #     border_proj_sf <- sf::st_transform(border_sf, proj_to_use)
  #     
  #     legendTitle <- paste0("Days with PWC below ", stressValue, " (%)")
  #     waterSourceText_upper <- stringr::str_to_title(waterSourceText)
  #     titleText_start <- paste0(waterSourceText_upper, " " , cropName, ", growing season days with PWC below ", stressValue, " percent, ")
  #     titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
  #     if (cropChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
  #       titleText <- paste0("Days with PWC below ", stressValue, " percent ", f_quarterFancyName(cropName), ", ",  sspText, ", ", yearSpanText, ", ", regionFancyName)
  #       caption <- caption_land_area #Rectangles show 1/2 degree grid."   
  #     }
  #     if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
  #     
  #      bigGroups <- c("globe", "countries_Africa", "country_India", "country_Brazil", "country_US")
  #     gridLineSize <- 0.1; borderLineSize <- 0.2
  #     if (regionChoice %in% bigGroups) {
  #       gridLineSize <- 0.01; borderLineSize <- 0.05
  #     }
  #     colorValues <- rev(colorList_disc_8_RdYlGn)
  #     caption <-  caption_land_area 
  #     
  #     g <- ggplot(r_stress_p_proj_sf, aes(fill = stressCts_disc_f)) +
  #       scale_fill_manual(values = colorValues, drop = FALSE) +
  #       geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
  #       labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + #
  #       theme_bw() +
  #       theme(
  #         legend.text.align = 1,
  #         axis.ticks = element_blank(),
  #         plot.title = element_text(size = 12, hjust = 0.5),
  #         plot.caption = element_text(hjust = 0, vjust = 3.0, size = 8)
  #       ) +
  #       theme(legend.position = 'bottom') +
  #       geom_sf(data = border_proj_sf, 
  #               color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
  #       theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
  #     print(g)
  #     fileName_out_stressDays_count <- paste0("graphics/cts/stressDaysbyCrop/", outChoice, "/stressDaysCtGS_", regionChoice, "_", cropChoice, "_", waterSource, "_cts_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".", graphicType) 
  #     h <- f_h(regionExtent, defaultWidth)
  #     ggsave(filename = fileName_out_stressDays_count, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300)
  #     system2('pdfcrop', c(fileName_out_stressDays_count, fileName_out_stressDays_count)) # gets rid of white space around the figure in the pdf
  #     print(paste0("fileName out: ", fileName_out_stressDays_count))
  #     
  #     # share of stress days in gsl days -------------------------
  #     gsl_ratio <- 100 * r_stress/gsl
  #     gsl_ratio[gsl_ratio > 100] <- 100 # to deal with a few rare cases
  #     names(gsl_ratio) <- "stressShare"
  #     # next 4 lines not needed because r_stress already incorporates them
  #     # gsl_ratio <- mask(gsl_ratio, border)
  #     # gsl_ratio <- mask(gsl_ratio, border, inverse = FALSE, touches = FALSE) # note that it uses coastline to remove coastal pixels.
  #     # cropMask_region <- crop(cropMask_globe, regionExtent)
  #     # gsl_ratio <- mask(gsl_ratio, cropMask_region, maskvalues = NA)
  #     
  #     # next few lines generate breaks for graphing. Also to generate a table of gsl_ratios_values. No longer used June 11, 2022
  #     # require(tidyr)
  #     # require(dplyr)
  #     # values_gsl_ratio <- values(gsl_ratio, dataframe = TRUE, na.rm = TRUE)
  #     gsl_ratio_p <- as.polygons(gsl_ratio, dissolve = TRUE, na.rm = TRUE)
  #     gsl_ratio_p_proj <- project(gsl_ratio_p, proj_to_use)
  #     gsl_ratio_p_proj_sf <- sf::st_as_sf(gsl_ratio_p_proj)
  #     
  #     breaks <- testbreaks
  #     gsl_ratio_p_proj_sf$stressShare_disc <- cut(gsl_ratio_p_proj_sf$stressShare, breaks = breaks, include.lowest = TRUE, right = TRUE)
  #     gsl_ratio_p_proj_sf$stressShare_disc_f <- as.factor(gsl_ratio_p_proj_sf$stressShare_disc)
  #     legendTitle <- paste0("Period share (%), days with PWC below ", stressValue, "%")
  #     caption <- caption_detailed
  #     titleText_start <- paste0(waterSourceText_upper, " " , cropName, ", share of days in period with PWC below ", stressValue, " percent, ")
  #     titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
  #     if (cropChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
  #       titleText_start <- paste0(f_quarterFancyName(cropName), ", share of days in period with PWC below ", stressValue, " percent, ")
  #       titleText <- paste0(titleText_start, sspText, ", ", yearSpanText, ", ", regionFancyName)
  #     }
  #     if (k == "historical" ) titleText <- gsub(", ,", "", titleText, fixed = TRUE)
  #     caption <- caption_land_area
  #     
  #     colorValues <- rev(colorList_disc_8_RdYlGn)
  #     g <- ggplot(gsl_ratio_p_proj_sf, aes(fill = stressShare_disc_f)) +
  #       scale_fill_manual(values = colorValues, drop = FALSE) +
  #       geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
  #       labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
  #       theme_bw() +
  #       theme(
  #         legend.text.align = 1,
  #         # axis.text.x = element_blank(),
  #         # axis.text.y = element_blank(),
  #         axis.ticks = element_blank(),
  #         plot.title = element_text(size = 12, hjust = 0.5),
  #         plot.caption = element_text(hjust = 0, vjust = 3.0, size = 8),
  #         legend.margin = margin(-20, 0, 0, 0)
  #       ) +
  #       theme(legend.position = 'bottom') +
  #       geom_sf(data = border_proj_sf, 
  #               color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
  #       theme(legend.margin = margin(-20, 0, 0, 0)) +
  #       theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
  #     print(g)
  #     
  #     h <- f_h(regionExtent, defaultWidth)
  #     fileName_stressDaysRatio_out <- paste0("graphics/cts/stressDaysRatiobyCrop/", outChoice, "/stressDaysGSShare_",  regionChoice, "_", cropChoice, "_", waterSource, "_", outChoice, "_daysCts_", runlength,  "_stressValue_", stressValue, "_", k, "_", yearSpan, ".", graphicType)
  #     ggsave(filename = fileName_stressDaysRatio_out, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300) 
  #     system2('pdfcrop', c(fileName_stressDaysRatio_out, fileName_stressDaysRatio_out)) # gets rid of white space around the figure in the pdf
  #     print(paste0("fileName out: ", fileName_stressDaysRatio_out))
  #   }
  
  f_thi_runs_graphing <- function(k, l, r_runs_cts_mask, runlength, r_runs_length_mask, regionChoice, stressValue, outChoice) {
    # browser()
    yearSpan <- paste0(l, "_", l + yearRange)
    regionFancyName <- f_regionFancyName(regionChoice) 
    regionExtent <- f_regionExtentLookup(regionChoice)
    border <- get(regionChoice)
    border_sf <- sf::st_as_sf(border)
    border_proj_sf <- sf::st_transform(border_sf, proj_to_use)
    r_pop_region_urban <- crop(popCtGlobal_urban, regionExtent, snap = "near") # number of urban people in each 1/2 degree pixel
    r_pop_region_rural <- crop(popCtGlobal_rural, regionExtent, snap = "near")  # number of rural people in each 1/2 degree pixel
    r_pop_region_tot <- crop(popCtGlobal_total, regionExtent, snap = "near")
    
    bigGroups <- c("globe", "countries_Africa", "country_India", "country_Brazil", "country_US")
    gridLineSize <- 0.1; borderLineSize <- 0.2
    if (regionChoice %in% bigGroups) {
      gridLineSize <- 0.01; borderLineSize <- 0.05
    }
    # browser()
    
    cropMask_region <- crop(cropMask_globe, regionExtent)
    r <- mask(r_runs_cts_mask, cropMask_region, maskvalues = NA) # mask regions with no crops
    r_runsCt <- r$runsCt
    r_runsCt_p <- as.polygons(r_runsCt, dissolve = TRUE, na.rm = TRUE)
    r_runsCt_p_proj <- project(r_runsCt_p, proj_to_use)
    r_runsCt_p_proj_sf <- sf::st_as_sf(r_runsCt_p_proj)
    
    r_runsCt_p_proj_sf <- sf::st_intersection(r_runsCt_p_proj_sf, border_proj_sf) 
    r_runsCt_p_proj_sf$runsCt_disc <- cut(r_runsCt_p_proj_sf$runsCt, breaks = breaks_cts, 
                                          include.lowest = TRUE, right = TRUE)
    r_runsCt_p_proj_sf$runsCt_disc_f <- as.factor(r_runsCt_p_proj_sf$runsCt_disc)
    
    r <- mask(r_runs_length_mask, cropMask_region, maskvalues = NA) # mask regions with no crops
    r_runLength <- r$runLength
    r_runLength_p <- as.polygons(r_runLength, dissolve = TRUE, na.rm = TRUE)
    r_runLength_p_proj <- project(r_runLength_p, proj_to_use)
    r_runLength_p_proj_sf <- sf::st_as_sf(r_runLength_p_proj)
    r_runLength_p_proj_sf <- sf::st_intersection(r_runLength_p_proj_sf, border_proj_sf)
    r_runLength_p_proj_sf$runLength_disc <- cut(r_runLength_p_proj_sf$runLength, breaks = breaks_runLength, 
                                                include.lowest = TRUE, right = TRUE)
    r_runLength_p_proj_sf$runLength_disc_f <- as.factor(r_runLength_p_proj_sf$runLength_disc)
    
    # graphing for r_runsCt -----
    legendTitle <- "No. of runs"
    #  browser()
    sspText <- f_sspText(k)
    regionFancyName <- f_regionFancyName(regionChoice) 
    yearSpanText <- f_periodFancyName(yearSpan)
    
    titleText <- paste0("Continuous period with PWC below ", stressValue, " percent, " , yearSpanText, ", ", regionFancyName)
    if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
    caption <- caption_land_area
    g <- ggplot(r_runsCt_p_proj_sf, aes(fill = runsCt_disc_f)) +
      scale_fill_manual(values = rev(colorList_disc_5_RdYlGn), drop = FALSE) +
      geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
      theme_bw() +
      theme(
        legend.text.align = 1,
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = c(0.5, -.1),
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8)# 
      ) +
      geom_sf(data = sf::st_as_sf(border_proj_sf), color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
    print(g)
    fileName_out <- paste0("graphics/cts/", outChoice,"/", regionChoice, "_", outChoice, "_runsCt_minrunlength_", runlength,  "_stressValue_", stressValue, "_", k, "_", yearSpan, ".",  graphicType)
    h <- f_h(regionExtent, defaultWidth)
    ggsave(filename = fileName_out, plot = g, device = graphicType, width = defaultWidth, , height = h, units = "in", dpi = 300) 
    system2('pdfcrop', c(fileName_out, fileName_out)) # gets rid of white space around the figure in the pdf
    print(paste0("fileName out: ", fileName_out))
    
    # graphing for r_runLength -----
    legendTitle <- "Days"
    sspText <- f_sspText(k)
    titleText <- paste0("Continuous periods with PWC below ", stressValue, " percent, ", sspText, ", ", yearSpanText, ", ", regionFancyName)
    if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
    caption <- caption_land_area
    
    g <- ggplot(r_runLength_p_proj_sf, aes(fill = runLength_disc_f)) +
      scale_fill_manual(values = rev(colorList_disc_8_RdYlGn), drop = FALSE) +
      geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + #
      theme_bw() +
      theme(
        legend.text.align = 1,
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(hjust = 0,  vjust = -4, size = 8),
        legend.position = c(0.5, -.1),
        legend.direction = "horizontal"
      ) +
      geom_sf(data = sf::st_as_sf(border_proj_sf), color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
    print(g)
    fileName_out <- paste0("graphics/runlengths/", outChoice,"/", regionChoice, "_", outChoice, "_runLength", runlength,  "_stressValue_", stressValue, "_", k, "_", yearSpan, ".", graphicType)
    h <- f_h(regionExtent, defaultWidth)
    ggsave(filename = fileName_out, plot = g, device = graphicType, width = defaultWidth, units = "in", dpi = 300)
    system2('pdfcrop', c(fileName_out, fileName_out)) # gets rid of white space around the figure in the pdf
    print(paste0("fileName out: ", fileName_out))
    return(dt_stressCts)
  }
  
  f_stressCtsnGraphics <- function(k, l, runlength, stressValue, outChoice, popCtGlobal_urban, popCtGlobal_rural, regionChoice) {
    regionExtent <- f_regionExtentLookup(regionChoice)
    border <- get(regionChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    regionFancyName <- f_regionFancyName(regionChoice) 
    print(paste0("k: ", k, " l: ", l))
    logicDirection <- "<" 
    ldtext <- "lt"
    test_logic <- paste0("x ", logicDirection, " ", stressValue)
    fileName_in <- paste0(locOfDataFiles_pwc, regionChoice, "_", outChoice, "_ensemble_daily_mean_run_", runlength, "_lim_", logicDirection, stressValue, "_", k, "_", yearSpan, ".tif")
    print(paste0("fileName in in f_stressCtsnGraphics: ", fileName_in))
    r <- rast(fileName_in)
    names(r) <- c("runsCt", "runLength")
    r <- crop(r, regionExtent) 
    r <- mask(r, get(regionChoice), inverse = FALSE, touches = FALSE)
    
    r_runs_cts_mask <- r[[1]]
    r_runs_length_mask <- r[[2]]
    cts <- f_dt_stressCts(k, l, r_runs_cts_mask, runlength, regionChoice, logicDirection, stressValue, outChoice, popCtGlobal_urban, popCtGlobal_rural) # cts includes urban and rural in a list; no graphs
    f_thi_runs_graphing(k, l, r_runs_cts_mask, runlength, r_runs_length_mask, regionChoice, stressValue, outChoice)
    print("-------------------------")
    return(cts)
  }
  
  f_thi_cts_crop_graphing <- function(k, l, stressValue, cropChoice, waterSource, outChoice, regionChoice, breaks) {
    #  two things going on in here
    # count the number of days in the gsl that are less than a stress value cutoff
    # calculate the ratio of stressed days to total days in the gsl
    # graph each of these
    #    breaks <- breaks_runLength
    yearSpan <- paste0(l, "_", l + yearRange)
    regionExtent <- f_regionExtentLookup(regionChoice)
    cropMask_region <- crop(cropMask_globe, regionExtent) # 1-0, locations 
    cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
    
    border <- get(regionChoice)
    border <- crop(border, regionExtent)
    border_sf <- sf::st_as_sf(border)
    border_proj_sf <- sf::st_transform(border_sf, proj_to_use)
    
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) {
      #     crop_loc_globe <- rast(paste0(rasterMaskLoc, cropChoice, ".tif")) # dummy to say all locations are included in the crop mask
      crop_loc_globe <- cropMask_region
    } else {
      crop_loc_globe <- rast(paste0(rasterMaskLoc, cropName, ".tif")) # 1/0 for location of crop area, globally
    }
    crop_loc_region <- any(crop(crop_loc_globe, regionExtent))
    crop_loc_region[crop_loc_region == 0] <- NA
    if (is.na(minmax(crop_loc_region)[1])) stop(paste0("crop area of ", cropName, " in region ", regionChoice, " is zero."))
    # convert crop_loc_region to polygons and to sf
    crop_loc_region_p <- as.polygons(crop_loc_region, dissolve = TRUE, na.rm = TRUE)
    crop_loc_region_p_proj <- project(crop_loc_region_p, proj_to_use)
    crop_loc_region_p_proj_sf <- sf::st_as_sf(crop_loc_region_p_proj)
    
    filename_PWC_daily_mean_in <- paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif")
    PWCvals <- rast(filename_PWC_daily_mean_in)
    PWCvals_region <- crop(PWCvals, regionExtent)
    PWCvals_region_masked <- mask(PWCvals_region, cropMask_region, maskvalues = NA)
    # PWCvals_region_masked <- mask(PWCvals_region, border)
    PWCvals_region_masked <- mask(PWCvals_region_masked, coastline, inverse = TRUE, touches = FALSE) #  removes coastal pixels ------
    
    stress <- 1 * (PWCvals_region_masked < stressValue) # daily locations where PWC is less than the stress cutoff, TRUE/FALSE
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) {
      dayCt_quarters <- c(90, 91, 92, 92) # days in each nonleap year quarter - https://www.google.com/search?client = safari&rls = en&q = number+of+days+in+a+quarter&ie = UTF-8&oe = UTF-8
      qnum <- as.numeric(gsub("q", "", cropChoice))
      maturity_days <- cumsum(dayCt_quarters)
      plant_days <- maturity_days - dayCt_quarters + 1
      gsl <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = dayCt_quarters[qnum], ext = regionExtent)
      plant <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = plant_days[qnum], ext = regionExtent)
      maturity <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = maturity_days[qnum], ext = regionExtent)
    } else {
      calendar_in <- paste0("data-raw/crops/cropCalendars_ggcmi/", cropChoice, "_", waterSource, "_ggcmi_crop_calendar_phase3_v1.01.nc4")
      temp <- rast(calendar_in)
      temp <- crop(temp, regionExtent)
      plant <- temp$planting_day
      maturity <- temp$maturity_day
      gsl <- temp$growing_season_length
    }
    gsl_region <- crop(gsl, regionExtent)
    #   gsl_region_masked <- mask(gsl_region_masked, border)
    gsl_region_masked <- mask(gsl_region, cropMask_region, maskvalues = NA)
    gsl_region_masked <- mask(gsl_region_masked, coastline, inverse = TRUE, touches = FALSE) #  removes coastal pixels ------
    
    # r_stress - days in the cropped area between planting and maturity  where the stress value is met or exceeded
    fileName_out_stress_days <- paste0(locOfDataFiles_pwc, "crops/", outChoice, "/", regionChoice, "_", cropChoice, "_", waterSource, "_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".tif")
    
    print(system.time(r_stress <- rapp(stress, plant, maturity, sum, na.rm = TRUE, circular = TRUE, filename = fileName_out_stress_days, overwrite = TRUE, wopt = woptList))) 
    names(r_stress) <- "stressCts"
    stress_max <- as.numeric(global(r_stress, max, na.rm = TRUE)) +5 # adding 5 bumps the maximum range which deals with some of the seq issues for some locations
    gsl_max <- as.numeric(global(gsl_region_masked, max, na.rm = TRUE)) + 1 # adding 1 bumps the maximum range
    breaks <- ceiling(seq(from = 0, to = gsl_max, length.out = 9)) # 9 is the value when doing 8 colors
    
    
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
    titleText_start <- paste0(waterSourceText_upper, " " , cropName, ", growing season days with PWC below ", stressValue, " percent, ")
    titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
      titleText <- paste0("Days with PWC below ", stressValue, " percent, ", f_quarterFancyName(cropName), ", ",  sspText, ", ", yearSpanText, ", ", regionFancyName)
      caption <- caption_land_area #Rectangles show 1/2 degree grid."   
    }
    if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
    
    bigGroups <- c("globe", "countries_Africa", "country_India", "country_Brazil", "country_US")
    gridLineSize <- 0.1; borderLineSize <- 0.2
    if (regionChoice %in% bigGroups) {
      gridLineSize <- 0.005; borderLineSize <- 0.05
    }
    colorValues <- colorList_disc_8_RdYlGn
    caption <-  caption_land_area 
    
    
    g <- ggplot(r_stress_p_proj_sf, aes(fill = stressCts_disc_f)) +
      scale_fill_manual(values = rev(colorValues), drop = FALSE) +
      geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + #
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
      #    theme(legend.margin = margin(-20, 0, 0, 0)) +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
    print(g)
    fileName_out_stressDays_count <- paste0("graphics/cts/stressDaysbyCrop/", outChoice, "/stressDaysCtGS_", regionChoice, "_", cropChoice, "_", waterSource, "_cts_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".", graphicType) 
    h <- f_h(regionExtent, defaultWidth)
    ggsave(filename = fileName_out_stressDays_count, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300)
    system2('pdfcrop', c(fileName_out_stressDays_count, fileName_out_stressDays_count)) # gets rid of white space around the figure in the pdf
    print(paste0("fileName out: ", fileName_out_stressDays_count))
    
    # share of stress days in gsl days -------------------------
    gsl_ratio <- 100 * r_stress/gsl_region_masked
    gsl_ratio[gsl_ratio > 100] <- 100 # to deal with a few rare cases
    names(gsl_ratio) <- "stressShare"
    
    gsl_ratio_p <- as.polygons(gsl_ratio, dissolve = TRUE, na.rm = TRUE)
    gsl_ratio_p_proj <- project(gsl_ratio_p, proj_to_use)
    gsl_ratio_p_proj_sf <- sf::st_as_sf(gsl_ratio_p_proj)
    
    breaks <- testbreaks
    gsl_ratio_p_proj_sf$stressShare_disc <- cut(gsl_ratio_p_proj_sf$stressShare, breaks = breaks, include.lowest = TRUE, right = TRUE)
    gsl_ratio_p_proj_sf <- sf::st_intersection(gsl_ratio_p_proj_sf, border_proj_sf)
    gsl_ratio_p_proj_sf$stressShare_disc_f <- as.factor(gsl_ratio_p_proj_sf$stressShare_disc)
    legendTitle <- paste0("Period share (%), days with PWC below ", stressValue, "%")
    
    titleText_start <- paste0(waterSourceText_upper, " " , cropName, ", share of days in period with PWC below\n", stressValue, " percent, ")
    titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
      titleText_start <- paste0(f_quarterFancyName(cropName), ", share of days in period with PWC below\n ", stressValue, " percent, ")
      titleText <- paste0(titleText_start, sspText, ", ", yearSpanText, ", ", regionFancyName)
    }
     if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
    
    caption <- caption_land_area
    
    colorValues <- rev(colorList_disc_10_RdYlGn)
    g <- ggplot(gsl_ratio_p_proj_sf, aes(fill = stressShare_disc_f)) +
      scale_fill_manual(values = colorValues, drop = FALSE) +
      geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
      theme_bw() +
      theme(
        legend.text.align = 1,
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 3.0, size = 8),
        legend.margin = margin(-20, 0, 0, 0)
      ) +
      theme(legend.position = 'bottom') +
      geom_sf(data = border_proj_sf, 
              color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
      theme(legend.margin = margin(-20, 0, 0, 0)) +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
    print(g)
    
    h <- f_h(regionExtent, defaultWidth)
    fileName_stressDaysRatio_out <- paste0("graphics/cts/stressDaysRatiobyCrop/", outChoice, "/stressDaysGSShare_",  regionChoice, "_", cropChoice, "_", waterSource, "_", outChoice, "_daysCts_", runlength,  "_stressValue_", stressValue, "_", k, "_", yearSpan, ".", graphicType)
    ggsave(filename = fileName_stressDaysRatio_out, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300) 
    system2('pdfcrop', c(fileName_stressDaysRatio_out, fileName_stressDaysRatio_out)) # gets rid of white space around the figure in the pdf
    print(paste0("fileName out: ", fileName_stressDaysRatio_out))
  }
  
  f_thi_mean_crop_graphing <- function(k, l, cropChoice, waterSource, outChoice, regionChoice, breaks) {
    # calculate the mean PWC during the growing season and graph it. The 'growing season can be a quarter as can the cropchoice.
    yearSpan <- paste0(l, "_", l + yearRange)
    regionExtent <- f_regionExtentLookup(regionChoice)
    cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
    yearSpanText <- f_periodFancyName(yearSpan)
    sspText <- f_sspText(k)
    regionFancyName <- f_regionFancyName(regionChoice) 
    waterSourceText <- f_waterSourceText(waterSource)
    cropMask_region <- crop(cropMask_globe, regionExtent)
    
    stressMetric <- f_stressMetricText(outChoice) # returns WBGT or UTCI for use in captions, etc.
    #   browser()
    border <- get(regionChoice)
    border <- crop(border, regionExtent)
    border_sf <- sf::st_as_sf(border)
    border_proj_sf <- sf::st_transform(border_sf, proj_to_use)
    
    # aggSwitch <- TRUE # whether or not to aggregate the cropmask data to 1/2 degree
    # cutoff <- 0 # should this be zero?  used to remove locations from the crop mask that are too 'small' in area
    
    #  print(paste0("cropChoice: ", cropChoice))
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) { # note use of cropChoice here, cropName in the else statement
      #  crop_loc_globe <- rast(paste0(rasterMaskLoc, cropChoice, ".tif")) # dummy to say all locations are included in the crop mask
      crop_loc_globe <- cropMask_globe
    } else {
      cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
      crop_loc_globe <- rast(paste0(rasterMaskLoc, cropName, ".tif")) # 1/0 for location of crop area, globally
    }
    #   if (aggSwitch) crop_loc_region <- aggregate(cropMask, fact = 6, fun = "sum", na.rm = TRUE) # needed because PWCvals is always 1/2 degree
    
    # create crop mask by region and convert to 1/NA
    crop_loc_region <- any(crop(crop_loc_globe, regionExtent)) # is the 'any' needed here?
    crop_loc_region <- mask(crop_loc_region, cropMask_region, maskvalues = NA)
    crop_loc_region[crop_loc_region == 0] <- NA
    # 
    if (is.na(minmax(crop_loc_region)[1])) stop(paste0("crop area of ", cropName, " in region ", regionChoice, " is zero."))
    
    # convert crop_loc_region to polygons and to sf
    crop_loc_region_p <- as.polygons(crop_loc_region, dissolve = TRUE, na.rm = TRUE)
    crop_loc_region_p_proj <- project(crop_loc_region_p, proj_to_use)
    crop_loc_region_p_proj_sf <- sf::st_as_sf(crop_loc_region_p_proj)
    
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) {
      # do calcs by quarters of the calendar, not actual growing seasons
      dayCt_quarters <- c(90, 91, 92, 92) # days in each nonleap year quarter - https://www.google.com/search?client = safari&rls = en&q = number+of+days+in+a+quarter&ie = UTF-8&oe = UTF-8
      qnum <- as.numeric(gsub("q", "", cropChoice))
      maturity_days <- cumsum(dayCt_quarters)
      plant_days <- maturity_days - dayCt_quarters + 1
      gsl <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = dayCt_quarters[qnum])
      plant <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = plant_days[qnum])
      maturity <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = maturity_days[qnum])
      gsl <- crop(gsl, regionExtent)
      plant <- crop(plant, regionExtent)
      maturity <- crop(maturity, regionExtent)
    } else {
      calendar_in <- paste0("data-raw/crops/cropCalendars_ggcmi/", cropChoice, "_", waterSource, "_ggcmi_crop_calendar_phase3_v1.01.nc4")
      temp <- rast(calendar_in)
      temp <- crop(temp, regionExtent)
      plant <- temp$planting_day
      maturity <- temp$maturity_day
      x <- rast(nlyrs = 366, nrows = nrow(plant), ncols = ncol(plant), extent = ext(plant), vals = 0)
      gsl <- rapp(x, plant, maturity, sum, na.rm = TRUE, circular = TRUE)
    }
    #read in PWCvals
    PWCvals <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
    PWCvals_region <- crop(PWCvals, regionExtent)
    PWCvals_region <- mask(PWCvals_region, cropMask_region, maskvalues = NA) 
    PWCvals_region_masked <- mask(PWCvals_region, border)
    PWCvals_region_masked <- mask(PWCvals_region_masked, coastline, inverse = TRUE, touches = FALSE) #  removes coastal pixels ------
    
    fileName_out_mean_days <- paste0(locOfDataFiles_pwc, "crops/", outChoice, "/", regionChoice, "_", cropChoice, "_", waterSource, "_ensemble_", outChoice, "_daily_mean", "_", k, "_mean_", yearSpan, ".tif")
    
    # r_mean - mean PWC values in the cropped area between planting and maturity windows 
    print(system.time(r_means <- rapp(PWCvals_region_masked, plant, maturity, mean, na.rm = TRUE, circular = TRUE,  filename = fileName_out_mean_days, overwrite = TRUE, wopt = woptList)))
    names(r_means) = "value"
    r_means <- mask(r_means, cropMask_region, maskvalues = NA) 
    
    r_means_p <- as.polygons(r_means, dissolve = TRUE, na.rm = TRUE)
    r_means_p_proj <- project(r_means_p, proj_to_use)
    r_means_p_proj_sf <- sf::st_as_sf(r_means_p_proj)
    r_means_p_proj_sf <- sf::st_intersection(r_means_p_proj_sf, border_proj_sf)
    
    legendTitle <- "Mean PWC value\ngrowing season (%)"
    waterSourceText_upper <- stringr::str_to_title(waterSourceText)
    
    titleText_start <- paste0(waterSourceText_upper, " " , cropName, ", growing season mean PWC,\n")
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) titleText_start <- paste0(waterSourceText_upper, " " , f_quarterFancyName(cropName), ", growing season mean PWC,\n")
    sspText <- f_sspText(k)
    titleText <- paste0(titleText_start,  sspText, ", ", yearSpanText, ", ", regionFancyName)
    if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
    
    caption <- caption_land_area
    
    # set up discrete breaks based on breaks_PWC values -----
    r_means_p_proj_sf$valueDiscr <- cut(r_means_p_proj_sf$value, breaks = breaks, 
                                        include.lowest = TRUE, right = TRUE)
    yearSpanText <- f_periodFancyName(yearSpan)
    sspText <- f_sspText(k)
    regionFancyName <- f_regionFancyName(regionChoice) 
    waterSourceText <- f_waterSourceText(waterSource)
    
    if (cropChoice %in% c("q1", "q2", "q3", "q4")) { # for quarterly results
      titleText <- paste0("Mean PWC for ",  f_quarterFancyName(cropName), ", ",  sspText, ", ", yearSpanText, ", ", regionFancyName)
      legendTitle <- "Mean season PWC value (%)"
      if (k == "historical" ) titleText <- gsub(", ,", "", titleText, fixed = TRUE)
      caption <- ""
    }
    caption <- caption_land_area
    bigGroups <- c("globe", "countries_Africa", "country_India", "country_Brazil", "country_US")
    gridLineSize <- 0.1; borderLineSize <- 0.2
    if (regionChoice %in% bigGroups) {
      gridLineSize <- 0.005; borderLineSize <- 0.05
    }
    
    g <- ggplot(r_means_p_proj_sf, aes(fill = valueDiscr)) +
      scale_fill_manual(values = colorList_disc_8_RdYlGn, drop = FALSE) +
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
      # theme(legend.margin = margin(-20, 0, 0, 0)) +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
    print(g)
    
    h <- f_h(regionExtent, defaultWidth)
    fileName_PWCmeanSeason_out <- paste0("graphics/PWCmeanSeason/", outChoice, "/meanGSPWC_", regionChoice, "_",  cropChoice, "_watersource_", waterSource, "_", outChoice, "_", k, "_", yearSpan, ".", graphicType)
    ggsave(filename = fileName_PWCmeanSeason_out, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300) 
    system2('pdfcrop', c(fileName_PWCmeanSeason_out, fileName_PWCmeanSeason_out)) # gets rid of white space around the figure in the pdf
    print(paste0(" Done with fileName out: ", fileName_PWCmeanSeason_out))
    g <- NULL
  }
  
  
  f_cts <- function(k, l, stressValue, outChoice, regionChoice) { # not crop-based
    yearSpan <- paste0(l, "_", l + yearRange)
    regionExtent <- f_regionExtentLookup(regionChoice)
    border <- get(regionChoice)
    border_cropped <- crop(border, regionExtent)
    border_cropped_sf <- sf::st_as_sf(border_cropped)
    border_cropped_proj_sf <- sf::st_transform(border_cropped_sf, proj_to_use)
    regionExtent <- f_regionExtentLookup(regionChoice)
    # create crop mask by region and convert to 1/NA
    # crop_loc_region <- any(crop(cropMask_globe, regionExtent))
    # crop_loc_region <- aggregate(cropMask_globe, fact = 6, fun = sum)
    # crop_loc_region[crop_loc_region > 1] <- 1
    # crop_loc_region[crop_loc_region < 1] <- NA
    # waterSourceText <- f_waterSourceText(waterSource)
    stressMetric <- f_stressMetricText(outChoice) # returns WBGT or UTCI for use in captions, etc.
    
    PWCvals <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
    PWCvals_region <- crop(PWCvals, regionExtent)
    PWCvals_region_masked <- mask(PWCvals_region, crop_loc_region, maskvalues = NA) 
    
    PWCvals_region_masked <- mask(PWCvals_region_masked, coastline, inverse = TRUE, touches = FALSE) # removes coastal pixels 
    
    # coastline_cropped <- crop(coastline, regionExtent )
    # coastline_cropped_proj <- project(coastline_cropped, proj_to_use)
    #   coastline_cropped_proj_sf <- border
    
    fileName_out <- paste0(locOfDataFiles_pwc, regionChoice, "_", "cts_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".tif")
    stress <- PWCvals_region_masked < stressValue # daily locations globally where PWC is less than the stress cutoff, TRUE/FALSE
    # r - number of days in the cropped area between planting and maturity windows where the stress values are met
    print(system.time(r <- app(stress, sum, na.rm = TRUE, circular = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList))) 
    names(r) = "value"
    r_p <- as.polygons(r, dissolve = TRUE, na.rm = TRUE)
    r_p_proj_proj <- project(r_p, proj_to_use)
    r_p_proj_proj_sf <- sf::st_as_sf(r_p_proj_proj)
    
    r_p_proj_proj_sf$valueDiscr <- cut(r_p_proj_proj_sf$value, breaks = breaks_PWC, 
                                       include.lowest = TRUE, right = TRUE)
    maxBreaks <- breaks_PWC[length(breaks_PWC)]
    r_p_proj_proj_sf$value[r_p_proj_proj_sf$value > maxBreaks] <- maxBreaks
    r_p_proj_proj_sf$valueDiscr <- cut(r_p_proj_proj_sf$value, breaks = breaks_PWC, 
                                       include.lowest = TRUE, right = TRUE)
    legendTitle <- "No. of days"
    yearSpanText <- f_periodFancyName(yearSpan)
    sspText <- f_sspText(k)
    regionFancyName <- f_regionFancyName(regionChoice) 
    
    titleText <- paste0("Days with PWC below ", stressValue, " percent, ", sspText, ", ", yearSpanText, ", ", regionFancyName)
    if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
    
    caption <- ""
    bigGroups <- c("globe", "countries_Africa", "country_India", "country_Brazil", "country_US")
    gridLineSize <- 0.1; borderLineSize <- 0.2
    if (regionChoice %in% bigGroups) {
      gridLineSize <- 0.01; borderLineSize <- 0.05
    }
    
    g <- ggplot(r_p_proj_proj_sf, aes(fill = valueDiscr)) +
      scale_fill_manual(values = rev(colorList_disc_5_RdYlGn), drop = FALSE) +
      geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + #
      theme_bw() +
      theme(
        legend.text.align = 1,
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 3.0, size = 8)
      ) +
      geom_sf(data = border_cropped_proj_sf, 
              color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
      theme(legend.margin = margin(-20, 0, 0, 0)) +
      theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
    print(g)
    fileName_out <- paste0("graphics/cts/stressDays/", regionChoice, "_",  "cts_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".", graphicType)
    h <- f_h(regionExtent, defaultWidth)
    ggsave(filename = fileName_out, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300) 
    system2('pdfcrop', c(fileName_PWCmeanSeason_out, fileName_PWCmeanSeason_out)) # gets rid of white space around the figure in the pdf
    print(paste0("fileName out: ", fileName_out))
  }
}

# calculate runs -----
  for (regionChoice in regionChoices) {
    for (stressValue in stressValues) {
      for (runlengthChoice in runlengthChoices) {
        #scenarios
        for (k in sspChoices) {
          for (l in startYearChoices) {
            f_runs_calculator(k, l, runlengthChoice, stressValue, outChoice, regionChoice)
          }
        }
        #historical
        k <- "historical"
        l <- 1991
        f_runs_calculator(k, l, runlengthChoice, stressValue, outChoice, regionChoice)
      }
    }
}

# table 1 for PWC paper -----
{
  stressValues = 75
  runlengthChoices <- c(1)
  dt_stressCts <-  data.table(outChoice = character(), ssp = character(), startYear = numeric(), minRunLength = numeric(), stressLevel = numeric(), location = character(), stressedPop = numeric(), stressedPop_ag = numeric(), pop_ag = numeric(), totalPop = numeric(), stressed_area_crop_ag = numeric(), area_crop_ag = numeric(), region = character())
  
  for (regionChoice in regionChoices) {
    for (stressValue in stressValues) {
      for (runlength in runlengthChoices) {
        for (outChoice in c("pwc_wbgt_out")) { #, "pwc_utci_out")) {
          #scenarios
          for (k in sspChoices) {
            for (l in startYearChoices) {
              cts <- f_stressCtsnGraphics(k, l, runlength, stressValue, outChoice, popCtGlobal_urban, popCtGlobal_rural, regionChoice)
              cts_urban <- cts[[1]]; cts_rural <- cts[[2]]
              dt_stressCts <- rbind(dt_stressCts, cts_urban, cts_rural)
            }
          }
          #historical
          k <- "historical"
          l <- 1991
          cts <- f_stressCtsnGraphics(k, l, runlength, stressValue, outChoice, popCtGlobal_urban, popCtGlobal_rural, regionChoice)
          cts_urban <- cts[[1]]; cts_rural <- cts[[2]]
          dt_stressCts <- rbind(dt_stressCts, cts_urban, cts_rural)
        }
      }
    }
  }
  print(dt_stressCts)
  fileName_out <- paste0("results/stressMetrics_region",  ".csv")
  write.csv(dt_stressCts, file = fileName_out, row.names = FALSE)
  print(paste0("filename out: ", fileName_out)) 
}

# generate stress counts by crop areas ------

# take map of global population stress levels, use the crop mask to only include areas where crop area was located in early century and then sum

# test values
stressValue <- 75
cropChoice <- "mai"
cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
l <- 2081
k <- "ssp585"

fileName_mask_crop_in <- paste0(rasterMaskLoc, cropName, ".tif")
crop_loc_globe <- rast(fileName_mask_crop_in)
runlength <- 5
logicDirection <- "<"

# get crop area and population in region ------
{
  runlength <- 1
  logicDirection <- "<"
  dt_stressCts_crop <- data.table(outChoice = character(), region = character(), crop = character(), ssp  = character(), startYear  = numeric(), minRunLength = numeric(), aveRunLength = numeric(), extremeStressLevel = numeric(), crop_area_region = numeric(), crop_area_region_stressed = numeric(), Pop_region_stressed_tot = numeric(), Pop_region_stressed_rural = numeric(), Pop_region_stressed_urban = numeric(), Pop_region_tot = numeric())
  
  for (cropChoice in cropChoices) {
    cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
    for (outChoice in c("pwc_wbgt_out")) { # , "pwc_utci_out"
      for (regionChoice in regionChoices)
        #scenarios
        for (k in sspChoices) {
          for (l in startYearChoices) { 
            for (stressValue in stressValues) {
              cts <- f_dt_stressCts_crop(k, l, runlength, logicDirection, stressValue, cropChoice, outChoice, regionChoice)
              dt_stressCts_crop <- rbind(dt_stressCts_crop, cts)
            }
          }
        }
      #historical
      k = "historical"
      l = 1991
      #     for (runlength in runlengthChoices) { 
      for (stressValue in stressValues) {
        cts <- f_dt_stressCts_crop(k, l, runlength, logicDirection, stressValue, cropChoice, outChoice, regionChoice)
        dt_stressCts_crop <- rbind(dt_stressCts_crop, cts)
      }
      #   }
    }
  }
  
  dt_stressCts_crop[, popstressedRatio_tot := round(Pop_region_stressed_tot/Pop_region_tot, 3)][
    , popstressedRatio_urban := round(Pop_region_stressed_urban/Pop_region_urban, 3)][
      , popstressedRatio_rural := round(Pop_region_stressed_rural/Pop_region_rural, 3)]
  print(dt_stressCts_crop)
  
  write.csv(dt_stressCts_crop, file = paste0("results/stressedPop_crop", "_", outChoice, ".csv"), row.names = FALSE)
}

# use crop calendars -----
cropmasks_complete <- c("almond", "apple", "barley", "bean", "blueberry", "broadbean", "cassava", "cherry", "chickpea", "cotton", "cowpea", "grape", "groundnut", "lentil", "maize", "millet", "mustard", "oats", "pea", "potato", "rapeseed", "rice", "rye", "sorghum", "soybean", "sugarbeet", "sunflower", "sweetpotato", "tomato", "walnut", "wheat", "yam")
cropmasks_perennial <- c("almond", "apple", "blueberry", "cherry", "grape")
cropmasks_annual <- cropmasks_complete[!cropmasks_complete %in% cropmasks_perennial]
#cropChoices <- c("cas", "bar", "mai",  "nut", "ri1", "sgc", "wwh") #"mil",

# test data
cropChoice <- "wwh"
waterSource <- "rf"
yearSpan <- paste0(l, "_", l + yearRange)

stressValues <- 75
runlength <- 1
logicDirection <- "<" 
ldtext <- "lt"
test_logic <- paste0("x ", logicDirection, " ", stressValue)

dt_ctsOut <- data.table(outChoice = character(), crop = character(), ssp  = character(), startYear  = character(), waterSource = character(), stressValue = numeric(), stressedPop = numeric(), totalPop = numeric(), region = character())

for (regionChoice in regionChoices) {
  print(regionChoice)
  regionFancyName <- f_regionFancyName(regionChoice) 
  regionExtent <- f_regionExtentLookup(regionChoice)
  r_pop_region_urban_croploc <- crop(popCtGlobal_urban, regionExtent, snap = "near") # number of urban people in each 1/2 degree pixel
  r_pop_region_rural_croploc <- crop(popCtGlobal_rural, regionExtent, snap = "near")  # number of rural people in each 1/2 degree pixel
  r_pop_region_tot_croploc <- crop(popCtGlobal_total, regionExtent, snap = "near")
  
  for (cropChoice in cropChoices) {
    print(cropChoice)
    cropName <- cropNameLookup[crop_abb == cropChoice, crop_name]
    crop_loc_globe <- rast(paste0(rasterMaskLoc, cropName, ".tif")) 
    crop_loc_region <- crop(crop_loc_globe, regionExtent)
    crop_loc_region[crop_loc_region == 0] <- NA # set locations with no crops to NA
    crop_loc_region[crop_loc_region > 0] <- 1 # set locations with any crops to 1
    
    for (waterSource in waterSources) {
      calendar_in <- paste0("data-raw/crops/cropCalendars_ggcmi/", cropChoice, "_", waterSource, "_ggcmi_crop_calendar_phase3_v1.01.nc4")
      
      #      # do calcs by quarters of the calendar, not actual growing seasons
      # dayCt_quarters <- c(90, 91, 92, 92) # days in each nonleap year quarter - https://www.google.com/search?client = safari&rls = en&q = number+of+days+in+a+quarter&ie = UTF-8&oe = UTF-8
      # qnum <- as.numeric(gsub("q", "", cropChoice))
      # maturity_days <- cumsum(dayCt_quarters)
      # plant_days <- maturity_days - dayCt_quarters + 1
      # gsl <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = dayCt_quarters[qnum])
      # plant <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = plant_days[qnum])
      # maturity <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = maturity_days[qnum])
      # 
      temp <- rast(calendar_in)
      temp <- crop(temp, regionExtent)
      plant <- temp$planting_day
      maturity <- temp$maturity_day
      gsl <- temp$growing_season_length
      # x <- rast(nlyrs = 366, nrows = nrow(plant), ncols = ncol(plant), extent = ext(plant), vals = 0)
      # gsl <- rapp(x, plant, maturity, sum, na.rm = TRUE, circular = TRUE)
      for (outChoice in outChoices) {
        for (stressValue in stressValues) {
          for (k in sspChoices) {
            for (l in startYearChoices) { 
              yearSpan <- paste0(l, "_", l + yearRange)
              PWCvals <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
              PWCvals_cropped <- crop(PWCvals, regionExtent)
              fileName_out <- paste0(locOfDataFiles_pwc, "crops/", outChoice,"/", regionChoice, "_", cropChoice, "_", waterSource, "_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".tif")
              stress <- PWCvals_cropped < stressValue # daily locations globally where PWC is less than the stress cutoff, TRUE/FALSE
              r_cropMask <- mask(PWCvals_cropped, cropMask_region, maskvalues = NA) # locations where the crop is grown and stress days are positive, globally
              # r - number of days in the cropped area between planting and maturity windows where the stress values are met
              print(system.time(r <- rapp(stress, plant, maturity, sum, na.rm = TRUE, circular = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList))) 
              #              r_cropMask <- mask(r, crop_loc_region, maskvalues = NA) # locations where the crop is grown and stress days are positive, globally
              r_popCtStressed_tot <- mask(r_pop_region_tot, r_cropMask, maskvalues = NA) # pop counts for stressed locations
              r_popCtStressed_rural <- mask(r_pop_region_rural, r_cropMask, maskvalues = NA) # pop counts for stressed locations
              r_popCtStressed_urban <- mask(r_pop_region_urban, r_cropMask, maskvalues = NA) # pop counts for stressed locations
              # needed?    r_popMask <- mask(r_pop_region_tot, crop_loc_region, maskvalues = NA) #  pop counts for locations where crop is grown 
              
              # pop counts for locations where stress is positive
              r_pop_region_stressed_tot <- mask(r_pop_region_tot_croploc, r_stressDays_region_cropMask, maskvalues = 0); names(r_pop_region_stressed_tot) <- "popTotStressedCropLoc"
              r_pop_region_stressed_rural <- mask(r_pop_region_rural_croploc, r_stressDays_region_cropMask, maskvalues = 0); names(r_pop_region_stressed_rural) <- "popRuralStressedCropLoc"
              r_pop_region_stressed_urban <- mask(r_pop_region_urban_croploc, r_stressDays_region_cropMask, maskvalues = 0); names(r_pop_region_stressed_urban) <- "popUrbanStressedCropLoc"
              
               totalNum_stressed_tot <- f_getsum(r_pop_region_stressed_tot)
              totalNum_stressed_rural <- f_getsum(r_pop_region_stressed_rural)
              totalNum_stressed_urban <- f_getsum(r_pop_region_stressed_urban)
              
              stressed_area_crop_rural_ag <- f_getArea(r_pop_region_stressed_rural_ag, 1)
              stressed_area_crop_urban_ag <- f_getArea(r_pop_region_stressed_urban_ag, 1)
              area_crop_rural_ag <- f_getArea(r_pop_region_rural_ag, 1)
              area_crop_urban_ag <- f_getArea(r_pop_region_urban_ag, 1)
              
              # totalNum_stressed_crop_area <- as.numeric(global(r_popCtStressed, fun = "sum", na.rm = TRUE)[1])/1000000
              # totalNum_crop_area <- as.numeric(global(r_popMask, fun = "sum", na.rm = TRUE)[1])/1000000
              # 
              cts <- list(outChoice, cropChoice, k, l, waterSource, stressValue, totalNum_stressed_crop_area, totalNum_crop_area, regionFancyName)
              dt_ctsOut <- rbind(dt_ctsOut, cts)
              print(paste0("done with ", fileName_out))
            }
          }
          
          # add historical
          k <- "historical"
          l <- 1991
          yearSpan <- paste0(l, "_", l + yearRange)
          PWCvals <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
          PWCvals_cropped <- crop(PWCvals, regionExtent)
          fileName_out <- paste0(locOfDataFiles_pwc, "crops/", outChoice,"/", regionChoice, "_", cropChoice, "_", waterSource, "_ensemble_", outChoice, "_daily_mean", "_", k, "_sval_", stressValue, "_", yearSpan, ".tif")
          stress <- PWCvals_cropped < stressValue # daily locations globally where PWC is less than the stress cutoff, TRUE/FALSE
          r_cropMask <- mask(PWCvals_cropped, crop_loc_region, maskvalues = NA) # locations where the crop is grown and stress days are positive, globally
          # r - number of days in the cropped area between planting and maturity windows where the stress values are met
          print(system.time(r <- rapp(stress,  plant, maturity, sum, na.rm = TRUE, circular = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList))) 
          r_cropMask <- mask(r, crop_loc_region, maskvalues = NA) # locations where the crop is grown and stress days are positive, globally
          r_popCtExtreme <- mask(r_pop_region_tot, r, maskvalues = 0) # pot counts for stressed locations
          r_popMask <- mask(r_pop_region_tot, crop_loc_region, maskvalues = NA) #  pop counts for locations where crop is grown
          totalNum_stressed_crop_area <- as.numeric(global(r_popCtExtreme, fun = "sum", na.rm = TRUE)[1])/1000000
          totalNum_crop_area <- as.numeric(global(r_popMask, fun = "sum", na.rm = TRUE)[1])/1000000
          
          cts <- list(outChoice, cropChoice, k, l, waterSource, stressValue, totalNum_stressed_crop_area, totalNum_crop_area, regionFancyName)
          dt_ctsOut <- rbind(dt_ctsOut, cts)
          print(paste0("done with ", fileName_out))
        }
      }
    }
  }
}

dt_ctsOut[, popRatio := stressedPop/totalPop]
write.csv(dt_ctsOut, file = paste0("results/crop_specific_stressMetrics_", outChoice, ".csv"), row.names = FALSE)


#demos
PWCvals_early <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", "historical",  "_", "1991_2010", ".tif"))
PWCvals_late <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", "ssp585_2081_2100.tif"))
PWCvals_late <- crop(PWCvals_late, extent_noAntarctica)

# writeCDF(PWCvals_late, filename = "PWCvals_late.nc", varname = "pwc", longname = "Physical work capacity, SSP5-8.5, End century", unit = "percent", zname = "day", prec = "double", compression = 4,overwrite = TRUE,  var_temp)
temp <- rast("data/wbgt/crops/mai_rf_ensemble_pwc_wbgt_out_daily_mean_ssp585_sval_60_2081_2100.tif")
plot(temp, main = "Maize, Stress days in a representative ", waterSourceText, " growing season, PWC less than 60%, ssp 585, 2081-2100", range = c(0,100))

temp <- rast("data/wbgt/crops/mai_rf_ensemble_pwc_wbgt_out_daily_mean_historical_sval_60_1991_2010.tif")
plot(temp, main = "Maize, Stress days a representative ", waterSourceText, " growing season, PWC less than 60%, eary century, 1991-2010", range = c(0,100))

waterSources <- "rf"
stressValues <- 75

# do calendars by quarter ------
cropChoices <- c("q1", "q3")  #"q1", "q2",, "q4"
#cropChoices <- c("mai", "swh", "ri1", "sor")
require(dplyr)
# Figure 2 and table for PWC paper ----
#create empty df to hold stress counts distribution 
testValues_w0_df <- data.frame(lyr1 = numeric())
stressCtsHolder <- testValues_w0_df %>% 
  group_by(gr = cut(lyr1, breaks = breaks_percent, include.lowest = TRUE, right = FALSE) ) %>% 
  summarise(n = n()) %>%
  arrange(as.numeric(gr)) %>%
  tidyr::complete(gr = levels(gr), fill = list(n = 0)) %>%
  select( -n)
# run code to fill up the empty df
waterSource <- "rf"
#breakList <- testbreaks
for (cropChoice in cropChoices) {
  for (regionChoice in regionChoices) {
    for (stressValue in stressValues) {
      #scenarios
      for (k in sspChoices) {
        for (l in startYearChoices) {
          #       for (waterSource in waterSources) {
          f_thi_mean_crop_graphing(k, l, cropChoice, waterSource, outChoice, regionChoice, breaks = testbreaks_8)
          stressCts <- f_thi_cts_crop_graphing(k, l, stressValue, cropChoice, waterSource, outChoice, regionChoice, breaks = testbreaks)
        }
      }
    }
    #historical    
    k = "historical"
    l = 1991
    #for (waterSource in waterSources) {
    f_thi_mean_crop_graphing(k, l, cropChoice, waterSource, outChoice, regionChoice, breaks = testbreaks_8)
    stressCts <- f_thi_cts_crop_graphing(k, l, stressValue, cropChoice, waterSource, outChoice, regionChoice, breaks = testbreaks)
    #       stressCtsHolder <- cbind(stressCtsHolder, stressCts)
    # }
  }
}

stressCtsHolder_t <- as.data.table(t(stressCtsHolder),keep.rownames = TRUE)
setnames(stressCtsHolder_t, old = names(stressCtsHolder_t), new = paste0("X", as.character(stressCtsHolder_t[1])))
setnames(stressCtsHolder_t, old = "Xgr", new = "cropVar")
stressCtsHolder_t <- stressCtsHolder_t[-1,] 
stressCtsHolder_t <- stressCtsHolder_t[, c("crop", "waterSource", "ssp", "startYear", "stressCutoff", "pwc", "metric", "out") := tstrsplit(cropVar, "_")]
stressCtsHolder_t[, c("pwc", "out") := NULL]
write.csv(stressCtsHolder_t, paste0("results/stressCtsHolder_crops_", outChoice, "_", regionChoice,  ".csv"))

# Figure 1 annual days in region with stress level below stressValue -----
#  
for (outChoice in outChoices) {
  for (regionChoice in regionChoices) {
    for (stressValue in stressValues) {
      #scenarios
      for (k in sspChoices) {
        for (l in startYearChoices) {
          f_cts(k, l, stressValue, outChoice, regionChoice)
        }
      }
      #historical
      k = "historical"
      l = 1991
      f_cts(k, l, stressValue, outChoice, regionChoice)
    }
  }
}

# annual means -----
outChoice <- "pwc_wbgt_out"

for (regionChoice in regionChoices) {
  regionExtent <- f_regionExtentLookup(regionChoice)
  regionFancyName <- f_regionFancyName(regionChoice) 
  
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      PWCvals_in <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
      filename_PWC_annual_mean_out <- paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_annual_mean", "_", k,  "_", yearSpan, ".tif")
      PWC_annual_mean <- app(PWCvals_in, mean, filename = filename_PWC_annual_mean_out, overwrite = TRUE, wopt = woptList)
    }
  }
  
  k = "historical"
  l = 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  PWCvals_in <- rast(paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_daily_mean", "_", k,  "_", yearSpan, ".tif"))
  filename_PWC_annual_mean_out <- paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_annual_mean", "_", k,  "_", yearSpan, ".tif")
  PWC_annual_mean <- app(PWCvals_in, mean, filename = filename_PWC_annual_mean_out, overwrite = TRUE, wopt = woptList)
}

# annual means graphed, by region -----
f_annualMeans <- function(k, l, stressValue, regionChoice, popCtGlobal_rural, popCtGlobal_urban, breaks) {
  yearSpan <- paste0(l, "_", l + yearRange)
  print(paste0("regionChoice: ", regionChoice, ", stressValue: ", stressValue, ", yearSpan: ", yearSpan))
  regionExtent <- f_regionExtentLookup(regionChoice)
  regionFancyName <- f_regionFancyName(regionChoice) 
  cropMask_region <- crop(cropMask_globe, regionExtent)
  
  r_pop_region_rural <- crop(popCtGlobal_rural, regionExtent, snap = "near")
  r_pop_region_urban <- crop(popCtGlobal_urban, regionExtent, snap = "near")
  totalNum_region_rural <- f_getsum(r_pop_region_rural)
  totalNum_region_urban <- f_getsum(r_pop_region_urban)
  
  # rural and urban population only in regions with crops
  r_pop_region_rural_ag <- mask(r_pop_region_rural, cropMask_region, maskvalues = NA)
  r_pop_region_urban_ag <- mask(r_pop_region_urban, cropMask_region, maskvalues = NA)
  totalNum_region_rural_ag <- f_getsum(r_pop_region_rural_ag)
  totalNum_region_urban_ag <- f_getsum(r_pop_region_urban_ag)
  
  filename_PWC_annual_mean_in <- paste0(locOfDataFiles_pwc, "ensemble_", outChoice, "_annual_mean", "_", k,  "_", yearSpan, ".tif")
  PWC <- rast(filename_PWC_annual_mean_in)
  PWC_region <- crop(PWC, regionExtent)
  PWC_region_ag <- mask(PWC_region, cropMask_region, maskvalues = NA) # mask regions with no crops
  
  # get pop count for areas where average PWC in crop areas is less than stressValue cutoff
  
  #find population numbers where average annual PWC in the cropped region is less than the stress value
  stresslocs <- 1*(PWC_region < stressValue)
  # stresslocs[stresslocs < stressValue] <- NA
  stresslocs[stresslocs == 0] <- NA
  
  stresslocs_ag <- 1*(PWC_region_ag < stressValue)
  # stresslocs[stresslocs < stressValue] <- NA
  stresslocs_ag[stresslocs_ag == 0] <- NA
  
  r_pop_region_stressed_rural <- mask(r_pop_region_rural, stresslocs, maskvalues = NA)
  r_pop_region_stressed_urban <- mask(r_pop_region_urban, stresslocs, maskvalues = NA)
  totalNum_region_stressed_rural <- f_getsum(r_pop_region_stressed_rural)
  totalNum_region_stressed_urban <- f_getsum(r_pop_region_stressed_urban)
  
  # regions with crops with PWC value less than stressValue
  r_pop_region_stressed_rural_ag <- mask(r_pop_region_rural_ag, stresslocs_ag, maskvalues = NA)
  r_pop_region_stressed_urban_ag <- mask(r_pop_region_urban_ag, stresslocs_ag, maskvalues = NA)
  totalNum_region_stressed_rural_ag <- f_getsum(r_pop_region_stressed_rural_ag)
  totalNum_region_stressed_urban_ag <- f_getsum(r_pop_region_stressed_urban_ag)
  
  # get stressed area, in 1000 sq km
  stressed_area_crop_rural_ag <- f_getArea(r_pop_region_stressed_rural_ag, 1)
  stressed_area_crop_urban_ag <- f_getArea(r_pop_region_stressed_urban_ag, 1)
  area_crop_rural_ag <- f_getArea(r_pop_region_rural_ag, 1)
  area_crop_urban_ag <- f_getArea(r_pop_region_urban_ag, 1)
  
  runlength <- 0 # something to put into runlength so the csv file looks the same as in f_dt_stressCts
  # cts_rural <- list(outChoice, k, l, runlength, stressValue, "rural", totalNum_region_rural, totalNum_region_rural_ag, totalNum_region_stressed_rural, regionFancyName)
  # cts_urban <- list(outChoice, k, l, runlength, stressValue, "urban", totalNum_region_urban, totalNum_region_urban_ag, totalNum_region_stressed_urban, regionFancyName)
  # cts <- list(cts_urban, cts_rural)
  
  cts_urban <- list(outChoice, k, l, runlength, stressValue, "urban", totalNum_region_stressed_urban, totalNum_region_stressed_urban_ag, totalNum_region_urban_ag, totalNum_region_urban, stressed_area_crop_urban_ag,  area_crop_urban_ag, regionFancyName)
  cts_rural <- list(outChoice, k, l, runlength, stressValue, "rural", totalNum_region_stressed_rural, totalNum_region_stressed_rural_ag, totalNum_region_rural_ag, totalNum_region_rural, stressed_area_crop_rural_ag, area_crop_rural_ag, regionFancyName)
  cts <- list(cts_urban, cts_rural)
  
  border <- get(regionChoice)
  border_sf <- sf::st_as_sf(border)
  border_proj_sf <- sf::st_transform(border_sf, proj_to_use)
  
  PWC_cropped_p <- as.polygons(PWC_region_ag, dissolve = TRUE, na.rm = TRUE)
  PWC_cropped_p_proj <- project(PWC_cropped_p, proj_to_use)
  PWC_cropped_p_proj_sf <- sf::st_as_sf(PWC_cropped_p_proj)
  PWC_cropped_p_proj_sf <- sf::st_intersection(PWC_cropped_p_proj_sf, border_proj_sf)
  PWC_cropped_p_proj_sf$meanDiscr <- cut(PWC_cropped_p_proj_sf$mean, breaks = breaks, include.lowest = TRUE, right = TRUE)
  
  regionFancyName <- f_regionFancyName(regionChoice) 
  yearSpanText <- f_periodFancyName(yearSpan)
  
 # bigGroups <- c("globe", "countries_Africa", "country_India", "country_Brazil", "country_US")
  gridLineSize <- 0.1
  borderLineSize <- 0.2
  # if (regionChoice %in% bigGroups) {
  #   gridLineSize <- 0.01; borderLineSize <- 0.05
  # }
  
  legendTitle <- "Mean daily PWC"
  yearSpanText <- f_periodFancyName(yearSpan)
  sspText <- f_sspText(k)
  regionFancyName <- f_regionFancyName(regionChoice) 
  
  titleText <- paste0("Annual mean PWC in a representative year (%), " , sspText, ", ", yearSpanText, ", ", regionFancyName)
  if (k == "historical" ) titleText <- gsub(", ,", ",", titleText, fixed = TRUE)
  caption <- caption_land_area #Rectangles show 1/2 degree grid."   
  
  PWC_cropped_p_proj_sf$meanDiscr <- as.factor(PWC_cropped_p_proj_sf$meanDiscr)
  colorValues <- colorList_disc_8_RdYlGn
  g <- ggplot(PWC_cropped_p_proj_sf, aes(fill = meanDiscr)) +
    scale_fill_manual(values = colorValues, drop = FALSE) +
    geom_sf(show.legend = legendSwitch, size = gridLineSize) + 
    labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + #
    theme_bw() +
    theme(
      legend.text.align = 1,
      axis.ticks = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(hjust = 0, size = 8), #, vjust = -4.0
      legend.position = "bottom"
    ) +
    geom_sf(data = sf::st_as_sf(border_proj_sf), color = "black", size = borderLineSize, stat = "sf", fill = NA, position = "identity") +
    theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
  print(g)
  fileName_PWCmeanSeason_out <- paste0("graphics/PWCmeanSeason/", outChoice, "/meanGSPWC_", regionChoice, "_", outChoice, "_", k, "_", yearSpan, ".", graphicType)
  h <- f_h(regionExtent, defaultWidth)
  ggsave(filename = fileName_PWCmeanSeason_out, plot = g, device = graphicType, width = defaultWidth, height = h, units = "in", dpi = 300) 
  system2('pdfcrop', c(fileName_PWCmeanSeason_out, fileName_PWCmeanSeason_out)) # gets rid of white space around the figure in the pdf
  print(paste0("fileName out: ", fileName_PWCmeanSeason_out))
  return(cts)
}

{
  dt_stressCts <-  data.table(outChoice = character(), ssp = character(), startYear = numeric(), minRunLength = numeric(), stressLevel = numeric(), location = character(), stressedPop = numeric(), stressedPop_ag = numeric(), pop_ag = numeric(), totalPop = numeric(), stressed_area_crop_ag = numeric(), area_crop_ag = numeric(), region = character())
  
  regionChoices <- "globe"
  stressValues = 75
  runlengthChoices <- c(1, 5)
  dt_stressCts <-  data.table(outChoice = character(), ssp = character(), startYear = numeric(), minRunLength = numeric(), stressLevel = numeric(), location = character(), stressedPop = numeric(), stressedPop_ag = numeric(), pop_ag = numeric(), totalPop = numeric(), stressed_area_crop_ag = numeric(), area_crop_ag = numeric(), region = character())
  
  for (regionChoice in regionChoices) {
    for (stressValue in stressValues) {
      for (outChoice in c("pwc_wbgt_out")) { #, "pwc_utci_out")) {
        #scenarios
        for (k in sspChoices) {
          for (l in startYearChoices) {
            cts <- f_annualMeans(k, l, stressValue, regionChoice, popCtGlobal_rural, popCtGlobal_urban, breaks = testbreaks_8)
            cts_urban <- cts[[1]]; cts_rural <- cts[[2]]
            dt_stressCts <- rbind(dt_stressCts, cts_urban, cts_rural)
          }
        }
        #historical    
        k = "historical"
        l = 1991
        cts <- f_annualMeans(k, l, stressValue, regionChoice, popCtGlobal_rural, popCtGlobal_urban, breaks = testbreaks_8)
        cts_urban <- cts[[1]]; cts_rural <- cts[[2]]
        dt_stressCts <- rbind(dt_stressCts, cts_urban, cts_rural)
      }
      print(dt_stressCts)
      write.csv(dt_stressCts, file = paste0("results/stressMetrics_region_annualMean", ".csv"), row.names = FALSE)
    }
  }
}


