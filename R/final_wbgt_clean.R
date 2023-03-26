# edited to replace tas with a choice of tas or tmin, 07/25/2022
{
  library(Rcpp)
  library(terra)
  # temperature choice
  tempChoice <- "tas"
  # choice of using rsds. if FALSE create a raster with 0s instead of values
  fileDestination <- "climdata_processed/"
  # if (!tempChoice == "tas") fileDestination <- "climdata_processed/"
  # useRSDS <- TRUE
  # if (!useRSDS == TRUE) fileDestination <- "climdata_processed/"
  
  # create raster with all zeros where land is by setting values of a real RSDS raster > 0 to 0
  # files only need to be generated once
  # r_zeros_1991 <- rast("climdata_processed/mri-esm2-0_rsds_hr_historical_1991_2010.tif")
  # r_zeros_1991[r_zeros_1991 > 0] <- 0
  # writeRaster(r_zeros_1991, filename = "climdata_processed/r_zeros_1991.tif")
  # r_zeros_2041 <- rast("climdata_processed/mri-esm2-0_rsds_hr_ssp585_2041_2060.tif")
  # r_zeros_2041[r_zeros_2041 > 0] <- 0
  # writeRaster(r_zeros_2041, filename = "climdata_processed/r_zeros_2041.tif")
  # r_zeros_2081 <- rast("climdata_processed/mri-esm2-0_rsds_hr_ssp585_2081_2100.tif")
  # r_zeros_2081[r_zeros_2081 > 0] <- 0
  # writeRaster(r_zeros_2081, filename = "climdata_processed/r_zeros_2081.tif")
  
  r_zeros_1991 <- rast("climdata_processed/r_zeros_1991.tif")
  r_zeros_2041 <- rast("climdata_processed/r_zeros_2041.tif")
  r_zeros_2081 <- rast("climdata_processed/r_zeros_2081.tif")
  
  path_code <- "R/code"
  path_data <- "data"
  path_clim_processed <- "climdata_processed/"
  tmpdir <- file.path(path_data, "ISIMIP")
  woptList <- list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR = 2", "ZLEVEL = 6", "NUM_THREADS = ALL_CPUS"))
}
sourceCpp(file.path(path_code, "wbgt9.cpp"))
outChoice <- "pwc_wbgt_out" # choices are pwc_utci_out, pwc_wbgt_out, wbgt_out, utci_out, Tg_out, Tnwb_out
optimizeChoice <- 2
modelChoices <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL")
sspChoices <- c("ssp126", "ssp585")
startYearChoices <- c(2041, 2081)
yearRange <- 19
extent_noAntarctica <- ext(-180, 180, -60, 90) #-60 gets rid of Antarctica for global
# rasterMaskLoc <- "data/crop_mask/rasterMask_"

#  test data
l <- 2041
k <- "ssp126"
modelChoice <- "MRI-ESM2-0"

# infile_mask <- paste0(rasterMaskLoc, cropChoice, ".tif")
# cropMask_globe <- rast(infile_mask)
#

# create lat, day and yearnum rasters
f_latlyrs <- function(ndays) {
  lat_1lyr <- rast(nrows = 300, ncols = 720, ext = extent_noAntarctica)
  lat_1lyr <- init(lat_1lyr, "y")
  x <- lapply(1:ndays, function(i) {
    setValues(lat_1lyr, values(lat_1lyr))
  })
  lat_yr <- terra::rast(x)
}

f_doy <- function(ndays) {
  temp <- rast(nrows = 300, ncols = 720, ext = extent_noAntarctica, vals = 1)
  x <- lapply(1:ndays, function(i) {
    setValues(temp, i)
  })
  doy_yr <- terra::rast(x)
}

f_year <- function(ndays, yearNum) {
  temp <- rast(nrows = 300, ncols = 720, ext = extent_noAntarctica, vals = yearNum)
  x <- lapply(1:ndays, function(i) {
    setValues(temp, i)
  })
  year_yr <- terra::rast(x)
}

# # combine years -----
f_readRast <- function(yearNum, k, modelChoice, outChoice, optimizeChoice) {
  # browser()
  modelChoice_lower <- tolower(modelChoice)
  infile <- paste0(fileDestination, outChoice, "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", "y_", yearNum, ".tif")
  print(infile)
  r <- rast(infile)
  startDate <- paste0(yearNum, "-01-01")
  endDate <- paste0(yearNum, "-12-31")
  dates <- seq(as.Date(startDate), as.Date(endDate), 1)
  names(r) <- paste0("X", dates)
  return(r)
}
# 
f_combineYears <- function(k, l, modelChoice, outChoice, optimizeChoice) {
  # browser()
  # filesCompleted <- list.files(fileDestination, full.names = TRUE)
  # filesCompleted <- filesCompleted[!grepl("aux.xml", filesCompleted, fixed = TRUE)]
  # filesCompleted <- gsub("//", "/", filesCompleted)
  modelChoice_lower <- tolower(modelChoice)
  yearNums <- l:(l + yearRange)
  yearSpan <- paste0(l, "_", l + yearRange)
  outf <- paste0(fileDestination, outChoice, "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
  x <- lapply(yearNums, f_readRast, k, modelChoice, outChoice, optimizeChoice)
  r <- rast(x)
  print(system.time(writeRaster(r, filename = outf, overwrite = TRUE, wopt = woptList)))
  return(r)
}

# calculate Tg results separately -----
f_Tgcalc <- function(k, l, modelChoice, useRSDS) {
  # filesCompleted <- list.files(fileDestination, full.names = TRUE)
  # filesCompleted <- filesCompleted[!grepl("aux.xml", filesCompleted, fixed = TRUE)]
  # filesCompleted <- gsub("//", "/", filesCompleted)
  modelChoice_lower <- tolower(modelChoice)
  startDate_period <- paste0(l, "-01-01")
  endDate_period <- paste0(l + yearRange, "-12-31")
  dates_period <- seq(as.Date(startDate_period), as.Date(endDate_period), 1)
  yearSpan <- paste0(l, "_", l + yearRange)
  # outf_Tg <- paste0("data/wbgt/Tg_out_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
  # print(outf_Tg)
  #   if (!outf_Tg %in% filesCompleted) {
  tempComponent <- paste0("_", tempChoice, "_")
  fileName_temp <- paste0(path_clim_processed, modelChoice_lower, tempComponent, k, "_", yearSpan, ".tif")
  fileName_hurs <- paste0(path_clim_processed, modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
  fileName_sfcwind <- paste0(path_clim_processed, modelChoice_lower, "_sfcwind_", k, "_", yearSpan, ".tif")
  fileName_rsds <- paste0(path_clim_processed, modelChoice_lower, "_rsds_", k, "_", yearSpan, ".tif") # the original rsds data
  
  temp_r <- rast(fileName_temp)
  hurs_r <- rast(fileName_hurs)
  wind_r <- rast(fileName_sfcwind)
  if (useRSDS == TRUE) {
    solar_r <- rast(fileName_rsds)
  } else {
    solar_r <- get(paste0("r_zeros_", l))
    names(solar_r) <- names(temp_r)
  }
  
  for (yearNum in l:(l + yearRange)) {
    outf <- paste0(fileDestination, "Tg_out", "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", "y_", yearNum, ".tif")
   # if (!outf %in% filesCompleted) {
      startDate <- paste0(yearNum, "-01-01")
      endDate <- paste0(yearNum, "-12-31")
      print(startDate)
      print(endDate)
      indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
      indicesChar <- paste0("X", indices)
      temp_yr <- subset(temp_r, indicesChar)
      
      hurs_yr <- subset(hurs_r, indicesChar)
      wind_yr <- subset(wind_r, indicesChar)
      solar_yr <- subset(solar_r, indicesChar)
      lat_yr <- latlyrs_365
      if (nlyr(temp_yr) == 366) lat_yr <- latlyrs_366
      year_yr <- rast(nrows = 360, ncols = 720)
      year_yr <- f_year(nlyr(temp_yr), l)
      doy_yr <- doy_yr_365
      if (nlyr(temp_yr) == 366) doy_yr <- doy_yr_366
      system.time(s2 <- sds(temp_yr, hurs_yr, wind_yr, solar_yr, lat_yr, year_yr, doy_yr))
      print(paste(nlyr(temp_yr), nlyr(hurs_yr), nlyr(wind_yr), nlyr(solar_yr), nlyr(lat_yr), nlyr(year_yr), nlyr(doy_yr)))
      names(s2) <- c("temp", "hurs", "wind", "srad", "lat", "year", "doy")
      fout <- outf
      tstart <- Sys.time()
      Tg <- lapp(s2, fun = Tg_out, tolerance = 0.1, optim = optimizeChoice, output = "Tg_out", filename = fout, overwrite = TRUE, wopt = woptList)
      speed <- Sys.time() - tstart
      print(speed)
      print(paste0("filename out: ", outf))
    }
  #}
}

f_pwc_wbgt_utci <- function(k, l, modelChoice, outChoice, optimizeChoice, tempChoice) {
  # filesCompleted <- list.files(fileDestination, full.names = TRUE)
  # filesCompleted <- filesCompleted[!grepl("aux.xml", filesCompleted, fixed = TRUE)]
  # filesCompleted <- gsub("//", "/", filesCompleted)
  modelChoice_lower <- tolower(modelChoice)
  startDate_period <- paste0(l, "-01-01")
  endDate_period <- paste0(l + yearRange, "-12-31")
  dates_period <- seq(as.Date(startDate_period), as.Date(endDate_period), 1)
  yearSpan <- paste0(l, "_", l + yearRange)
  #  do year specific setup
  tempComponent <- paste0("_", tempChoice, "_")
  fileName_temp <- paste0(path_clim_processed, modelChoice_lower, tempComponent, k, "_", yearSpan, ".tif")
  fileName_hurs <- paste0(path_clim_processed, modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
  fileName_sfcwind <- paste0(path_clim_processed, modelChoice_lower, "_sfcwind_", k, "_", yearSpan, ".tif")
  fileName_rsds <- paste0(path_clim_processed, modelChoice_lower, "_rsds_", k, "_", yearSpan, ".tif") # the original rsds data
  fileName_Tg <- paste0(path_clim_processed, "Tg_out_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
  
  temp_r <- rast(fileName_temp)
  hurs_r <- rast(fileName_hurs)
  wind_r <- rast(fileName_sfcwind)
  Tg_r <- rast(fileName_Tg)
  if (useRSDS == TRUE) {
    solar_r <- rast(fileName_rsds)
  } else {
    solar_r <- get(paste0("r_zeros_", l))
    names(solar_r) <- names(temp_r)
  }
  names(Tg_r) <- names(temp_r)
  for (yearNum in l:(l + yearRange)) {
    outf_pwc_adj <- paste0(fileDestination, outChoice, "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", "y_", yearNum, ".tif")
   # if (!outf_pwc_adj %in% filesCompleted) {
      startDate <- paste0(yearNum, "-01-01")
      endDate <- paste0(yearNum, "-12-31")
      print(startDate)
      print(endDate)
      indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
      indicesChar <- paste0("X", indices)
      temp_yr <- subset(temp_r, indicesChar)
      hurs_yr <- subset(hurs_r, indicesChar)
      wind_yr <- subset(wind_r, indicesChar)
      solar_yr <- subset(solar_r, indicesChar)
      Tg_yr <- subset(Tg_r, indicesChar)
      lat_yr <- latlyrs_365
      if (nlyr(temp_yr) == 366) lat_yr <- latlyrs_366
      year_yr <- f_year(nlyr(temp_yr), l)
      doy_yr <- doy_yr_365
      if (nlyr(temp_yr) == 366) doy_yr <- doy_yr_366
      names(year_yr) <- names(doy_yr) <- names(lat_yr) <- names(temp_yr)
      dates_period <- seq(as.Date(startDate), as.Date(endDate), 1)
      
      system.time(s2 <- sds(temp_yr, hurs_yr, wind_yr, solar_yr, Tg_yr, lat_yr, year_yr, doy_yr))
      names(s2) <- c("temp", "hurs", "wind", "srad", "Tg", "lat", "year", "doy")
      fout <- outf_pwc_adj
      tstart <- Sys.time()
      pwc <- lapp(s2, fun = pwc_lapp, tolerance = 0.1, optim = optimizeChoice, output = outChoice, filename = fout, overwrite = TRUE, wopt = woptList)
      speed <- Sys.time() - tstart
      print(speed)
      print(fout)
      print(pwc)
      gc()
    }
  }
#}

f_means <- function(k, l, modelChoice, outChoice) {
  yearSpan <- paste0(l, "_", l + yearRange)
  # modelChoices <- c("MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL", "GFDL-ESM4", "IPSL-CM6A-LR")
  modelChoice_lower <- tolower(modelChoice)
  startDate <- paste0(l, "-01-01")
  endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices_date <- paste0("X", as.character(indices))
  indices_day <- as.numeric(format(indices, format = "%j"))
  infile_pwc_adj <- paste0(fileDestination, outChoice, "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
  outf_pwc_adj_daily_mean <- paste0(fileDestination, outChoice, "_daily_mean", "_", modelChoice_lower, "_", k, "_", yearSpan, ".tif")
  r_in <- rast(infile_pwc_adj)
  print(system.time(r_day <- tapp(r_in, indices_day, fun = "mean", na.rm = TRUE, filename = outf_pwc_adj_daily_mean, overwrite = TRUE, wopt = woptList)))
  print(r_day)
}

f_readRast_ensemble <- function(modelChoice, k, l, outChoice) {
  modelChoice_lower <- tolower(modelChoice)
  yearSpan <- paste0(l, "_", l + yearRange)
  infile <- paste0(fileDestination, outChoice, "_daily_mean", "_", modelChoice_lower, "_", k, "_", yearSpan, ".tif")
  r <- rast(infile)
  indices <- seq(from = 1, to = nlyr(r), 1)
  indices <- paste0("X", as.character(indices))
  names(r) <- indices
  r
}


latlyrs_365 <- f_latlyrs(ndays = 365)
latlyrs_366 <- f_latlyrs(ndays = 366)
doy_yr_365 <- f_doy(ndays = 365)
doy_yr_366 <- f_doy(ndays = 366)

#  test data
l <- 2041
k <- "ssp126"
modelChoice <- "GFDL-ESM4"
modelChoice_lower <- tolower(modelChoice)
yearSpan <- paste0(l, "_", l + yearRange)
yearNum <- l + 16

# Tg files generation -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      f_Tgcalc(k, l, modelChoice, useRSDS)
    }
  }
  k <- "historical"
  l <- 1991
  f_Tgcalc(k, l, modelChoice, useRSDS)
}

# combine Tg years -----
# scenarios
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      r_out <- f_combineYears(k, l, modelChoice, "Tg_out", optimizeChoice)
    }
  }
  # historical
  k <- "historical"
  l <- 1991
  r_out <- f_combineYears(k, l, modelChoice, "Tg_out", optimizeChoice)
}

# yearly calculation of wbgt or utci version of pwc or wbgt or utci out -----
# scenarios
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      f_pwc_wbgt_utci(k, l, modelChoice, outChoice, optimizeChoice, tempChoice)
      print("----------------------------------------------")
    }
  }
  # historical
  k <- "historical"
  l <- 1991
  f_pwc_wbgt_utci(k, l, modelChoice, outChoice, optimizeChoice, tempChoice)
  print("----------------------------------------------")
}

# combine years, outChoice determines which one -----
# scenarios
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      r_out <- f_combineYears(k, l, modelChoice, outChoice, optimizeChoice)
    }
  }
  # historical
  k <- "historical"
  l <- 1991
  r_out <- f_combineYears(k, l, modelChoice, outChoice, optimizeChoice)
}

# calculate period mean for each model -----
# scenarios
for (modelChoice in modelChoices) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      f_means(k, l, modelChoice, outChoice)
    }
  }
}
# historical
k <- "historical"
l <- 1991
for (modelChoice in modelChoices) {
  f_means(k, l, modelChoice, outChoice)
}

# combine across models -----
{
  for (k in sspChoices) {
    for (l in startYearChoices) {
      # scenarios
      yearSpan <- paste0(l, "_", l + yearRange)
      x <- lapply(modelChoices, f_readRast_ensemble, k, l, outChoice)
      r <- rast(x)
      indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
      outf <- paste0(fileDestination, "ensemble_", outChoice, "_daily_mean", "_", k, "_", yearSpan, ".tif")
      print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = outf, overwrite = TRUE, wopt = woptList)))
    }
  }
  # historical
  k <- "historical"
  l <- 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  x <- lapply(modelChoices, f_readRast_ensemble, k, l, outChoice)
  r <- rast(x)
  indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  outf <- paste0(fileDestination, "ensemble_", outChoice, "_daily_mean", "_", k, "_", yearSpan, ".tif")
  print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = outf, overwrite = TRUE, wopt = woptList)))
}
# 
# # calculate 1 hr pwc using different equations, experimental -----
# # do for just one year
# k <- "historical"
# l <- 1991
# yearSpan <- paste0(l, "_", l + yearRange)
# modelChoice <- "MRI-ESM2-0"
# modelChoice_lower <- tolower(modelChoice)
# startDate <- paste0(l, "-01-01")
# endDate_period <- paste0(l + yearRange, "-12-31")
# dates <- seq(as.Date(startDate), as.Date(endDate_period), 1)
# indices <- seq(as.Date(startDate), as.Date(paste0(l + yearRange, "-12-31")), by = "days")
# indicesChar <- paste0("X", indices)
# 
# fileName_temp <- paste0(path_clim_processed, modelChoice_lower, "_temp_", k, "_", yearSpan, ".tif")
# fileName_hurs <- paste0(path_clim_processed, modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
# fileName_sfcwind <- paste0(path_clim_processed, modelChoice_lower, "_sfcwind_", k, "_", yearSpan, ".tif")
# fileName_rsds <- paste0(path_clim_processed, modelChoice_lower, "_rsds_hr_", k, "_", yearSpan, ".tif") # new rsds data adjusted for day length, already cropped
# fileName_Tg <- paste0(path_clim_processed, "Tg_out_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
# 
# fileName_utci <- paste0(fileDestination, "utci_out", "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
# fileName_wbgt <- paste0(fileDestination, "wbgt_out", "_", modelChoice_lower, "_", optimizeChoice, "_", k, "_", yearSpan, ".tif")
# hurs <- rast(fileName_hurs)
# temp <- rast(fileName_temp)
# if (useRSDS == TRUE) {
#   solar_r <- rast(fileName_rsds)
# } else {
#   solar_r <- get(paste0("r_zeros_", l))
#   names(solar_r) <- names(temp_r)
# }
# wbgt <- rast(fileName_wbgt)
# names(wbgt) <- indicesChar
# 
# startDate_yr <- paste0(l, "-01-01")
# endDate_yr <- paste0(l, "-12-31")
# dates_yr <- seq(as.Date(startDate_yr), as.Date(endDate_yr), 1)
# indices_yr <- seq(as.Date(startDate_yr), as.Date(paste0(l, "-12-31")), by = "days")
# indicesChar_yr <- paste0("X", indices_yr)
# temp_yr <- subset(temp, indicesChar_yr)
# hurs_yr <- subset(hurs, indicesChar_yr)
# utci_yr <- subset(utci, indicesChar_yr)
# wbgt_yr <- subset(wbgt, indicesChar_yr)
# 
# f_pwc_utci <- function(utci) {
#   100. / (1. + ((45.33 / utci)^-4.30))
# }
# f_pwc_wbgt <- function(wbgt) {
#   100. / (1. + ((33.63 / wbgt)^-6.33))
# }
# f_pwc_orig <- function(temp, hurs) {
#   pwc <- 100 / (1 + (((-12.28 * log(hurs) + 87.99) / temp))^(-2.21 * log(hurs) + 2.63))
#   pwc[temp < 12.] <- 100
#   return(pwc)
# }
# 
# sds_orig <- sds(temp_yr, hurs_yr)
# 
# system.time(pwc_utci_yr <- app(utci_yr, f_pwc_utci))
# system.time(pwc_wbgt_yr <- app(wbgt_yr, f_pwc_wbgt))
# system.time(pwc_1hr <- lapp(sds_orig, f_pwc_orig))
# # system.time(pwc_orig_yr <- lapp(sds_orig, fun=pwc_1hr))
# 
# plot(pwc_utci_yr, 1, main = "pwc_utci_yr, Jan 1")
# plot(pwc_wbgt_yr, 1, main = "pwc_wbgt_yr, Jan 1")
# plot(pwc_1hr, 1, main = "pwc_1hr, Jan 1")
# 
# plot(utci_yr, 1, main = "utci_yr, Jan 1")
# plot(wbgt_yr, 1, main = "wbgt_yr, Jan 1")