# combine 10 year rasters to get 20 year rasters and convert units where needed
# on my macbook pro M1 version it takes about 2 1/2 minutes to run one ssp, start year, climate variable, model combination

library(terra)
library(meteor)
woptList <- list(gdal = c("COMPRESS = DEFLATE", "PREDICTOR = 2", "ZLEVEL = 6", "NUM_THREADS = ALL_CPUS"))
terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/tempFiles", verbose = TRUE) 
dir.create("data/tempFiles", F, F)

ext_globe <- ext(-180, 180, -60, 90)
mask_land_sea <- rast("data-raw/landseamask_no_antarctica.nc")
locOfFiles <- "climdata"
dir.create(locOfFiles, F, F)
locOfFiles_out <- "climdata_processed/"
dir.create(locOfFiles_out, F, F)
climateVars <- c("_tasmin_", "_tasmax_", "_tas_", "_hurs_",  "_pr_", "_hurs_", "_rsds_", "_sfcwind_") 
sspChoices <- c("ssp585", "ssp126") 
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
yearRange <- 19

#test data
modelChoice.lower <- "gfdl-esm4"
startYearChoice = 2041
climateVar <- "_rsds_"
sspChoice <- "ssp126"

# code to adjust rsds. Need latitude values
r_365 <- rast(res=0.5, ymin=-60, nlyr=365)
lat <- yFromRow(r_365, 1:nrow(r_365))
d <- sapply(1:365, function(i) photoperiod(i, lat))
values(r_365) <- rep(d, each=ncol(r_365))
r_365[r_365 == 0] <- NA # convert locations and dates with no daylight to NA
r_366 <- rast(res=0.5, ymin=-60, nlyr=366)
lat <- yFromRow(r_366, 1:nrow(r_366))
d <- sapply(1:366, function(i) photoperiod(i, lat))
values(r_366) <- rep(d, each=ncol(r_366))
r_366[r_366 == 0] <- NA # convert locations and dates with no daylight to NA

f_comb <- function(startYearChoice, modelChoice.lower, climateVar, sspChoice) {
  yearSpan <- paste0(startYearChoice, "_", startYearChoice + 19)
  outf <- paste0(locOfFiles_out, modelChoice.lower, climateVar, sspChoice, "_", yearSpan, ".tif")
  if (outf %in% list.files("climdata_processed", full.names = TRUE)) (return())
  pattern1 <- paste0(modelChoice.lower, ".*", sspChoice, ".*", climateVar, ".*",  startYearChoice)
  pattern2 <- paste0(modelChoice.lower, ".*", sspChoice, ".*", climateVar, ".*",  startYearChoice + 10)
  ff1 <- list.files(locOfFiles, pattern = pattern1, full.names = TRUE)
  noFiles1 <- paste0("pattern ", pattern1, " finds nothing.")
  if (identical(ff1, character(0))) stop(noFiles1) 
  ff2 <- list.files(locOfFiles, pattern = pattern2, full.names = TRUE)
  noFiles2 <- paste0("pattern 2 ", pattern2, " finds nothing.")
  if (identical(ff2, character(0))) stop(noFiles2) 
  r1 <- rast(ff1) ; r2 <- rast(ff2) 
  comb <- c(r1, r2) |> mask(mask_land_sea) |> crop(ext_globe)
  startDate <- paste0(startYearChoice, "-01-01"); endDate <- paste0(startYearChoice + 19, "-12-31")
  names(comb) <- paste0("X", as.character(seq(as.Date(startDate), as.Date(endDate), 1)))
  if (climateVar %in% c("_tas_", "_tasmin_", "_tasmax_")) comb <- comb - 273.15 # convert from kelvin to C
  if (climateVar == "_pr_") comb <- comb * 86400 # convert precip from kg m^-2 s^-1 to mm per day
  id (climateVar == "_rsds_") {
    # convert average daily rsds to total daily rsds and then divide by hours in the day to get daylight hours average rsds
    holder <- c()
    rsds_tot <- comb * 24
    endYear <- startYearChoice + yearRange
    for (i in startYearChoice:endYear) {
      startDate <- paste0(i, "-01-01"); endDate <- paste0(i, "-12-31") 
      dates <- paste0("X", seq(as.Date(startDate), as.Date(endDate), 1))
      yearSubset <- subset(rsds_tot, dates)
      if (lubridate::leap_year(i)) {
        rad_hourly <- yearSubset/r_366
      }else{
        rad_hourly <- yearSubset/r_365
      }
      holder <- c(holder, rad_hourly)
    }
    comb <- rast(holder)
  }
  print(outf)
  writeRaster(comb, outf, overwrite = TRUE, wopt = woptList); flush.console()
}

for (sspChoice in sspChoices) {
  for (startYearChoice in startYearChoices) {
    for (climateVar in climateVars) {
      for (modelChoice.lower in modelChoices.lower) {
        system.time(f_comb(startYearChoice, modelChoice.lower, climateVar, sspChoice))
      }
    }
  }
}

sspChoice <- "historical" 
startYearChoice <- 1991
for (climateVar in climateVars) {
  for (modelChoice.lower in modelChoices.lower) {
    system.time(f_comb(startYearChoice, modelChoice.lower, climateVar, sspChoice))
  }
}



