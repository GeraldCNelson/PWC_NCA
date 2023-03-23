# combine 10 year rasters to get 20 year rasters and convert units where needed
# on my macbook pro M1 version it takes about 2 1/2 minutes to run one ssp, start year, climate variable, model combination

library(terra)
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
climateVarstoKeep <- c("_tasmax_") # "tasmax", "tasmin"
sspChoices <- c("ssp585", "ssp126") 
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)

#test data
modelChoice.lower <- "mpi-esm1-2-hr"
startYear = 2041
climVar <- "_tas_"
sspChoice <- "ssp126"

f_comb <- function(startYear, modelChoice.lower, climVar, sspChoice) {
  pattern1 <- paste0(modelChoice.lower, ".*", sspChoice, ".*", climVar, ".*",  startYear)
  pattern2 <- paste0(modelChoice.lower, ".*", sspChoice, ".*", climVar, ".*",  startYear + 10)
  ff1 <- list.files(locOfFiles, pattern = pattern1, full.names = TRUE)
  noFiles1 <- paste0("pattern ", pattern1, " finds nothing.")
  if (identical(ff1, character(0))) stop(noFiles1) 
  ff2 <- list.files(locOfFiles, pattern = pattern2, full.names = TRUE)
  noFiles2 <- paste0("pattern 2 ", pattern2, " finds nothing.")
  if (identical(ff2, character(0))) stop(noFiles2) 
  r1 <- rast(ff1) ; r2 <- rast(ff2) 
  comb <- c(r1, r2) |> mask(mask_land_sea) |> crop(ext_globe)
  startDate <- paste0(startYear, "-01-01"); endDate <- paste0(startYear + 19, "-12-31")
  names(comb) <- paste0("X", as.character(seq(as.Date(startDate), as.Date(endDate), 1)))
  if (climVar %in% c("_tas_", "_tasmin_", "_tasmax_")) comb <- comb - 273.15 # convert from kelvin to C
  if (climVar == "_pr_") comb <- comb * 86400 # convert precip from kg m^-2 s^-1 to mm per day
  yearSpan <- paste0(startYear, "_", startYear + 19)
  outf <- paste0(locOfFiles_out, modelChoice.lower, climVar, sspChoice, "_", yearSpan, ".tif")
  print(outf)
  writeRaster(comb, outf, overwrite = TRUE, wopt = woptList); flush.console()
}

for (sspChoice in sspChoices) {
  for (climVar in climVars) {
    for (modelChoice.lower in modelChoices.lower) {
      for (startYear in startYears) {
        system.time(f_comb(startYear, modelChoice.lower, climVar, sspChoice))
      }
    }
  }
}

sspChoice <- "historical" 
startYear <- 1991
for (climVar in climVars) {
  for (modelChoice.lower in modelChoices.lower) {
    system.time(f_comb(startYear, modelChoice.lower, climVar, sspChoice))
  }
}



