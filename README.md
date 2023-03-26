# PWC_NCA Github Repository - R code to create the graphic for the National Climate Assessment Figure 28.7.

The code in this repository is designed to make it possible to create Figure 28.7 in the 2023 US National Climate Assessment. 

The process involves the following steps

* download climate data files from the ISIMIP3b [https://www.isimip.org](https://www.isimip.org) data set

* run R code (R/combineRasters.R) that prepares the climate data for analysis

* run R code (R/final_wbgt_clean.R) that generates the ensemble results files

* run R code (R/pwc_graphics_southwestUS.R) that generates the graphics in pdf format.

## Install needed R packages

The following R packages are used - terra, geodata, ggplot2, data.table, Rcpp. Use this code to install them all at one time.
install.packages(c("terra", "geodata", "ggplot2", "data.table", "Rcpp", "sf"))

## Download climate data files

This is not a speedy task and requires well over 100 GB of storage. Each of the data files is about 1 GB so the download process will take some time. Each of the needed _.nc_ files can be accessed using the urls in the csv files in the zip file _NCA Fig 28.7-rawdata_links.zip"_ in the _data.raw_ directory. The _.nc_ files should be put into the _climdata_ directory. The earth system models (ESMs) (acronyms are UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, GFDL-ESM4, IPSL-CM6A-LR)- were chosen by the ISIMIP team to reflect a range of climate sensitivites.

## Prepare data for analysis
This step is done with the R code R/combineRasters.R. The data are downloaded as netcdf files with 10 years of data. These files are combined to create 3 20-year periods for each of 5 ESMs - 1991-2010, 2041-2060, and 208-2100 - and cropped to show data only for land areas using the land-only mask - _data-raw/landseamask_no_antarctica.nc_. The code writes these files to the _climatedata_processed_ directory (and creates the directory if it is not available.)

# Process the data for use in the graphics code
This is done with the R code R/final_wbgt_clean.R. The final output of this process are files with names like _ensemble_pwc_wbgt_out_daily_mean_historical_1991_2010.tif_. These are daily PWC values for a representative year in the 20 year period 1991-2010. Each daily value is the average of the value from each of the ESMs.

## Generate the graphics
This is done with R/pwcGraphics.R code. The graphics are written to the _graphics_ directory.
