# PWC_NCA Github Repository - R code to create the graphic for the National Climate Assessment Figure 28.7.

The code  in this repository is designed to make it possible to create Figure 28.7 in the 2023 US National Climate Assessment. 

The process involves the following steps

* download climate data files from the ISIMIP3b [https://www.isimip.org](https://www.isimip.org) data set

* run R code (R/combineRasters.R) that prepares the climate data for analysis

* run R code (R/final_wbgt_clean.R) that generates the ensemble results files

* run R code (R/pwcGraphics.R) that generates the graphics in pdf format.

## Download climate data files

This is not a speedy task and requires well over 100 GB of storage. Each of the data files is about 1 GB so the download process will take some time. Each of the needed _.nc_ files can be accessed using the urls in the csv files in the zip file _NCA Fig 28.7-rawdata_links.zip"_. These should be put into the climdata directory. 

# Prepare data for analysis
The data are downloaded as netcdf files with 10 years of data. These files were combined to create 3 20-year periods for each earth system model (ESM) - 1991-2010, 2041-2060, and 208-2100 - and cropped to show data only for land areas using the land-only mask - _data-raw/landseamask_no_antarctica.nc_ - 
