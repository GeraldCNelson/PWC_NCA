# PWC_NCA package
Code to create the graphic for the National Climate Assessment Figure 28.7.

The code included in this repository is designed to make it possible to create Figure 28.7 in the 2023 US National Climate Assessment. This is not an easy task. Each of the data files is about 1 GB. The ISIMIP project [https://www.isimip.org] prepares daily bias-corrected 1/2 degree resolution from five earth system models (ESMs - GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR). The paper uses the ISIMIP3b data from 
[https://doi.org/10.48364/ISIMIP.842396.1](https://doi.org/10.48364/ISIMIP.842396.1). The data are downloaded as netcdf files with 10 years of data. These files were combined to create 3 20-year periods for each ESM - 1991-2010, 2041-2060, and 208-2100 - and cropped to show data only for land areas. Each of the needed _.nc_ can be accessed using the urls in the csv files in the zip file _NCA Fig 28.7-rawdata_links.zip"_. A land-only mask is available - _data-raw/landseamask_no_antarctica.nc_.