# frequently used spatial data
 extent_noAntarctica <- ext(-180, 180, -60, 90) #-60 gets rid of Antarctica for global
# RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# crsRob <- RobinsonProj
# RobinsonWorld <- "EPSG:54030"
# albersEqualAreaConic_USwest <- "EPSG:5070"
albersEqualAreaConic_USwest <- "EPSG:102003"
albersEqualAreaConic_USwestNew <- "ESRI:102008"

proj_to_use <- albersEqualAreaConic_USwestNew
sf::sf_use_s2(FALSE) # useful for making sf crop work. See https://r-spatial.github.io/sf/articles/sf7.html

# data from https://www.naturalearthdata.com. From the website
# No permission is needed to use Natural Earth. Crediting the authors is unnecessary.
# However, if you wish to cite the map data, simply use one of the following.
# Short text:
#   Made with Natural Earth.
# Long text: 
#   Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.

coastline <- vect("data-raw/ne_50m_coastline/ne_50m_coastline.shp", extent = extent_noAntarctica)
coastline_sf <- sf::st_read("data-raw/ne_50m_coastline/ne_50m_coastline.shp")
countries <- vect("data-raw/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
states_provinces <- vect("data-raw/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
states_provinces_sf <- sf::st_read("data-raw/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")

#countries_gadm <- world(path = "data-raw/gadm/") # will read it in and write to the path if not already there

# country_US <- gadm(country = "USA", level = 1, path = "data-raw/gadm/", resolution = 2)
# states_abbr_SW <- c("CA", "AZ", "NM", "CO", "NV", "UT")
# states_SW <- subset(country_US,country_US$HASC_1 %in% paste0("US.", states_abbr_SW))

#globe <- crop(countries_gadm, extent_noAntarctica)

# quarter information
# dayCt_quarters <- c(90, 91, 92, 92) # days in each nonleap year quarter - https://www.google.com/search?client=safari&rls=en&q=number+of+days+in+a+quarter&ie=UTF-8&oe=UTF-8
# qnum <- as.numeric(gsub("q", "", cropChoice))
# maturity_days <- cumsum(dayCt_quarters)
# plant_days <- maturity_days - dayCt_quarters + 1
# gsl <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = dayCt_quarters[qnum])
# plant <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = plant_days[qnum])
# maturity <- rast(ncol = 300, nrow = 720, xmin = -180, xmax = 180, ymin = -60, ymax = 90, res = 0.5, vals = maturity_days[qnum])

cropMask_globe <- rast("data/crop_mask/rasterMask_noCrops_175.tif") # locations with no crops = 0
# aggregate to 1/2 degree
cropMask_globe <- aggregate(cropMask_globe, fact = 6, fun = "mean", na.rm = FALSE)
minCt = .5 # at least .5 of the pixels in the aggregated pixel have at least one crop grown there
cropMask_globe[cropMask_globe < minCt] <- NA
cropMask_globe[cropMask_globe >= minCt] <- 1 
#cropMask_globe[cropMask_globe == 0] <- NA 

