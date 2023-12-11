require(raster)
belize_dem <- raster("./3_DEMs/Belize.tif")
plot(belize_dem)

require(sf)
boundary <- read_sf("studyareaboundary.shp")

belize_dem_crop <- crop(belize_dem, boundary)

plot(belize_dem_crop)
plot(boundary$geometry, bg="transparent", add=TRUE)

writeRaster(belize_dem_crop, filename = "./belize_dem_crop.tif", format="GTiff")

belize_dem_new <- raster("./belize_dem_crop.tif")
plot(belize_dem_new)
