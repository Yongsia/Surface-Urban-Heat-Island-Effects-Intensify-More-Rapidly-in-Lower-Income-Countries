
library(sp)
library(raster)
library(rgdal)
library(terra)


setwd('E:/phd/UHI/趋势分析')
slope = rast("e_slope.tif")
Zs = rast("e_Zs.tif")
#重分类，<0 negative； >0 positive
m = c(-0.4505771, 0, -1,
      0, 0, 0,
      0, 0.484252, 1)
rclmat = matrix(m, ncol = 3, byrow = T)
slope_rcl = classify(slope, rclmat, include.lowest=T, right=F)
writeRaster(slope_rcl, filename ="slope_rcl.tif", filetype="GeoTiff")



m2 = c(-5.357679, -1.96, 2,
      -1.96, 1.96, 1,
      1.96, 5.267634, 2)
rclmat2 = matrix(m2, ncol = 3, byrow = T)
Zs_rcl = classify(Zs, rclmat2, include.lowest=T, right=F)


RCL= slope_rcl * Zs_rcl

writeRaster(RCL, "reclassify.tif", format="GTiff", overwrite=T)