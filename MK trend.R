# install.packages("raster")
# install.packages("sp")
# install.packages("rgdal")
# install.packages("terra")
# install.packages("trend")
library(sp)
library(raster)
library(rgdal)
library(trend)
library(terra)

setwd('G:/phd/phd��������/�����ȵ�/Global SHUII 2003��2018/yearly/day')
#����һ���ļ����ڵĵ�����TIFF���ݣ��������������NDVImean
fl <- list.files(pattern = '.tif$')
firs <- raster(fl[1])
for (i in 2:16) {
  r <- raster(fl[i])
  firs <- stack(firs, r)
}

fun <- function(y){
  if(length(na.omit(y)) <16) return(c(NA, NA, NA))   #ɾ�����ݲ���������NA����Ԫ
  MK_estimate <- sens.slope(ts(na.omit(y), start = 2003, end = 2018, frequency = 1), conf.level = 0.95) #Senб�ʹ���
  slope <- MK_estimate$estimate
  MK_test <- MK_estimate$p.value
  Zs <- MK_estimate$statistic
  return(c(Zs, slope, MK_test))
}


e <- calc(firs, fun)   #դ�����
e_Zs <- subset(e,1)  #��ȡ��ֵͼ��
e_slope <- subset(e,2)   #��ȡsenб�� 
e_MKtest <- subset(e,3)   #��ȡpֵ

writeRaster(e_Zs, "e_Zs.tif", format="GTiff", overwrite=T)
writeRaster(e_slope, "e_slope.tif", format="GTiff", overwrite=T)
writeRaster(e_MKtest, "e_MKtest.tif", format="GTiff", overwrite=T)