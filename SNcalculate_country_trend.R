#library(ggmap)
#install.packages("raster")
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(data.table)

tempfile(tmpdir="/mnt/temp/yz2875")
#################
# library(terra)
# aa = rast('/mnt/work/YY/summer/day/firs_sen.tif')
# p = aa[[3]]
# s = aa[[2]]
# writeRaster(s, filename = "/mnt/work/YY/summer/day/slope.tif")
# writeRaster(p, filename = "/mnt/work/YY/summer/day/pvalue.tif")
############

#terraOptions(tempdir="/mnt/temp/yz2875")

df = raster('/mnt/work/YY/summer/night/pvalue.tif')
df_s = raster('/mnt/work/YY/summer/night/slope.tif')


p.hit = as.data.frame(df, na.rm = TRUE, xy = TRUE)

# p.col = xFromCol(df)
# p.row = yFromRow(df)
# p.cell = expand.grid(p.col, p.row,stringsAsFactors=FALSE)
# p.cell$z = values(df)
# 
# p.hit = p.cell[!is.na(p.cell$z),]
names(p.hit) = c("pvalue","lon","lat")

coordinates(p.hit) = c("lon", "lat")
proj4string(p.hit) = proj4string(df)

# slope.col = xFromCol(df_s)
# slope.row = yFromRow(df_s)
# slope.cell = expand.grid(slope.col, slope.row)
# slope.cell$z = values(df_s)
# slope.hit = slope.cell[!is.na(slope.cell$z),]
slope.hit = as.data.frame(df_s, na.rm = TRUE, xy = TRUE)
names(slope.hit) = c("slope","lon","lat")
coordinates(slope.hit) = c("lon", "lat")
proj4string(slope.hit) = proj4string(df_s) #using p's coordination




bb.shape = readOGR("/mnt/work/YY/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp")


is.inp <- over(p.hit, bb.shape)
is.ins <- over(slope.hit, bb.shape)

is.inp <- as.data.frame(is.inp)
is.ins <- as.data.frame(is.ins)

ad.mergep = cbind(is.inp, p.hit,row.names = NULL)
write.csv(ad.mergep,file = "/mnt/work/YY/summer/night/SN_p.csv")

ad.merges = cbind(is.ins, slope.hit,row.names = NULL)
write.csv(ad.merges,file = "/mnt/work/YY/summer/night/SN_s.csv")



ad.mergep =as.data.table(ad.mergep)
ad.merges =as.data.table(ad.merges)


ad.mergep = ad.mergep[,c(9,14,15,23,31,54,55,56)]
ad.merges = ad.merges[,c(54,55,56)]

ad.mergep = unique(ad.mergep)
ad.merges = unique(ad.merges)

last.df = cbind(ad.mergep,ad.merges,row.names = NULL)

write.csv(last.df,file = "/mnt/work/YY/summer/night/SN.csv")




a = read.csv("/mnt/work/YY/summer/night/SN.csv")  



b<- a %>%
  filter(pvalue<0.05 & slope >0) %>%
  group_by(Freq.ISO_A3) %>%
  summarise(
    n_1=n()
  )


c<- a %>%
  group_by(Freq.ISO_A3) %>%
  summarise(
    n=n()
  ) %>%
  left_join(b,by="Freq.ISO_A3") %>%
  mutate(
    
    per=n_1*100/n
  )

c1 =a[,c("Freq.FORMAL_EN","Freq.ISO_A3")]
c1 = unique(c1)

d<- c1 %>%
  left_join(c,by="Freq.ISO_A3")
  

names(d) = c("country","ISO","total","trendN","percent")
d <- na.omit(d)

write.csv(d,file = "/mnt/work/YY/summer/night/SN_per.csv")
