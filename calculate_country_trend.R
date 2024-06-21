#library(ggmap)
#install.packages("raster")
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(data.table)


#writeRaster(s, filename = "/mnt/work/YY/summer/night/slope.tif")

terraOptions(tempdir="/mnt/temp/yz2875")
#aa = rast('/mnt/work/YY/summer/night/firs_sen.tif')
df = raster('/mnt/work/YY/summer/night/pvalue.tif')
df_s = raster('/mnt/work/YY/summer/night/slope.tif')

p.col = xFromCol(df)
p.row = yFromRow(df)
p.cell = expand.grid(p.col, p.row)
p.cell$z = values(df)

p.hit = p.cell[!is.na(p.cell$z),]
names(p.hit) = c("lon","lat","pvalue")

coordinates(p.hit) = c("lon", "lat")
proj4string(p.hit) = proj4string(df)

slope.col = xFromCol(df_s)
slope.row = yFromRow(df_s)
slope.cell = expand.grid(slope.col, slope.row)
slope.cell$z = values(df_s)
slope.hit = slope.cell[!is.na(slope.cell$z),]
names(slope.hit) = c("lon","lat","slope")
coordinates(slope.hit) = c("lon", "lat")
proj4string(slope.hit) = proj4string(df_s) #using p's coordination

bb.shape = readOGR("/mnt/work/YY/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp")


is.inp <- over(p.hit, bb.shape)
is.ins <- over(slope.hit, bb.shape)


is.inp <- as.data.frame.table(is.inp)
is.ins <- as.data.frame.table(is.ins)

ad.mergep = cbind(is.inp, p.hit)
ad.merges = cbind(is.ins, slope.hit)

ad.mergep =as.data.table(ad.mergep)
ad.merges =as.data.table(ad.merges)


ad.merge.p = ad.mergep[,c(7,10,16,19,25,55,56,57,58)]
ad.merge.s = ad.merges[,c(55,56,57)]

last.dfp = unique(ad.merge.p)
last.dfs = unique(ad.merge.s)

last.df = cbind(last.dfs,last.dfp)

write.csv(last.df,file = "/mnt/work/YY/summer/night/df.csv")




a <- last.df




b<- a %>%
  filter(pvalue<0.05 & slope >0) %>%
  group_by(Freq.ISO_A3...6) %>%
  summarise(
    n_1=n()
  )

c<- a %>%
  group_by(Freq.ISO_A3...6) %>%
  summarise(
    n=n()
  ) %>%
  left_join(b,by="Freq.ISO_A3...6") %>%
  mutate(
    
    per=n_1*100/n
  )


write.csv(c,file = "/mnt/work/YY/summer/night/c.csv")
