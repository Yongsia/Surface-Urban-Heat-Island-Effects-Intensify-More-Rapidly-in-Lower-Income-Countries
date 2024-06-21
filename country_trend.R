
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(data.table)

ad.mergep = read.csv("/mnt/work/YY/summer/night/SN_p.csv")

ad.merges = read.csv("/mnt/work/YY/summer/night/SN_s.csv")

ad.mergep =as.data.table(ad.mergep)
ad.merges =as.data.table(ad.merges)

ad.mergep = ad.mergep[,c(9,14,15,23,31,54,55,56)]
ad.merges = ad.merges[,c(54,55,56)]

ad.mergep = unique(ad.mergep)
ad.merges = unique(ad.merges)

last.df = merge(ad.mergep,ad.merges,by = c("lon","lat"))
write.csv(last.df,file = "/mnt/work/YY/summer/night/SN.csv")

a = last.df


#--------------


###increasing trend
b<- a %>%
  filter(pvalue<0.05 & slope >0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_1=n()
  )

b2<- a %>%
  filter(pvalue<0.05 & slope ==0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_2=n()
  )
b3<- a %>%
  filter(pvalue<0.05 & slope <0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_3=n()
  )

b4<- a %>%
  filter(pvalue >= 0.05) %>%
  group_by(WB_A3) %>%
  summarise(
    n_4=n()
  )

c<- a %>%
  group_by(WB_A3) %>%
  summarise(
    n=n()
  ) %>%
  left_join(b,by="WB_A3") %>%
  left_join(b2,by="WB_A3") %>%
  left_join(b3,by="WB_A3") %>%
  left_join(b4,by="WB_A3") %>%
  mutate(
    
    increasing=n_1*100/n
  )%>%
  mutate(
    
    stable=n_2*100/n
  )%>%
  mutate(
    
    decreasing=n_3*100/n
  )%>%
  mutate(
    
    notrend=n_4*100/n
  )

c1 =a[,c("NAME_EN","WB_A3")]
c1 = unique(c1)

d<- c1 %>%
  left_join(c,by="WB_A3")


names(d) = c("country","ISO","totalN","increasingN","stableN","decreasingN","notrend","increasingP","stableP","decreasingP","notrendP")
#d <- na.omit(d)

write.csv(d,file = "/mnt/work/YY/summer/night/trendSN.csv")

#####----------------------------------------------
##----------------summer-day

ad.mergep = read.csv("/mnt/work/YY/summer/day/SD_p.csv")

ad.merges = read.csv("/mnt/work/YY/summer/day/SD_s.csv")

ad.mergep =as.data.table(ad.mergep)
ad.merges =as.data.table(ad.merges)

ad.mergep = ad.mergep[,c(9,14,15,23,31,54,55,56)]
ad.merges = ad.merges[,c(54,55,56)]

ad.mergep = unique(ad.mergep)
ad.merges = unique(ad.merges)

last.df = merge(ad.mergep,ad.merges,by = c("lon","lat"))
write.csv(last.df,file = "/mnt/work/YY/summer/day/SD.csv")

a = last.df


#--------------


###increasing trend
b<- a %>%
  filter(pvalue<0.05 & slope >0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_1=n()
  )

b2<- a %>%
  filter(pvalue<0.05 & slope ==0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_2=n()
  )
b3<- a %>%
  filter(pvalue<0.05 & slope <0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_3=n()
  )

b4<- a %>%
  filter(pvalue >= 0.05) %>%
  group_by(WB_A3) %>%
  summarise(
    n_4=n()
  )

c<- a %>%
  group_by(WB_A3) %>%
  summarise(
    n=n()
  ) %>%
  left_join(b,by="WB_A3") %>%
  left_join(b2,by="WB_A3") %>%
  left_join(b3,by="WB_A3") %>%
  left_join(b4,by="WB_A3") %>%
  mutate(
    
    increasing=n_1*100/n
  )%>%
  mutate(
    
    stable=n_2*100/n
  )%>%
  mutate(
    
    decreasing=n_3*100/n
  )%>%
  mutate(
    
    notrend=n_4*100/n
  )

c1 =a[,c("NAME_EN","WB_A3")]
c1 = unique(c1)

d<- c1 %>%
  left_join(c,by="WB_A3")


names(d) = c("country","ISO","totalN","increasingN","stableN","decreasingN","notrend","increasingP","stableP","decreasingP","notrendP")
#d <- na.omit(d)

write.csv(d,file = "/mnt/work/YY/summer/day/trendSD.csv")


#####----------------------------------------------
##----------------winter-day

ad.mergep = read.csv("/mnt/work/YY/winter/day/WD_p.csv")

ad.merges = read.csv("/mnt/work/YY/winter/day/WD_s.csv")

ad.mergep =as.data.table(ad.mergep)
ad.merges =as.data.table(ad.merges)

ad.mergep = ad.mergep[,c(9,14,15,23,31,54,55,56)]
ad.merges = ad.merges[,c(54,55,56)]

ad.mergep = unique(ad.mergep)
ad.merges = unique(ad.merges)

last.df = merge(ad.mergep,ad.merges,by = c("lon","lat"))
write.csv(last.df,file = "/mnt/work/YY/winter/day/WD.csv")

a = last.df


#--------------


###increasing trend
b<- a %>%
  filter(pvalue<0.05 & slope >0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_1=n()
  )

b2<- a %>%
  filter(pvalue<0.05 & slope ==0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_2=n()
  )
b3<- a %>%
  filter(pvalue<0.05 & slope <0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_3=n()
  )

b4<- a %>%
  filter(pvalue >= 0.05) %>%
  group_by(WB_A3) %>%
  summarise(
    n_4=n()
  )

c<- a %>%
  group_by(WB_A3) %>%
  summarise(
    n=n()
  ) %>%
  left_join(b,by="WB_A3") %>%
  left_join(b2,by="WB_A3") %>%
  left_join(b3,by="WB_A3") %>%
  left_join(b4,by="WB_A3") %>%
  mutate(
    
    increasing=n_1*100/n
  )%>%
  mutate(
    
    stable=n_2*100/n
  )%>%
  mutate(
    
    decreasing=n_3*100/n
  )%>%
  mutate(
    
    notrend=n_4*100/n
  )

c1 =a[,c("NAME_EN","WB_A3")]
c1 = unique(c1)

d<- c1 %>%
  left_join(c,by="WB_A3")


names(d) = c("country","ISO","totalN","increasingN","stableN","decreasingN","notrend","increasingP","stableP","decreasingP","notrendP")
#d <- na.omit(d)

write.csv(d,file = "/mnt/work/YY/winter/day/trendWD.csv")

#####----------------------------------------------
##----------------winter-night
ad.mergep = read.csv("/mnt/work/YY/winter/night/WN_p.csv")

ad.merges = read.csv("/mnt/work/YY/winter/night/WN_s.csv")

ad.mergep =as.data.table(ad.mergep)
ad.merges =as.data.table(ad.merges)

ad.mergep = ad.mergep[,c(9,14,15,23,31,54,55,56)]
ad.merges = ad.merges[,c(54,55,56)]

ad.mergep = unique(ad.mergep)
ad.merges = unique(ad.merges)

last.df = merge(ad.mergep,ad.merges,by = c("lon","lat"))
write.csv(last.df,file = "/mnt/work/YY/winter/night/WN.csv")

a = last.df


#--------------


###increasing trend
b<- a %>%
  filter(pvalue<0.05 & slope >0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_1=n()
  )

b2<- a %>%
  filter(pvalue<0.05 & slope ==0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_2=n()
  )
b3<- a %>%
  filter(pvalue<0.05 & slope <0) %>%
  group_by(WB_A3) %>%
  summarise(
    n_3=n()
  )

b4<- a %>%
  filter(pvalue >= 0.05) %>%
  group_by(WB_A3) %>%
  summarise(
    n_4=n()
  )

c<- a %>%
  group_by(WB_A3) %>%
  summarise(
    n=n()
  ) %>%
  left_join(b,by="WB_A3") %>%
  left_join(b2,by="WB_A3") %>%
  left_join(b3,by="WB_A3") %>%
  left_join(b4,by="WB_A3") %>%
  mutate(
    
    increasing=n_1*100/n
  )%>%
  mutate(
    
    stable=n_2*100/n
  )%>%
  mutate(
    
    decreasing=n_3*100/n
  )%>%
  mutate(
    
    notrend=n_4*100/n
  )

c1 =a[,c("NAME_EN","WB_A3")]
c1 = unique(c1)

d<- c1 %>%
  left_join(c,by="WB_A3")


names(d) = c("country","ISO","totalN","increasingN","stableN","decreasingN","notrend","increasingP","stableP","decreasingP","notrendP")
#d <- na.omit(d)

write.csv(d,file = "/mnt/work/YY/winter/night/trendWN.csv")
