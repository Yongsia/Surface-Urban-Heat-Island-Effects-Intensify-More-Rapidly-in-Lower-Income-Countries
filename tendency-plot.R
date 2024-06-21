library(ggplot2)
library(dplyr)
library(maps)
world <- map_data("world")
a <- read.csv("/mnt/work/YY/dataset/YD_total.csv")
# a <- read.csv("/mnt/work/YY/dataset/YN_total.csv")
# a <- read.csv("//mnt/work/YY/dataset/SD_total.csv")
# a <- read.csv("//mnt/work/YY/dataset/SN_total.csv")
# a <- read.csv("//mnt/work/YY/dataset/WD_total.csv")
# a <- read.csv("//mnt/work/YY/dataset/WN_total.csv")

# head(data)
# data = a[,c(7,8,9,10)]
# data = a

data1 = subset(a, pvalue < 0.05)
p = ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="white") +
  geom_point( data=data1, aes(x=lon, y=lat, color=slope),size = 0.001) +
  #scale_color_distiller(palette = "Spectral")+
  scale_color_gradient2( low = "#014C17", high = "#870406",mid = NA,limit=c(-0.6,0.6),breaks = c(-0.6, -0.3, 0.3, 0.6))+
  scale_x_continuous(limits=c(-180,195)) +
  scale_y_continuous(limits=c(-58,85))+
  #theme_bw()+
  # geom_point( data=data3, aes(x=lon, y=lat, color=slope),size = 0.5) +
  # scale_color_gradient2( low = "#1B7B02", high = "#38FC06")+
  # geom_point(data = data1, aes(x = lon, y = lat, shape = p), size = 0.01) +
  # scale_shape_manual(values = c(3),na.translate = FALSE)+ 
  
  coord_quickmap()+
  theme(legend.background = element_rect(fill = "transparent"),panel.background = element_rect(fill = '#99CBD6'))+
  labs(x="Longitude",y="Latitude",color="Slope")
ggsave(p,dpi=300,filename = "/mnt/work/YY/year_day/WN.png")
ggsave(p,dpi=300,filename = "/mnt/work/YY/year_day/WN.eps")

# Á¬????Í¼?? -------------------------------------------------------------------


p = ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="white") +
  geom_point( data=data1, aes(x=lon, y=lat, color=slope),size = 0.1) +
  #scale_color_distiller(palette = "Spectral")+
  scale_color_gradient2( low = "#014C17", high = "#870406",limit=c(-0.6,0.6))+
  #t
  me_bw()+
  # geom_point( data=data3, aes(x=lon, y=lat, color=slope),size = 0.5) +
  # scale_color_gradient2( low = "#1B7B02", high = "#38FC06")+
  # geom_point(data = data1, aes(x = lon, y = lat, shape = p), size = 0.01) +
  # scale_shape_manual(values = c(3),na.translate = FALSE)+ 

  coord_quickmap()+
  theme(legend.background = element_rect(fill = "transparent"),panel.background = element_rect(fill = '#99CBD6'))+
  labs(x="Longitude",y="Latitude",color="Slope")
ggsave(p,dpi=300,filename = "/mnt/work/YY/year_day/YD.png")
ggsave(p,dpi=300,filename = "/mnt/work/YY/year_day/YD.eps")

# "#78A8CD"
ggsave(p,filename = "/mnt/work/YY/year_day/yd.png")
ggsave(p,filename = "/mnt/work/YY/year_day/yn.png")
# Left chart




ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=lon, y=lat)) +
  theme_void()  + coord_map() 

library(viridis)
# Left: use size and color
ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=lon, y=lat, color=slope)) +
  geom_point( data=data, aes(x=lon, y=lat)) +
  scale_size_continuous(range=c(1,12)) +
#scale_color_viridis(trans="log") +
  theme_void()  + coord_map() 


ggsave(p,filename = "/mnt/work/YY/year_day/yd.png")



# Center: reorder your dataset first! Big cities appear later = on top
data %>%
  arrange(pop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

# Right: just use arrange(desc(pop)) instead
data %>%
  arrange(desc(pop)) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")