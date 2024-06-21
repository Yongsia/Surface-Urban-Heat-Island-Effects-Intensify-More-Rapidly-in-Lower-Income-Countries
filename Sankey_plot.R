# install.packages("remotes")
# library(remotes)
# remotes:::install_github( "davidsjoberg/ggsankey")

library(dplyr)
library(ggplot2)
library(ggsankey)

d1 = subset(d, pvalue < 0.05 & slope > 0)
d2 = d1[,c(6,9,10)]
d2$Cli_Zone = factor(d2$Cli_Zone, levels = c("A","B","C","D","E"))
d2$INCOME_GRP.y =  factor(d2$INCOME_GRP.y, levels = c("Low income","Lower middle income","Upper middle income","High income"))
d3 = d2[order(d2$Cli_Zone,d2$INCOME_GRP.y),]

d = read.csv("/mnt/work/YY/dataset/YD_total.csv",row.names = 1)



df = d3 %>%
  make_long(NAME_EN, Cli_Zone, INCOME_GRP.y)

#?Զ?????ɫ??
c4a_gui()
mycol <- c4a('rainbow_wh_rd',53)
mycol2 <- sample(mycol,length(mycol)) #??????????ɫ˳??
#ͼ???��???

df$node <- factor(df$node,levels = c("A","B","C","D","E"))
df$next_node <- factor(df$next_node,levels = c("Low income","Lower middle income","Upper middle income","High income"))
df = df[order(df$node,df$next_node),]
p1 <- ggplot(d4, aes(x = x,
                     next_x = next_x,
                     node = node,
                     next_node = next_node,
                     fill = factor(node),
                     label = node)) +
  #scale_fill_manual(values = mycol2) + #??????ɫ
  geom_sankey(flow.alpha = 0.5,#??????͸????
              #flow.fill = 'grey', #????????ɫ
              #flow.color = 'grey80', #????????ɫ
              smooth = 8, #??????????
              width = 0.12) + #?ڵ?????
  geom_sankey_text(size = 1.2,
                   color = "black",hjust = 0.7) +
  theme_void() +
  theme(legend.position = 'none')
  #scale_fill_lancet()##library("ggsci")
  #scale_fill_viridis_d(option ="inferno")
ggsave(p1,filename = "/mnt/work/YY/s.png")


# Sankey base -------------------------------------------------------------
# Step 1
d4 <- d %>%
  make_long(NAME_EN, Cli_Zone, INCOME_GRP.y)
TotalCount = nrow(d4)
# Step 2
dagg <- d4%>%
  dplyr::group_by(node)%>%
  tally()
dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n*100/TotalCount)
quantile(dagg$pct)# ?ҳ?ǰ25%?????? ȡ75%??ֵ??һ??183?????ң?ʣ??46??????
# sum(df2$node == "E") #????
dagg1 = subset(dagg, pct > 1.001663e-01)

dagg2 = subset(dagg1, n >= 1807)
# Step 3
df2 <- merge(d4, dagg1, by.x = 'node', by.y = 'node', all.x = TRUE)
#df2 <-df2[complete.cases(df2[,5:6]),]

# Chart 2
p1 <- ggplot(df2, aes(x = x,
                     next_x = next_x,
                     node = node,
                     next_node = next_node,
                     fill = factor(node),
                     label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
  #scale_fill_manual(values = mycol2) + #??????ɫ
  geom_sankey(#flow.alpha = 0.6,#??????͸????
              #flow.fill = 'grey', #????????ɫ
              #flow.color = 'grey80', #????????ɫ
              smooth = 10, #??????????
              width = 0.12) + #?ڵ?????
  geom_sankey_text(size = 1,
                   color = "black",hjust = 1.2) +
  theme_void() +
  theme(legend.position = 'none')
#scale_fill_lancet()##library("ggsci")
#scale_fill_viridis_d(option ="inferno")
ggsave(p1,dpi=300, width=12, height=6,filename = "/mnt/work/YY/s0.eps")

# test --------------------------------------------------------------------

a = c("A","A","C","E","B")
b = c("high","low","low","high","middle")
c = c("china","US","japan","japan","china")
d = cbind(a,b,c)
df = d %>%
  make_long(clim,income,country)
d = data.frame(d)
colnames(d) = c("clim","income","country")


TotalCount = nrow(df)
# Step 2
dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()

sum(df$node %in% c("A")) #,"B","C","D","E"



dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n*100/TotalCount)
sum(dagg$pct)

df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)
p1 <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node," (n=", n, ', ',  round(pct,1), '%)' ))) +
  #scale_fill_manual(values = mycol2) + #??????ɫ
  geom_sankey(flow.alpha = 0.4,#??????͸????
    #flow.fill = 'grey', #????????ɫ
    #flow.color = 'grey80', #????????ɫ
    smooth = 8, #??????????
    width = 0.12) + #?ڵ?????
  geom_sankey_text(size = 4,
                   color = "black",hjust = 1.2) +
  theme_void() +
  theme(legend.position = 'none')
p1




# group_barplot -----------------------------------------------------------
library(ggplot2)
eco = read.csv("C:/Users/27780/Desktop/UHI-2/论文2/barplot.csv")
eco$Income_Groups <-factor(eco$Income_Groups,ordered=TRUE,levels=c("Low","Lower-Middle","Upper-Middle","High"))
library(ggthemes)
install.packages("ggthemes")

a = ggplot(eco, aes(Income_Groups, Increasing...., fill = Time, label =Increasing....))+
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  theme_bw()+
  scale_fill_wsj()+ #library(ggthemes)
  #scale_fill_npg()+##library("ggsci")
  theme(axis.ticks.length=unit(0.1,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  #ggtitle("(a) Income groups")+
  theme(strip.text = element_blank(),axis.title.y = element_blank(),legend.position='none')+ 
  facet_grid(Time~.)+
  geom_text(aes(y = Increasing.... + 3.5), vjust = 0.5)+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 40), name = "Increasing percent (%)")#????x??????ͼ???ļ?϶
#scale_x_continuous(expand = c(0, 0))#????y??????ͼ???ļ?϶


b = ggplot(eco, aes(Income_Groups, Decreasing...., fill = Time, label =Decreasing....))+
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  theme_bw()+
  scale_fill_wsj()+ #library(ggthemes)
  #scale_fill_npg()+##library("ggsci")
  theme(axis.ticks.length=unit(0.1,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  #ggtitle("(b) Income groups_Decreasing trend")+
  theme(axis.text.y = element_blank(),strip.text = element_blank(),axis.title.y = element_blank(),legend.position='none')+ 
  facet_grid(Time~.)+
  geom_text(aes(y = Decreasing.... + 3.5), vjust = 0.5)+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 40), name = "Decreasing percent (%)")#????x??????ͼ???ļ?϶
#scale_x_continuous(expand = c(0, 0))#????y??????ͼ???ļ?϶
b

c = ggplot(eco, aes(Income_Groups, increasing.trend...., group = Time, label =increasing.trend....))+
  geom_line(size = 1,linetype = "dashed",aes(color = Time))+
  geom_point(size = 3, aes(color = Time),  stroke = 1)+
  theme_bw()+
  scale_color_manual(values = c("red","blue", "green", "darkgreen", "orange","black")) +
  #scale_fill_npg()+##library("ggsci")
  theme(axis.ticks.length=unit(0.1,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  #ggtitle("(c) Income groups_Decreasing trend")+
  theme(axis.text.y = element_blank(),axis.title.y = element_blank(),legend.position='none')+ 
  facet_grid(Time~.)+
  geom_text(aes(y = increasing.trend....+ 3.5), vjust = 0.5)+
  coord_flip()+
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed",size= 0.5) +
  scale_y_continuous(expand = c(0, 0),limits = c(-3, 33), name = "Δ Increasing trend (%)")#????x??????ͼ???ļ?϶
c

p1 = ggpubr::ggarrange(a,b,c, nrow = 1, ncol = 3)#, labels = c('A', 'B')
p1

clim = read.csv("C:/Users/27780/Desktop/UHI-2/论文2/clim_barplot.csv")
head(clim)
clim$Cli_Zone <-factor(clim$Cli_Zone,ordered=TRUE,levels=c("E","D","C","B","A"))
colnames(clim)
a1 = ggplot(clim, aes(Cli_Zone, Increasing...., fill = Time, label =Increasing....))+
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  theme_bw()+
  #scale_fill_wsj()+ #library(ggthemes)
  #scale_fill_jama()+
  scale_fill_npg()+##library("ggsci")
  theme(axis.ticks.length=unit(0.1,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  #ggtitle("(c) Climate zones_Increasing trend")+
  theme(strip.text = element_blank(),axis.title.y = element_blank(),legend.position='none')+ 
  facet_grid(Time~.)+
  geom_text(aes(y = Increasing.... + 3.5), vjust = 0.5)+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 40), name = "Increasing percent (%)")#????x??????ͼ???ļ?϶
#scale_x_continuous(expand = c(0, 0))#????y??????ͼ???ļ?϶
a1

b1 = ggplot(clim, aes(Cli_Zone, Decreasing...., fill = Time, label =Decreasing....))+
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  theme_bw()+
  #scale_fill_wsj()+ #library(ggthemes)
  scale_fill_npg()+##library("ggsci")
  theme(axis.ticks.length=unit(0.1,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  #ggtitle("(d) Climate zones_Decreasing trend")+
  theme(axis.text.y = element_blank(),strip.text = element_blank(),axis.title.y = element_blank(),legend.position='none')+ 
  facet_grid(Time~.)+
  geom_text(aes(y = Decreasing.... + 3.5), vjust = 0.5)+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 40), name = "Decreasing percent (%)")#????x??????ͼ???ļ?϶


c1 = ggplot(clim, aes(Cli_Zone, X2, group = Time, label =X2))+
  geom_line(size = 1,linetype = "dashed",aes(color = Time))+
  geom_point(size = 3, aes(color = Time),  stroke = 1)+
  theme_bw()+
  scale_color_manual(values = c("#E51616","#04AAE6","#029881", "#5B3E9F", "#E0730E","#C490DD")) +
  #scale_fill_npg()+##library("ggsci")
  theme(axis.ticks.length=unit(0.1,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  #ggtitle("(c) Income groups_Decreasing trend")+
  theme(axis.text.y = element_blank(),axis.title.y = element_blank(),legend.position='none')+ 
  facet_grid(Time~.)+
  geom_text(aes(y = X2 + 3.5), vjust = 0.5)+
  coord_flip()+
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed",size= 0.5) +
  scale_y_continuous(expand = c(0, 0),limits = c(-13, 33), name = "Δ Increasing trend (%)")#????x??????ͼ???ļ?϶
c1

p1 = ggpubr::ggarrange(a1,b1,c1, nrow = 1, ncol = 3)#, labels = c('A', 'B')
p1


# base table --------------------------------------------------------------
install.packages("fpc")
library(fpc)
a <- read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/YD_country.csv")
b <- read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/YN_country.csv")
c <- read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/SD_country.csv")
d <- read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/SN_country.csv")
e <- read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/WD_country.csv")
f <- read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/WN_country.csv")


a1 = a[,c("lon","lat","pvalue","slope")]
names(a1) = c("lon","lat","YD_p","YD_s")

b1 = b[,c("lon","lat","pvalue","slope")]
names(b1) = c("lon","lat","YN_p","YN_s")

c1 = c[,c("lon","lat","pvalue","slope")]
names(c1) = c("lon","lat","SD_p","SD_s")

d1 = d[,c("lon","lat","pvalue","slope")]
names(d1) = c("lon","lat","SN_p","SN_s")

e1 = e[,c("lon","lat","pvalue","slope")]
names(e1) = c("lon","lat","WD_p","WD_s")

f1 = f[,c("lon","lat","pvalue","slope")]
names(f1) = c("lon","lat","WN_p","WN_s")

library(data.table)


b0 = merge(a1,b1,by = c("lon","lat"))
c0 = merge(b0,c1,by = c("lon","lat"))
d0 = merge(c0,d1,by = c("lon","lat"))
e0 = merge(d0,e1,by = c("lon","lat"))
f0 = merge(e0,f1,by = c("lon","lat"))

write.csv(f0,file="C:/Users/27780/Desktop/UHI-2/????2/data/countries.csv")

a = read.csv("C:/Users/27780/Desktop/UHI-2/????2/data/countries.csv",row.names = 1)
a1 = a[,c(1,2,4,6,8,10,12,14)]

a2 = a1[,c(3:8)]

boxplot(a2,ylab="Sen's slope value", notch=TRUE,
        col=(c("gold","darkgreen","#E85A5A","#2F8FEF","#EC56CC")),boxwex = 0.5,border="#585858",outline = FALSE,dotsize = 0.1)



a = read.csv("C:/Users/27780/Desktop/UHI-2/论文2/change_year.csv")

a$Year = as.numeric(a$Year)
ggplot(a, aes(x = reorder(Year, -Year), y = percent....,fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#619CFF","#E51616","#04AAE6","#029881", "#5B3E9F", "#E0730E","#C490DD","#585858"))+
  coord_flip()+
  theme(axis.title.y = element_blank(),legend.position='none')+ 
  scale_y_continuous(expand = c(0, 0),limits = c(0, 22), name = "")

