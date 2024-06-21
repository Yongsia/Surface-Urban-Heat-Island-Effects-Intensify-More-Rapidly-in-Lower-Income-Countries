#R�汾����,����Rgui�ϸ���
# version
# install.packages("installr")
# library(installr)
# updateR()
####���csv�ļ�
#write.csv(w2019,file ="C:/Users/admin/Desktop/������/������/��ɽ����/2016/data2016.csv")


setwd('E:/phd/UHI/countryscale_data')

SUHIMAX=read.csv("SUHI_max.csv",na.strings = "")
SUHIMEAN=read.csv("SUHI_mean.csv",na.strings = "")
GINI=read.csv("Gini index (World Bank estimate).csv",na.strings = "")
GINI_R=read.csv("Gini inequality index reduction.csv",na.strings = "")
GNI=read.csv("GNI.csv",na.strings = "")

GDP2018=GDP[,c(2,18)]
SUHImax2018=SUHIMAX[,c(1,17)]


GDP=read.csv("perGDP.csv",na.strings = "")
DEATH=read.csv("death_rate.csv",na.strings = "")
class=read.csv("incomeCLASS.csv",na.strings = "")
SUHIMAX1= left_join(SUHIMAX,class,by="NAME")
GDP_UHIcor= read.csv("GDP_UHIcor.csv", na.strings = "")

##����NAME�кϲ���ͬ��
library(dplyr)
c= left_join(b,SUHImax2018,by="NAME")

#���csv�ļ�
write.csv(c, file ="c.csv")

##########�������sigϵ����rcorr������hmisc��
# install.packages("Hmisc")
# library(Hmisc)#���ذ�
# library(lattice)
# library(survival)
# library(Formula)
# 
# res2 <- rcorr(as.matrix(mydata))
# dat_cor <- function(MAX_GDP, ca) {
#   dat_cor <- vector()
#   for (i in 1:171) {
#     dat1 <- unlist(MAX_GDP[i,2:17])
#     dat2 <- unlist(MAX_GDP[i,22:37])
#     dat_cor[i] <-rcorr(dat1,dat2, type = "pearson")
#   }  
#   dat_cor
# }
# GDP_ca <- dat_cor(MAX_GDP, ca)


##### ��SUHI��GDP�������,dplyr����ctr+shif+c ����ע��
# dat_cor <- function(MAX_GDP, ca) {
#   dat_r <- vector("double")
#   for (i in 1:171) {
#     dat1 <- unlist(MAX_GDP[i,2:17])
#     dat2 <- unlist(MAX_GDP[i,22:37])
#     dat_r[i] <-cor(dat1,dat2,use="na.or.complete",method='pearson')
#    
#   }  
#   dat_r
# 
# }
# GDP_ca <- dat_cor(MAX_GDP, ca)
# 


# --------------------------------------------------------------------
########---------��SUHI��GDP�������,psych��,�������ֵ��������ֵ
###--��ѭ��---
# dat1 <- unlist(MAX_GDP[129,2:17])
# dat2 <- unlist(MAX_GDP[129,22:37])
# dat <-corr.test(dat1,dat2,use="na.or.complete", method = "pearson", adjust = "fdr")
# dat_cor <-data.frame(dat$r, dat$p)



# library(psych)

name <- c()
economy <- c()
region <- c()
income_group <- c()
country_name <- c()

cor_r <- c()
cor_p <- c()

for (i in 1:nrow(MAX_GDP)) {
  name_1 <- MAX_GDP[i,1]
  economy_1 <- MAX_GDP[i,18]
  region_1 <- MAX_GDP[i,19]
  income_group_1 <- MAX_GDP[i,20]
  country_name_1 <- MAX_GDP[i,21]
  #dat <- corr.test(as.numeric(MAX_GDP[5,2:17]),as.numeric(MAX_GDP[5,22:37]), use="na.or.complete", method = "pearson", adjust = "fdr")
  dat <- corr.test(as.numeric(MAX_GDP[i,2:17]),as.numeric(MAX_GDP[i,22:37]), use="na.or.complete", method = "pearson", adjust = "fdr")
  c_r <- dat$r
  c_p <- dat$p
  
  name <- c(name,name_1)
  economy <- c(economy,economy_1)
  region <- c(region,region_1)
  income_group <- c(income_group,income_group_1)
  country_name <- c(country_name,country_name_1)
  
  cor_r <- c(cor_r,c_r)
  cor_p <- c(cor_p,c_p)
  
  
}

data_cor <- data.frame(name,economy,region,income_group,country_name,cor_r,cor_p)
#------------------------------------------------------------------------------------------------------
  
#��cor����GNI��С����
order= read.csv("GNIorder.csv",na.strings = "")
# cor_order= merge(order,data_cor,by="name",all=TRUE)#������
cor_order= right_join(order,data_cor,by="name")

library(ggplot2)
library(reshape2)
library(cowplot)

#######��ͼ#########
##########��SE(��׼���/��׼��)������ͼ###########
a<-ggplot(cor_order, aes(x=income_group, y=cor_r, fill=country_name)) +
  geom_bar(stat="identity", position=position_dodge(),
           color="black", width=.8) +
  geom_errorbar(aes(ymin=Mean-Se, ymax=Mean +Se),
                position=position_dodge(.8), width=.2) +
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim = c(0, 8))+
  theme(axis.text.x = element_text(size = 14, color = "black"))+##����x�������С
  theme(axis.text.y = element_text(size = 14, color = "black"))+##����y�������С
  theme(title=element_text(size=13))+#���ñ��������С
  theme_bw()
a














# -------------------------------------------------------------
#####-------���������ͼ����������
#��װ linkET ��
install.packages('devtools')
devtools::install_github('Hy4m/linkET')

library(linkET)
library(ggplot2)
library(dplyr)

#��ȡʾ������
micro <- read.delim('΢��������.txt', row.names = 1)
env <- read.delim('��������.txt', row.names = 1)

#���� Mantel �����
#ͨ�� spec_select ָ�������鷶Χ���������ʾ�������У�΢�������ĵ�1-22�������ַ�����ݣ�ָ������ Taxonomy������23-40���ǻ��������ݣ�ָ������ Function��
#Ĭ������£��� spec ���� Bray-Curtis ���룬�� env ���� Euclidean ���룬Ȼ�������� Mantel ���
mantel <- mantel_test(spec = micro, env = env, spec_select = list(Taxonomy = 1:22, Function = 23:40), mantel_fun = 'mantel')

#�������ϵ�������������ñ�ǩ���Ա���ͼʱ�����߿�����ɫ
mantel <- mutate(mantel, 
                 rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf), labels = c('< 0.2', '0.2 - 0.4', '>= 0.4')),
                 pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c('< 0.01', '0.01 - 0.05', '>= 0.05'))
)
mantel

#�������ͼ
qcorrplot(correlate(env, method = 'spearman'), type = 'upper', diag = FALSE) +  #��������������� Spearman ���ϵ��
  geom_square() +  #���� Spearman ���ϵ����ͼ
  geom_mark(sep = '\n', size = 2.5, sig.thres = 0.05) +  #��ʾ Spearman ���ϵ����������
  geom_couple(aes(color = pd, size = rd), data = mantel, curvature = nice_curvature()) +  #������΢����������չʾΪ���� Mantel ���
  scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +  #���� Spearman ���ָ����ͼ��ɫ
  scale_size_manual(values = c(0.5, 1, 2)) +  #���� Mantel ���ָ��������ϸ
  scale_color_manual(values = c('#D95F02', '#1B9E77', '#E0E0E0')) +  #���� Mantel ��� p ֵָ��������ɫ
  guides(color = guide_legend(title = "Mantel's p", order = 1), #ͼ�����������
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "Spearman's r", order = 3)) +
  theme(legend.key = element_blank())


########
library(ggcor)
library(ggplot2)
library(dplyr)

#��ȡʾ������
micro <- read.delim('΢��������.txt', row.names = 1)
env <- read.delim('��������.txt', row.names = 1)

#���� Mantel �����
#ͨ�� spec.select ָ�������鷶Χ���������ʾ�������У�΢�������ĵ�1-22�������ַ�����ݣ�ָ������ Taxonomy������23-40���ǻ��������ݣ�ָ������ Function��
#Ĭ������£��� spec ���� Bray-Curtis ���룬�� env ���� Euclidean ���룬Ȼ�������� Mantel ���
mantel <- fortify_mantel(micro, env, mantel.fun = 'mantel.randtest',
                         spec.dist.method = 'euclidean', env.dist.method = 'euclidean', 
                         spec.select = list(Taxonomy = 1:22, Function = 23:40))

#�������ϵ�������������ñ�ǩ���Ա���ͼʱ�����߿�����ɫ
mantel <- mutate(mantel, 
                 r = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
                 p.value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE)
)
mantel

#�������ͼ������������������һ�¾����������
quickcor(env, cor.test = TRUE, type = 'upper') +
  geom_square(data = get_data(type = 'upper', show.diag = FALSE)) +
  geom_mark(data = get_data(type = 'upper', show.diag = FALSE), sep = '\n', size = 2.5, sig.thres = 0.05) +
  add_link(mantel, mapping = aes(color = p.value, size = r), diag.label = TRUE) +
  scale_size_manual(values = c(0.1, 0.5, 1)) + 
  scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +
  scale_color_manual(values = c('#56B4E9', '#E69F00', '#999999')) + 
  guides(color = guide_legend(title = "Mantel's p", order = 1), #ͼ�����������
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  add_diag_label(angle = 45) +  #������ڶԽ�����ʾ����������ǩ
  remove_axis('all') +
  theme(legend.key = element_blank())









