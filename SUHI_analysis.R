#R版本更新,需在Rgui上更新
# version
# install.packages("installr")
# library(installr)
# updateR()
####输出csv文件
#write.csv(w2019,file ="C:/Users/admin/Desktop/三江汇/富阳区/渔山数据/2016/data2016.csv")


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

##按照NAME列合并相同行
library(dplyr)
c= left_join(b,SUHImax2018,by="NAME")

#输出csv文件
write.csv(c, file ="c.csv")

##########求相关性sig系数，rcorr函数，hmisc包
# install.packages("Hmisc")
# library(Hmisc)#加载包
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


##### 求SUHI和GDP间相关性,dplyr包，ctr+shif+c 快速注释
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
########---------求SUHI和GDP间相关性,psych包,有相关性值和显著性值
###--无循环---
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
  
#将cor按照GNI大小排序。
order= read.csv("GNIorder.csv",na.strings = "")
# cor_order= merge(order,data_cor,by="name",all=TRUE)#外连接
cor_order= right_join(order,data_cor,by="name")

library(ggplot2)
library(reshape2)
library(cowplot)

#######绘图#########
##########用SE(标准误差/标准误)进行作图###########
a<-ggplot(cor_order, aes(x=income_group, y=cor_r, fill=country_name)) +
  geom_bar(stat="identity", position=position_dodge(),
           color="black", width=.8) +
  geom_errorbar(aes(ymin=Mean-Se, ymax=Mean +Se),
                position=position_dodge(.8), width=.2) +
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim = c(0, 8))+
  theme(axis.text.x = element_text(size = 14, color = "black"))+##设置x轴字体大小
  theme(axis.text.y = element_text(size = 14, color = "black"))+##设置y轴字体大小
  theme(title=element_text(size=13))+#设置标题字体大小
  theme_bw()
a














# -------------------------------------------------------------
#####-------绘制相关性图，带聚类线
#安装 linkET 包
install.packages('devtools')
devtools::install_github('Hy4m/linkET')

library(linkET)
library(ggplot2)
library(dplyr)

#读取示例数据
micro <- read.delim('微生物数据.txt', row.names = 1)
env <- read.delim('环境数据.txt', row.names = 1)

#计算 Mantel 相关性
#通过 spec_select 指定数据组范围，例如这个示例数据中，微生物矩阵的第1-22列是物种丰度数据（指定名称 Taxonomy），第23-40列是基因丰度数据（指定名称 Function）
#默认情况下，对 spec 计算 Bray-Curtis 距离，对 env 计算 Euclidean 距离，然后计算二者 Mantel 相关
mantel <- mantel_test(spec = micro, env = env, spec_select = list(Taxonomy = 1:22, Function = 23:40), mantel_fun = 'mantel')

#根据相关系数和显著性设置标签，以便作图时定义线宽和颜色
mantel <- mutate(mantel, 
                 rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf), labels = c('< 0.2', '0.2 - 0.4', '>= 0.4')),
                 pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c('< 0.01', '0.01 - 0.05', '>= 0.05'))
)
mantel

#绘制相关图
qcorrplot(correlate(env, method = 'spearman'), type = 'upper', diag = FALSE) +  #环境变量矩阵计算 Spearman 相关系数
  geom_square() +  #绘制 Spearman 相关系数热图
  geom_mark(sep = '\n', size = 2.5, sig.thres = 0.05) +  #显示 Spearman 相关系数和显著性
  geom_couple(aes(color = pd, size = rd), data = mantel, curvature = nice_curvature()) +  #环境和微生物的相关性展示为上述 Mantel 相关
  scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +  #根据 Spearman 相关指定热图颜色
  scale_size_manual(values = c(0.5, 1, 2)) +  #根据 Mantel 相关指定线条粗细
  scale_color_manual(values = c('#D95F02', '#1B9E77', '#E0E0E0')) +  #根据 Mantel 相关 p 值指定线条颜色
  guides(color = guide_legend(title = "Mantel's p", order = 1), #图例标题和排序
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "Spearman's r", order = 3)) +
  theme(legend.key = element_blank())


########
library(ggcor)
library(ggplot2)
library(dplyr)

#读取示例数据
micro <- read.delim('微生物数据.txt', row.names = 1)
env <- read.delim('环境数据.txt', row.names = 1)

#计算 Mantel 相关性
#通过 spec.select 指定数据组范围，例如这个示例数据中，微生物矩阵的第1-22列是物种丰度数据（指定名称 Taxonomy），第23-40列是基因丰度数据（指定名称 Function）
#默认情况下，对 spec 计算 Bray-Curtis 距离，对 env 计算 Euclidean 距离，然后计算二者 Mantel 相关
mantel <- fortify_mantel(micro, env, mantel.fun = 'mantel.randtest',
                         spec.dist.method = 'euclidean', env.dist.method = 'euclidean', 
                         spec.select = list(Taxonomy = 1:22, Function = 23:40))

#根据相关系数和显著性设置标签，以便作图时定义线宽和颜色
mantel <- mutate(mantel, 
                 r = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
                 p.value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE)
)
mantel

#绘制相关图，各参数和上述基本一致就随便设置了
quickcor(env, cor.test = TRUE, type = 'upper') +
  geom_square(data = get_data(type = 'upper', show.diag = FALSE)) +
  geom_mark(data = get_data(type = 'upper', show.diag = FALSE), sep = '\n', size = 2.5, sig.thres = 0.05) +
  add_link(mantel, mapping = aes(color = p.value, size = r), diag.label = TRUE) +
  scale_size_manual(values = c(0.1, 0.5, 1)) + 
  scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +
  scale_color_manual(values = c('#56B4E9', '#E69F00', '#999999')) + 
  guides(color = guide_legend(title = "Mantel's p", order = 1), #图例标题和排序
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  add_diag_label(angle = 45) +  #如果想在对角线显示环境变量标签
  remove_axis('all') +
  theme(legend.key = element_blank())










