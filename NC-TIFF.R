library(terra)
prebr <- rast("C:/Users/admin/Downloads/CO2/CO2.nc")
terra::writeRaster(prebr,paste0("D:/download/CO2",names(prebr),'.tif'),
                   overwrite = T,filetype = "GTiff") 



#加载软件包
library(rgdal) 

library(raster)

#设定工作空间，一般是环境图层所在的文件夹。(看个人喜好)

setwd("E:/phd/UHI/2010plot/2010")

name <- strsplit(getwd(),split="/") #使用"/"对目录进行分割(嵌套list)

number <- length(name[[1]]) #判断目录级数

csvname <- paste(as.character(name[[1]][number]),".csv",sep = "")#为输出文件指定名字。

lst <- list.files(path=getwd(),pattern='tif$',full.names = T) # 读取目录下所有指定类型的文件，并返回列表。这里用的是tiff格式图层，可根据自己的需要修改，例如# bil、asc 等。数据来源WorldClid网站。

n <- length(lst) #检测数据文件个数

#读取物种分布点

data<-read.csv("summer.csv",head=T,row.names=1) #读取坐标csv文件，并将第一列设置为行名。(如果有大量文件的话建议这个CSV文件写成不在工作目录外的绝对路径)

attach(data) #添加路径索引。

dim(data) #创建维数

#设置坐标系

coordinates(data)<-c("lng","lat") #Lon 为经度，Lat为纬度。

#建立矩阵，用于后期存储数据结果

result<-matrix(rep(0,10188*n),ncol = n) #10188为分布点个数，n为环境因子个数。生成10188*n的矩阵好往里面填数据。

bio_number<-vector()

#运行核心代码，提取每个点的气候信息

for(i in 1:length(lst)){
  filename <- strsplit(lst[[i]],split="/") 
  
  file<-filename[[1]][length(filename[[1]])]
  
  BIO <- raster(file) #读取第i个气候图层
  
  BIO_centroid <- extract(BIO, data, method='simple', buffer=1000, fun=mean, df=TRUE) #提取每个点的气候信息，第i个气候图层，其中用的是均值(座标处不一定有数值)，同时也可选择其他计算方法。
  
  bio_number[i]<-strsplit(strsplit(lst[[i]],split="/")[[1]][number+1],split=".tif")[[1]] #获取气候图层名
  
  #注意其中的"number+1"是指当前图层在第几级文件中。（可手动指定，对比上下两个路径的例子来理解这一点）
  
  #"E:/Worldclim/30 seconds/version (1.0)/bio1-19_30s_bil/bio_1.bil"   这里设置应为[6]，注意斜杠的方向。
  
  result[,i]<-BIO_centroid[,2]
  
}

colnames(result)<-bio_number

detach (data) #接触attach绑定

write.csv(result,csvname) #将结果导出为CSV文件，并储存在工作空间。

rm(list = ls()) #清除所有变量，以备下次运行。

#提取单个tiff文件
BIO <- raster("CO2emi_co2.tif") #读取第i个气候图层

BIO_centroid <- extract(BIO, data, method='simple', buffer=1000, fun=mean, df=TRUE) #提取每个点的气候信息，第i个气候图层，其中用的是均值(座标处不一定有数值)，同时也可选择其他计算方法。


result =  cbind(data,BIO_centroid)
write.csv(result,"summer.csv")
