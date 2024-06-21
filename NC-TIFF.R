library(terra)
prebr <- rast("C:/Users/admin/Downloads/CO2/CO2.nc")
terra::writeRaster(prebr,paste0("D:/download/CO2",names(prebr),'.tif'),
                   overwrite = T,filetype = "GTiff") 



#����������
library(rgdal) 

library(raster)

#�趨�����ռ䣬һ���ǻ���ͼ�����ڵ��ļ��С�(������ϲ��)

setwd("E:/phd/UHI/2010plot/2010")

name <- strsplit(getwd(),split="/") #ʹ��"/"��Ŀ¼���зָ�(Ƕ��list)

number <- length(name[[1]]) #�ж�Ŀ¼����

csvname <- paste(as.character(name[[1]][number]),".csv",sep = "")#Ϊ����ļ�ָ�����֡�

lst <- list.files(path=getwd(),pattern='tif$',full.names = T) # ��ȡĿ¼������ָ�����͵��ļ����������б��������õ���tiff��ʽͼ�㣬�ɸ����Լ�����Ҫ�޸ģ�����# bil��asc �ȡ�������ԴWorldClid��վ��

n <- length(lst) #��������ļ�����

#��ȡ���ֲַ���

data<-read.csv("summer.csv",head=T,row.names=1) #��ȡ����csv�ļ���������һ������Ϊ������(����д����ļ��Ļ��������CSV�ļ�д�ɲ��ڹ���Ŀ¼��ľ���·��)

attach(data) #����·��������

dim(data) #����ά��

#��������ϵ

coordinates(data)<-c("lng","lat") #Lon Ϊ���ȣ�LatΪγ�ȡ�

#�����������ں��ڴ洢���ݽ��

result<-matrix(rep(0,10188*n),ncol = n) #10188Ϊ�ֲ��������nΪ�������Ӹ���������10188*n�ľ���������������ݡ�

bio_number<-vector()

#���к��Ĵ��룬��ȡÿ�����������Ϣ

for(i in 1:length(lst)){
  filename <- strsplit(lst[[i]],split="/") 
  
  file<-filename[[1]][length(filename[[1]])]
  
  BIO <- raster(file) #��ȡ��i������ͼ��
  
  BIO_centroid <- extract(BIO, data, method='simple', buffer=1000, fun=mean, df=TRUE) #��ȡÿ�����������Ϣ����i������ͼ�㣬�����õ��Ǿ�ֵ(���괦��һ������ֵ)��ͬʱҲ��ѡ���������㷽����
  
  bio_number[i]<-strsplit(strsplit(lst[[i]],split="/")[[1]][number+1],split=".tif")[[1]] #��ȡ����ͼ����
  
  #ע�����е�"number+1"��ָ��ǰͼ���ڵڼ����ļ��С������ֶ�ָ�����Ա���������·����������������һ�㣩
  
  #"E:/Worldclim/30 seconds/version (1.0)/bio1-19_30s_bil/bio_1.bil"   ��������ӦΪ[6]��ע��б�ܵķ���
  
  result[,i]<-BIO_centroid[,2]
  
}

colnames(result)<-bio_number

detach (data) #�Ӵ�attach��

write.csv(result,csvname) #���������ΪCSV�ļ����������ڹ����ռ䡣

rm(list = ls()) #������б������Ա��´����С�

#��ȡ����tiff�ļ�
BIO <- raster("CO2emi_co2.tif") #��ȡ��i������ͼ��

BIO_centroid <- extract(BIO, data, method='simple', buffer=1000, fun=mean, df=TRUE) #��ȡÿ�����������Ϣ����i������ͼ�㣬�����õ��Ǿ�ֵ(���괦��һ������ֵ)��ͬʱҲ��ѡ���������㷽����


result =  cbind(data,BIO_centroid)
write.csv(result,"summer.csv")