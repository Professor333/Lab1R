#Move all of your sample.csv files into a folder, and then set the WD to that file path
setwd("C:/Users/Ryan/Development/R/BIO 614/Lab1/Samples")
a <- read.csv("sample1.csv", header=T)
attach(a)
s1=a$x
hist(s1)
mean(s1)

sds=function(x) (x-mean(x))/sd(x)
sds1=sds(s1)
sds1


out=data.frame(cbind(sds1))
write.table(out, "sample1_sds.csv", sep=",", row.names = FALSE)            
sample1_sds=read.csv("sample1_sds.csv", header = TRUE)
hist(sds1)



b=read.csv("sample2.csv", header=T)
attach(b)
s2=b$x
mean(s2)

c=read.csv("sample3.csv", header=T)
attach(c)
s3=c$x
mean(s3)

d=read.csv("sample4.csv", header=T)
attach(d)
s4=d$x
mean(s4)

e=read.csv("sample5.csv", header=T)
attach(e)
s5=e$x
mean(s5)


f=read.csv("sample6.csv", header=T)
attach(f)
s6=f$x
mean(s6)

g=read.csv("sample7.csv", header=T)
attach(g)
s7=g$x
mean(s7)

h=read.csv("sample8.csv", header=T)
attach(h)
s8=h$x
mean(s8)

i=read.csv("sample9.csv", header=T)
attach(i)
s9=i$x
mean(s9)

j=read.csv("sample10.csv", header=T)
attach(j)
s10=j$x
mean(s10)

data1<- cbind(s1)
colnames(data)<- NULL
rownames(data)<-NULL
means1<-apply(data1,2,mean)
sds1=apply(data1,2,sds)


data2<- cbind(s1,s2)
colnames(data)<- NULL
rownames(data)<-NULL
means2<-apply(data2,2,mean)
means2=mean(means2)
sds2=apply(data2,2,sds)


data3<- cbind(s1,s2,s3)
colnames(data)<- NULL
rownames(data)<-NULL
means3<-apply(data3,2,mean)
means3=mean(means3)
sds3=apply(data3,2,sds)


data4<- cbind(s1,s2,s3,s4)
colnames(data)<- NULL
rownames(data)<-NULL
means4<-apply(data4,2,mean)
means4=mean(means4)
sds4=apply(data4,2,sds)


data5<- cbind(s1,s2,s3,s4,s5)
colnames(data)<- NULL
rownames(data)<-NULL
means5<-apply(data5,2,mean)
means5=mean(means5)
sds5=apply(data5,2,sds)


data6<- cbind(s1,s2,s3,s4,s5,s6)
colnames(data)<- NULL
rownames(data)<-NULL
means6<-apply(data6,2,mean)
means6=mean(means6)
sds6=apply(data6,2,sds)


data7<- cbind(s1,s2,s3,s4,s5,s6,s7)
colnames(data)<- NULL
rownames(data)<-NULL
means7<-apply(data7,2,mean)
means7=mean(means7)
sds7=apply(data7,2,sds)


data8<- cbind(s1,s2,s3,s4,s5,s6,s7,s8)
colnames(data)<- NULL
rownames(data)<-NULL
means8<-apply(data8,2,mean)
means8=mean(means8)
sds8=apply(data8,2,sds)


data9<- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9)
colnames(data)<- NULL
rownames(data)<-NULL
means9<-apply(data9,2,mean)
means9=mean(means9)
sds9=apply(data9,2,sds)


data10<- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
colnames(data)<- NULL
rownames(data)<-NULL
means10<-apply(data10,2,mean)
means10=mean(means10)
sds10=apply(data10,2,sds)

#####Make sure to have the ggplot2 and gridextra packages installed

#The mean is the y-value and the x-values are all the individual data points encompassing the mean
#All of the information that is really needed to draw conclusions from is contained with df10...
#However, the assignment wants datasets of s1, then s1&s2, then s1&s2&s3, and so on...
#Really the only point of this is to watch the mean settle to a value as it includes more datasets
#This is the method that makes the most sense to me, because you see all the individual data points...
#Wherein if you type mean(means10) into the console, then you get only one value, the y-value...
#and if you type plot(means10) into the console, there is only one data point returned which is a poor graph

library(ggplot2)
library(gridExtra)

df1= data.frame(x, means1)
df1g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means1, col = "means1"))

df2= data.frame(x, means1,means2)
df2g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df3= data.frame(x, means1,means2,means3)
df3g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df4= data.frame(x, means1,means2,means3,means4)
df4g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df5= data.frame(x, means1,means2,means3,means4,means5)
df5g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means5, col = "means5")) +
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df6= data.frame(x, means1,means2,means3,means4,means5,means6)
df6g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means6, col = "means6")) +
  geom_point(aes(y = means5, col = "means5")) +
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df7= data.frame(x, means1,means2,means3,means4,means5,means6,means7)
df7g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means7, col = "means7")) +
  geom_point(aes(y = means6, col = "means6")) +
  geom_point(aes(y = means5, col = "means5")) +
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df8= data.frame(x, means1,means2,means3,means4,means5,means6,means7,means8)
df8g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means8, col = "means8")) +
  geom_point(aes(y = means7, col = "means7")) +
  geom_point(aes(y = means6, col = "means6")) +
  geom_point(aes(y = means5, col = "means5")) +
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df9= data.frame(x, means1,means2,means3,means4,means5,means6,means7,means8,means9)
df9g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means9, col = "means9")) +
  geom_point(aes(y = means8, col = "means8")) +
  geom_point(aes(y = means7, col = "means7")) +
  geom_point(aes(y = means6, col = "means6")) +
  geom_point(aes(y = means5, col = "means5")) +
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df10= data.frame(x, means1,means2,means3,means4,means5,means6,means7,means8,means9,means10)
df10g=ggplot(df10, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = means10, col = "means10")) + 
  geom_point(aes(y = means9, col = "means9")) +
  geom_point(aes(y = means8, col = "means8")) +
  geom_point(aes(y = means7, col = "means7")) +
  geom_point(aes(y = means6, col = "means6")) +
  geom_point(aes(y = means5, col = "means5")) +
  geom_point(aes(y = means4, col = "means4")) +
  geom_point(aes(y = means3, col = "means3")) +
  geom_point(aes(y = means2, col = "means2")) +
  geom_point(aes(y = means1, col = "means1"))

df10g

##################Multiplots the different concatenated data, but it doesn't look good##########
#means_graph1=grid.arrange(df1g,df2g,df3g,df4g,df5g, nrow = 5, top = "Means Part 1")

#means_graph2=grid.arrange(df6g,df7g,df8g,df9g,df10g, nrow = 5, top = "Means Part 2")

#means_graph1
#Stop to collect first Image
#means_graph2
#############################


plot.default(means10) #Just used as a means to set margins to default 
#and see that you get a graphical output
#Alternatively, use the brush in the plot window

par(mfrow=c(2,5))

#Histograms work well for this data set
hist(sds1)
hist(sds2)
hist(sds3)
hist(sds4)
hist(sds5)
hist(sds6)
hist(sds7)
hist(sds8)
hist(sds9)
hist(sds10)




