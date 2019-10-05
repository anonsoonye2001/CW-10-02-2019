load("fish_data.Rdata")
f<-fish
f
# subsetting---
# indexing by condition
fd<- f[f$depth_fac=="Deep",]
fd
fd2<-subset(x=f,depth_fac=="Deep") # method two
fd2
#shallow tows
#east
#patches
fd4<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac"))
fd4
##filter
library(dplyr)
fd3<-filter(.data=f, depth_fac=="Deep")
fd3
##which
fd5<-f[which(f$depth_fac=='Deep' & f$area_fac=="east"),]
fd5
fd6<-f[which(f$depth_fac=='Deep' &f$area_fac=="east")&f$yr_fac!="2014",]
fd6
str(fd6)
head(fd6)
#subset & combine using rowbind(rbind function)----
d1<-f[which(f$depth_fac=='Deep' & f$area_fac=="east"),]
d1
str(d1)
d2<-f[which(f$depth_fac=='shallow' & f$area_fac=="west"),]
#combine d1 and d2 into a single dataframe
nrow(d1)
nrow(d2)
nrow(d1)+nrow(d2)
d3<-rbind(d1,d2)
nrow(d3)
#combine dataframes with separate columns into a single dataframe
c1<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac"))
c1
c2<-subset(x=f,depth_fac=="Deep", select=c("area_fac","parcel.length.m","group"))
c2
c3<-cbind(c1,c2)
c3
head(c3)
#merging 2 dataframes ensuring that observations from one dataframe are connected with
#observation in the second data frame correctly
m1<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac"))
m1$seq<-seq(1,nrow(m1),1)
head(m1)
m2<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac","parcel.length.m","group"))
m2$seq<-seq(1,nrow(m2),1)
head(m2)
# create a sequence of data
m2$seq<-seq(from=1, to=nrow(m2),by=1)
v<-seq(5,20,0.5)
v
vc<-cut(x=v,breaks=seq(5,20,1),include.lowest = T)
vc
