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
##merge I
?merge()
mt<-merge(x=m1, y=m2,by=c("transect.id","seq"),all.x=T,no.dups=T)
nrow(m1)+nrow(m2)
##join
library(dplyr)
mj<-dplyr::right_join(x=m1,y=m2,by=c('transect.id'))
nrow(m1)+nrow(m2)
nrow(mt)


#summarizing data
#2 Oct 2019
#-----
library(tidyverse)
install.packages("nutshell")
library(nutshell)
#data we will be using today
data("batting.2008")
d<-batting.2008
d

#tapply---(tidyverse function)
?tapply()
hr<-tapply(x=d$HR,INDEX=list(d$teamID),FUN=sum)

##find qualitile values for home runs by team
##fivenum gives you:min, lower-hinge, median, upper-hinge, and max value
hr.q<-tapply(x=d$HR,INDEX=list(d$teamID),FUN=sum)
# one category summarize
lg.q<-tapply(x=(d$H/d$AB),INDEX=list(d$lgID),FUN=fivenum)
lg.q
head(d$lgID)
summary(d$H/d$AB)
summary(d[d$lgID=="AL",]$H/d[d$lgID=="AL",]$AB)

#two category summarize
bats<- tapply(x=d$HR, INDEX=list(d$lgID,d$bats),FUN=mean)

bats
unique(d$bats)
names(d)
#three category summarize
bats.team<- tapply(x=d$HR, INDEX=list(d$lgID,d$teamID,d$bats),FUN=mean)
bats.team
#aggregate------

team.stats.sum<-aggregate(x=d,[c("AB","H","BB","2B","HR")],by=list(d$teamID),FUN=sum)
team.stats.sum
team.stats.mean<-aggregate(x=d,[c("AB","H","BB","2B","HR")],by=list(d$teamID),FUN=mean)
team.stats.mean


#tidyverse summarise()----
team.sum=summarise(.data=d,)
team.sum=d%>%group_by(teamID)%>%summarise(ABsum=sum(AB),ABmean=mean(AB),
                                          ABsd=sd(AB),ABcount=n())
lg.team.sum=d%>%group_by(lgID,teamID)%>%summarise(ABsum=sum(AB),ABmean=mean(AB),
                                                  ABsd=sd(AB),ABcount=n())

head(team.sum)
str(team.sum)
team.sum$ABsum

#rowsum----
#when you just want to add up the values in each row

rs<-rowsum(d[,c("AB","H","HR","2B","3B")],group=d$teamID)
rs

#counting variables
#use the function "tabulate"
HR.cnts<-tabulate(d$HR)
names(HR.cnts)<-0:(length(HR.cnts)-1)
#table-----
table(d$bats)
table(d[,c("bats","throws")])
length(HR.cnts)
HR.cnts
length(d$teamID)
(unique(d$teamIlengthD))

#aside about the 'names' function------
m<-matrix(nrow=4,ncol=3)
colnames(m)<-c("one","two","three")
rownames(m)<-c("apple","pear","orange","berry")
m
#reshaping your data----
n<-matrix(1:10,nrow=5)

t(n)

v<-1:10
v
t(v)
str(t(v))

#unstack and stack----

s<-d[,c("lgID","teamID","AB","HR","throws")]
head(s)
s.un<-unstack(x=s,form=teamID~HR)

s.un<-unstack(x=s,form=HR~AB)

#melt and cast-----
library(reshape2)


head(s)
#use the "cast" function to change data frame from the long to wide format
s.wide<-dcast(data=s,formula=lgID~teamID,fun.aggregate = mean)
s.wide<-dcast(data=s,value.var="HR",formula=lgID~teamID,fun.aggregate = mean)
