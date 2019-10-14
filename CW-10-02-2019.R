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
d3
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
mt

##join
library(dplyr)
mj<-dplyr::right_join(x=m1,y=m2,by=c('transect.id'))
nrow(m1)+nrow(m2)
nrow(mt)


#2 Oct 2019
#-----

#Summarizing data:

library(tidyverse)
install.packages("nutshell")
library(nutshell)

#Data to be used today:
data("batting.2008")
d<-batting.2008

#tapply---(tidyverse function)
#find sum of all home runs
hr<-tapply(X=d$HR, INDEX=list(d$teamID), FUN=sum)
hr
d

##find quantile values for home runs by team
##fivenum gives you:min, lower-hinge, median, upper-hinge, and max value
hr.q<-tapply(X=d$HR,INDEX=list(d$teamID),FUN=fivenum)
hr.q

# one category summarize
lg.q<-tapply(X=(d$H/d$AB),INDEX=list(d$lgID),FUN=fivenum)
lg.q
head(d$lgID)
summary(d$H/d$AB)
summary(d[d$lgID=="AL",]$H/d[d$lgID=="AL",]$AB)
or 
summary(al.hits/al.bats)

#two category summarize
bats<- tapply(X=d$HR, INDEX=list(d$lgID,d$bats),FUN=mean)
bats
unique(d$bats)
names(d)
#three category summarize (crazy array)
bats.team<- tapply(X=d$HR, INDEX=list(d$lgID,d$teamID,d$bats),FUN=mean)
bats.team

#aggregate------

team.stats.sum<-aggregate(x=d[,c("AB","H","BB","2B","HR")],by=list(d$teamID),FUN=sum)
team.stats.sum
team.stats.mean<-aggregate(x=d[,c("AB","H","BB","2B","HR")],by=list(d$teamID),FUN=mean)
team.stats.mean


#tidyverse summarise()----
team.sum=summarise(.data=d,)
team.sum
team.sum=d%>%group_by(teamID)%>%summarise(ABsum=sum(AB),ABmean=mean(AB),
                                          ABsd=sd(AB),ABcount=n())
team.sum
lg.team.sum=d%>%group_by(lgID,teamID)%>%summarise(ABsum=sum(AB),ABmean=mean(AB),
                                                  ABsd=sd(AB),ABcount=n())
lg.team.sum
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
HR.cnts
names(HR.cnts)<-0:(length(HR.cnts)-1)
names(HR.cnts)

length(HR.cnts)
HR.cnts
length(d$teamID)
length(unique(d$teamID))


#table-----
table(d$bats)
table(d[,c("bats","throws")])

#assign about the 'names' function------
m<-matrix(nrow=4,ncol=3)
colnames(m)<-c("one","two","three")
rownames(m)<-c("apple","pear","orange","berry")
m
#reshaping your data----
n<-matrix(1:10,nrow=5)
n
t(n)

v<-1:10
v
t(v)
str(t(v))

#unstack and stack----

s<-d[,c("lgID","teamID","AB","HR","throws")]
head(s)
s.un<-unstack(x=s,form=teamID~HR)
s.un
s.un<-unstack(x=s,form=HR~AB)

#melt and cast-----

library(reshape2)
head(s)

#use the "cast" function to change data frame from the long to wide format
s.wide<-dcast(data=s,value.var="HR",formula=lgID~teamID,fun.aggregate = mean)

s.wide

#class: reshape

load("fish_data.Rdata")
f<-fish;rm(fish)
names(f)
fs<-f[,c("transect.id","area_fac","depth_fac",
         "parcel.id","parcel.density.m3","parcel.length.m")]
#how to rename a field(or column)
library(tidyverse)
fs<-rename(.data=fs,tid=transect.id)
names(fs)
fs<-rename(.data=fs,area=area_fac)
fs<-rename(.data=fs,depth=depth_fac)
fs<-rename(.data=fs,pid=parcel.id)
fs<-rename(.data=fs,pl=parcel.length.m)
fs<-rename(.data=fs,pd=parcel.density.m3)
fs

#another way to rename columns
names(fs)[1]=c("transect")
names(fs)[1:3]=c("transect",a","z")  #?

#reshaping your data-----
library(reshape2)

#using the function 'melt'(reshape2)to change your
#data frame from wide to a long format
?melt
fs

fs.melt<-melt(data=fs,id.vars = c("tid","pid","area","depth"),
                  measure.vars=c("pl","pd"))
fs.melt
head(fs.melt)
unique(fs.melt$variable)

fs.m$variable<-as.character()

#using dcast function to transform your data from long to the wide format--
?dcast
fs.cast<-dcast(data=fs.melt,formula=tid~variable,value.var = "numbers",
               fun.aggregate = mean)
head(fs.cast)

#spread and gather(tidyverse)

fs.gather=fs%>%group_by(tid,area,depth,pid)%>%gather(key="variable",value="value",pd,pl)
fs.gather

fs.spread=fs.gather%>%spread(variable,value)

#TBD-----
fs.spread=fs.gather

#removingg duplicates-----
## create duplicates with command below
o1<-fs[1,]
o2<-fs[1,]
o3<-fs[1,]
o4<-fs[2:10,]
#bind the individual objects back together using functiom "rbind"
o<-rbind(o1,o2,o3,o4)
o
#now the first 3 rows are duplicate observations
no.dups<-o[!duplicated(o),]
no.dups
dups<-o[duplicated(o),]
dups

?complete.cases   # return datas without NAs.

#complete.cases identify column with NAs i.e data with observations missing(NAs)
fs[2,]$pd<-NA
fs[4,]$pl<-NA
fs.complete<-complete.cases(fs)
head(fs.complete)
fs.comlete<-fs[complete.cases(fs),]
head(fs.comlete)               

# sorting data
attach(mtcars)

#sort by mpg
nd<-mtcars[order(mpg),]
nd

#using the arrange function
nd.arrange<-arrange(.data=mtcars,mpg)# ascending
nd.arrange

nd.arrange.desc=arrange(.data=mtcars,desc(cyl)) # descending            
nd.arrange.desc


nd.m.c<-arrange(.data=mtcars,mpg,desc(cyl)) # mpg ascending; cyl descending
nd.m.c

