method5 <-
function(db, ncomp, period) {
notNA<-na.omit(db)
if (missing(ncomp)) {print("ncomp is missing.")}

if (missing(period)) {
matCQ<-matrix(nrow=(nrow(notNA)), ncol=(ncomp))
for (i in 3:(2+ncomp)) {
matCQ[,i-2]<-(notNA$flow)*(notNA[,i])
sum.CQ<-colSums(matCQ)
if (i==(2+ncomp))
break}
sum.flow<-sum(notNA$flow)
fluxM5gsec<-((sum.CQ/sum.flow)*mean(db$flow))
difference<-as.numeric(db[nrow(db),1]-db[1,1])
method5<-(fluxM5gsec*(difference)*86400)
mat.met<-matrix(nrow=1, ncol=(ncomp))
mat.met<-matrix(method5, nrow=1, ncol=ncomp)
colnames(mat.met)<-c(names(db)[3:(ncomp+2)])
method5N<-as.numeric(mat.met)
names(method5N)<-c(names(db)[3:(ncomp+2)])
return(method5N)
}


else if (period=="month") {
matCQ<-matrix(nrow=(nrow(notNA)), ncol=(ncomp))
for (i in 3:(2+ncomp)) {
matCQ[,i-2]<-(notNA$flow)*(notNA[,i])
if (i==(2+ncomp))
break}
concdate<-cbind.data.frame(notNA$datetime, matCQ)
colnames(concdate)[1]<-"datetime"
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y-%m")
aggrg.data<-matrix(nrow=length(unique(concdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
summonth<-aggregate(concdate[,i+1]~newdate, concdate, sum)
aggrg.data[,i]<-summonth[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

flowdate<-cbind.data.frame(notNA$datetime, notNA$flow)
colnames(flowdate)<-c("datetime", "flow")
flowdate$newdate<-format(as.POSIXct(flowdate$datetime), format="%Y-%m")
aggrg.data1<-(aggregate(flow~newdate, flowdate, sum))
aggrg.data2<-(aggregate(flow~newdate, flowdate, sum))

division<-(aggrg.data/(aggrg.data1$flow))

aggrg.data1$newdate<-as.Date(paste(aggrg.data1$newdate, "-01", sep=""))
aggrg.data1$newdate<-as.POSIXct(aggrg.data1$newdate, format ="%Y-%m")
year<-as.numeric(format(aggrg.data1$newdate, "%Y"))

bindyear<-cbind(year,  division)
nforyear<-aggregate(bindyear[,2]~year, bindyear, function(x) length(unique(x)))
numbyear<-length(unique(nforyear$year))
db$year<-format(as.POSIXct(db$datetime), format="%Y")
meanflowyear<-aggregate(flow~year, db, mean)


valueyear<-unique(year)
load.data<-bindyear[,-1]
for(i in 1:numbyear){
ind<-which(bindyear[,1]==valueyear[i], arr.ind=T)
flowsel<-subset(meanflowyear, year==valueyear[i])[,2]
if (is.null(ncol(load.data))==T) {
load.data<-load.data*flowsel}
if (is.null(ncol(load.data))==F) {
load.data[ind,]<-(load.data[ind,]*(flowsel))
}}


n<-nrow(db)
datemonth<-seq(as.POSIXct(db$datetime[1], tz="CET"), as.POSIXct(db$datetime[n], tz="CET"), "days")
b<-length(datemonth)
dateplus<-as.Date(datemonth[b])+2
daymonths<- as.numeric(round(diff(seq(as.POSIXct(datemonth[1], tz="CET"), as.POSIXct(dateplus, tz="CET"), "month")), digits=0))
method5<-as.matrix(load.data*(daymonths)*86400)
rownames(method5)<-((aggrg.data2)[,1])
return(method5)}



else if (period=="year") {
matCQ<-matrix(nrow=(nrow(notNA)), ncol=(ncomp))
for (i in 3:(2+ncomp)) {
matCQ[,i-2]<-(notNA$flow)*(notNA[,i])
if (i==(2+ncomp))
break}
concdate<-cbind.data.frame(notNA$datetime, matCQ)
colnames(concdate)[1]<-"datetime"
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y")
aggrg.data<-matrix(nrow=length(unique(concdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
summonth<-aggregate(concdate[,i+1]~newdate, concdate, sum)
aggrg.data[,i]<-summonth[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

flowdate<-cbind.data.frame(notNA$datetime, notNA$flow)
colnames(flowdate)<-c("datetime", "flow")
flowdate$newdate<-format(as.POSIXct(flowdate$datetime), format="%Y")
aggrg.data1<-(aggregate(flow~newdate, flowdate, sum))

division<-(aggrg.data/(aggrg.data1$flow))

bindyear<-cbind.data.frame(aggrg.data1$newdate,  division)
colnames(bindyear)[1]<-"year"
nforyear<-aggregate(bindyear[,2]~year, bindyear, function(x) length(unique(x)))
numbyear<-length(unique(nforyear$year))
db$year<-format(as.POSIXct(db$datetime), format="%Y")
meanflowyear<-aggregate(flow~year, db, mean)

valueyear<-unique(aggrg.data1$newdate)
load.data<-bindyear[,-1]
for(i in 1:numbyear){
ind<-which(bindyear[,1]==valueyear[i], arr.ind=T)
flowsel<-subset(meanflowyear, year==valueyear[i])[,2]
if (is.null(ncol(load.data))==T) {
load.data<-load.data*flowsel}
if (is.null(ncol(load.data))==F) {
load.data[ind,]<-(load.data[ind,]*(flowsel))
}}




is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(aggrg.data1))){
if (is.leapyear(as.numeric(aggrg.data1$newdate[i]))== T) {
method5<-(load.data*(366)*86400)}
else{
method5<-(load.data*(365)*86400)
}}

method5<-as.matrix(method5)
rownames(method5)<-((aggrg.data1)[,1])
colnames(method5)<-c(names(db)[3:(ncomp+2)])
return(method5)
}
}
