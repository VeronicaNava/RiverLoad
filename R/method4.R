method4 <-
function(db, ncomp, period) {
notNA<-na.omit(db)
if (missing(ncomp)) {print("ncomp is missing.")}

if (missing(period)){
mat.conc<-matrix(nrow=(nrow(notNA)), ncol=(ncomp))
for (i in 3:(2+ncomp)) {
mat.conc[,i-2]<-notNA[,i]/(nrow(notNA))
conc.sum<-colSums(mat.conc)
if (i==(2+ncomp))
break}
mediaport<-mean(db$flow)
fluxM4gsec<-(mediaport*conc.sum)
difference<-as.numeric(db[nrow(db),1]-db[1,1])
method4<-(fluxM4gsec*(difference)*86400)
mat.met<-matrix(nrow=1, ncol=(ncomp))
mat.met<-matrix(method4, nrow=1, ncol=ncomp)
colnames(mat.met)<-c(names(db)[3:(ncomp+2)])
method4<-as.numeric(mat.met)
names(method4)<-c(names(db)[3:(ncomp+2)])
return(method4)
}


else if(period=="month") {
new<-notNA
new$newdate<-format(as.POSIXct(new$datetime), format="%Y-%m")

maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

result <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
result[[i]]<-seldata[,-which(names(seldata) %in% c("datetime", "flow", "newdate"))]/index[i]
}
if (is.null(nrow(result[[1]]))==T) {
mat.conc<-as.matrix(unlist(result))}
if (is.null(nrow(result[[1]]))==F){
mat.conc<-do.call(rbind, result)}

concdate<-cbind.data.frame(notNA$datetime, mat.conc)
colnames(concdate)[1]<-c("datetime")
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y-%m")
aggrg.data<-matrix(nrow=length(unique(concdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(concdate[,i+1]~newdate, concdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

db$year<-format(as.POSIXct(db$datetime), format="%Y")
meanflowyear<-aggregate(flow~year, db, mean)
numbyear<-length(unique(meanflowyear$year))


dateaggr<-(aggregate(concdate[,2]~newdate, concdate, sum))
agg.data1<-cbind.data.frame(dateaggr$newdate, aggrg.data)
colnames(agg.data1)[1]<-c("newdate")
agg.data1$newdate<-as.Date(paste(agg.data1$newdate, "-01", sep=""))
agg.data1$newdate<-as.POSIXct(agg.data1$newdate, format ="%Y-%m")
year<-as.numeric(format(agg.data1$newdate, "%Y"))

bindyear<-cbind(year, agg.data1)
nforyear<-aggregate(bindyear[,3]~year, bindyear, function(x) length(unique(x)))

valueyear<-unique(year)
load.data<-as.vector(bindyear[,-which(names(bindyear) %in% c("year", "newdate"))])
for(i in 1:numbyear){
ind<-which(bindyear$year==valueyear[i], arr.ind=T)
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
method4<-as.matrix((load.data*(daymonths)*86400))
rownames(method4)<-((dateaggr)[,1])
colnames(method4)<-(names(db)[3:(ncomp+2)])
return(method4)
}


else if(period=="year") {
new<-notNA
new$newdate<-format(as.POSIXct(new$datetime), format="%Y")

maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

result <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
result[[i]]<-seldata[,-which(names(seldata) %in% c("datetime", "flow", "newdate"))]/index[i]
}
if (is.null(nrow(result[[1]]))==T) {
mat.conc<-as.matrix(unlist(result))}
if (is.null(nrow(result[[1]]))==F){
mat.conc<-do.call(rbind, result)}

concdate<-cbind.data.frame(notNA$datetime, mat.conc)
colnames(concdate)[1]<-c("datetime")
concdate$year<-format(as.POSIXct(concdate$datetime), format="%Y")
aggrg.data<-matrix(nrow=length(unique(concdate$year)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(concdate[,i+1]~year, concdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

meanflowyear<-aggregate(new$flow~newdate, new, mean)
colnames(meanflowyear)[2]<-c("flow")
numbyear<-length(unique(meanflowyear$newdate))


agg.data1<-cbind.data.frame(meanflowyear$newdate, aggrg.data)
colnames(agg.data1)[1]<-c("newdate")

load.data<-(agg.data1[,-which(names(agg.data1) %in% c("newdate"))]) *(meanflowyear$flow)

is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(agg.data1))){
if (is.leapyear(as.numeric(agg.data1$newdate[i]))== T) {
method4<-(load.data*(366)*86400)}
else{
method4<-(load.data*(365)*86400)
}}

method4<-as.matrix(method4)
rownames(method4)<-((agg.data1)[,1])
colnames(method4)<-c(names(db)[3:(ncomp+2)])
return(method4)
}}
