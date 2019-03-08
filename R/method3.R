method3 <-
function(db, ncomp, period) {
mat.met<-matrix(nrow=1, ncol=(ncomp))
colnames(mat.met)<-c(names(db)[3:(ncomp+2)])
notNA<-na.omit(db)

if (missing(period)){
NonNAindex <- which(!is.na(db[,3]))
vr<-(matrix(nrow=length(NonNAindex), ncol=1))
colnames(vr)<-c("flowmean")
for(i in 2:length(NonNAindex)) {
analyze<-mean(db[NonNAindex[i]:NonNAindex[i-1],2])
vr[i,1]<-analyze
if (i==length(NonNAindex)) break
}
vr[is.na(vr)]<-c(db[1,2])
vrmcd<-vr*86400
dbsel.eliminationNA<-notNA[,-c(1,2)]
prodflowconc<-vrmcd*dbsel.eliminationNA
loadperiod<-apply(prodflowconc, 2, sum)
nday<-nrow(notNA)
differ<-as.numeric(db[nrow(db),1]-db[1,1])
method3<-(loadperiod/nday)*differ
mat.met<-matrix(nrow=1, ncol=(ncomp))
mat.met<-matrix(method3, nrow=1, ncol=ncomp)
colnames(mat.met)<-c(names(db)[3:(ncomp+2)])
method3<-as.numeric(mat.met)
names(method3)<-c(names(db)[3:(ncomp+2)])
return(method3)
}


else if(period=="month"){
NonNAindex <- which(!is.na(db[,3]))
vr<-(matrix(nrow=length(NonNAindex), ncol=1))
colnames(vr)<-c("flowmean")
for(i in 2:length(NonNAindex)) {
analyze<-mean(db[NonNAindex[i]:NonNAindex[i-1],2])
vr[i,1]<-analyze
if (i==length(NonNAindex)) break
}
vr[is.na(vr)]<-c(db[1,2])
vrmcd<-vr*86400
dbsel.eliminationNA<-notNA[,-c(1,2)]
prodflowconc<-vrmcd*dbsel.eliminationNA
loaddate<-cbind.data.frame(notNA$datetime, prodflowconc)
colnames(loaddate)[1]<-c("datetime")
loaddate$newdate<-format(as.POSIXct(loaddate$datetime), format="%Y-%m")
aggrg.data<-matrix(nrow=length(unique(loaddate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
loadmonth<-aggregate(loaddate[,i+1]~newdate, loaddate, sum)
aggrg.data[,i]<-loadmonth[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

dateaggr<-(aggregate(loaddate[,2]~newdate, loaddate, sum))
dateaggr$newdate<-as.Date(paste(dateaggr$newdate, "-01", sep=""))
dateaggr$newdate<-as.POSIXct(dateaggr$newdate, format ="%Y-%m")


dateaggr1<-(aggregate(loaddate[,2]~newdate, loaddate, sum))
nformonth<-aggregate(loaddate[,2]~newdate, loaddate, function(x) length(unique(x)))
n<-nrow(db)
datemonth<-seq(as.POSIXct(db$datetime[1], tz="CET"), as.POSIXct(db$datetime[n], tz="CET"), "days")
b<-length(datemonth)
dateplus<-as.Date(datemonth[b])+2
daymonths<- as.numeric(round(diff(seq(as.POSIXct(datemonth[1], tz="CET"), as.POSIXct(dateplus, tz="CET"), "month")), digits=0))

method3<-(aggrg.data/nformonth[,2])*daymonths
rownames(method3)<-((dateaggr1)[,1])
return(method3)
}


else if(period=="year") {
NonNAindex <- which(!is.na(db[,3]))
vr<-(matrix(nrow=length(NonNAindex), ncol=1))
colnames(vr)<-c("flowmean")
for(i in 2:length(NonNAindex)) {
analyze<-mean(db[NonNAindex[i]:NonNAindex[i-1],2])
vr[i,1]<-analyze
if (i==length(NonNAindex)) break
}
vr[is.na(vr)]<-c(db[1,2])
vrmcd<-vr*86400
dbsel.eliminationNA<-notNA[,-c(1,2)]
prodflowconc<-vrmcd*dbsel.eliminationNA
loaddate<-cbind.data.frame(notNA$datetime, prodflowconc)
colnames(loaddate)[1]<-c("datetime")
loaddate$newdate<-format(as.POSIXct(loaddate$datetime), format="%Y")
aggrg.data<-matrix(nrow=length(unique(loaddate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
loadmonth<-aggregate(loaddate[,i+1]~newdate, loaddate, sum)
aggrg.data[,i]<-loadmonth[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

dateaggr1<-(aggregate(loaddate[,2]~newdate, loaddate, sum))

nforyear<-aggregate(loaddate[,2]~newdate, loaddate, function(x) length(unique(x)))
is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(dateaggr1))){
if (is.leapyear(as.numeric(dateaggr1$newdate[i]))== T) {
method3<-((aggrg.data/nforyear[,2])*(366))}
else{
method3<-((aggrg.data/nforyear[,2])*(365))
}}
rownames(method3)<-dateaggr1[,1]
colnames(method3)<-c(names(db)[3:(ncomp+2)])
return(method3)
}}
