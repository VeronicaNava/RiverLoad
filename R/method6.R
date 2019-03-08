method6 <-
function(db, ncomp, period) {
n<-nrow(db)
intflow<-(with(db, data.frame(approx(datetime, db[,2], xout=seq(as.POSIXct(datetime[1], tz="CET"), as.POSIXct(datetime[n], tz="CET"), "days"), rule=2))))
dateday<-(with(db, data.frame(approx(datetime, db[,2], xout=seq(as.POSIXct(datetime[1], tz="CET"), as.POSIXct(datetime[n], tz="CET"), "days"), rule=2))))
colnames(dateday)[1]<-c("datetime")
colnames(intflow)<-c("datetime", "inter.flow")
nint<-nrow(intflow)
intconc<-matrix(nrow=nint, ncol=(ncomp))
for (i in 3:(ncomp+2)) {
vrt<-with(db, data.frame(approx(datetime, db[,i], xout=seq(datetime[1], datetime[n], "days"), rule=2)))
intconc[,i-2]<-vrt[,2]
if (i==(ncomp+2)) break
}
interpolate<-cbind.data.frame(intflow$datetime, (intconc)) 
colnames(interpolate)[1]<-"datetime"
colnames(interpolate)[2:(ncomp+1)]<-c(names(db)[3:(ncomp+2)])

db$newdate<-format(as.POSIXct(db$datetime), format="%Y-%m-%d")
flowday<-as.matrix(intflow$inter.flow*86400) 

if(missing(period)){
flowday<-as.matrix(intflow[,2]*86400) 
load<-(as.vector(flowday)*intconc)
colnames(load)<-c(names(db)[3:(ncomp+2)])
fluxM6<-apply(load,2, sum)
return(fluxM6)}

else if (period=="month") {
flowday<-as.matrix(intflow[,2]*86400) 
load<-(as.vector(flowday)*intconc)
loadtot<-cbind.data.frame(dateday$datetime, load) 
colnames(loadtot)[1]<-c("datetime")
loadtot$datetime<-format(as.POSIXct(loadtot$datetime), format="%Y-%m")
forrow<-agg.data<-aggregate(loadtot[,2]~datetime, loadtot, sum)
agg.dataC<-matrix(nrow=nrow(forrow), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(loadtot[,i+1]~datetime, loadtot, sum)
agg.dataC[,i]<-as.matrix(agg.data[,2])}
colnames(agg.dataC)<-c(names(db)[3:(ncomp+2)])
rownames(agg.dataC)<-forrow$datetime
return(agg.dataC)
}


else if (period=="year") {
flowday<-as.matrix(intflow[,2]*86400) 
load<-(as.vector(flowday)*intconc)
loadtot<-cbind.data.frame(dateday$datetime, load) 
colnames(loadtot)[1]<-c("datetime")
loadtot$datetime<-format(as.POSIXct(loadtot$datetime), format="%Y")
forrow<-agg.data<-aggregate(loadtot[,2]~datetime, loadtot, sum)
agg.dataC<-matrix(nrow=nrow(forrow), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(loadtot[,i+1]~datetime, loadtot, sum)
agg.dataC[,i]<-as.matrix(agg.data[,2])}
colnames(agg.dataC)<-c(names(db)[3:(ncomp+2)])
rownames(agg.dataC)<-forrow$datetime
return(agg.dataC)
}}
