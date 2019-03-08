method2 <-
function(db, ncomp, period) {
notNA<-na.omit(db)

if (missing(ncomp)) {print("ncomp is missing.")}

if (missing(period)) {
mat.conc<-matrix(nrow=(nrow(notNA)), ncol=(ncomp))
for (i in 3:(2+ncomp)) {
mat.conc[,i-2]<-((notNA$flow)*(notNA[,i])/nrow(notNA))
sumCQ<-colSums(mat.conc)
if (i==(2+ncomp))
break}
difference<-as.numeric(db[nrow(db),1]-db[1,1])
method2<-(sumCQ*(difference)*86400)
mat.meth<-matrix(nrow=1, ncol=(ncomp))
mat.met<-matrix(method2, nrow=1, ncol=ncomp)
colnames(mat.met)<-c(names(db)[3:(ncomp+2)])
method2<-as.numeric(mat.met)
names(method2)<-c(names(db)[3:(ncomp+2)])
return(method2)}


else if(period=="month"){
new<-notNA
new$newdate<-format(as.POSIXct(new$datetime), format="%Y-%m")

maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

result <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
result[[i]]<-(seldata[,-which(names(seldata) %in% c("datetime", "flow", "newdate"))]*seldata[,which(names(seldata) %in% c("flow"))])/index[i]
}
if (is.null(nrow(result[[1]]))==T) {
mat.conc<-as.matrix(unlist(result))}
if (is.null(nrow(result[[1]]))==F){
mat.conc<-do.call(rbind, result)}

concdate<-cbind.data.frame(notNA$datetime, mat.conc)
colnames(concdate)[1]<-"datetime"
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y-%m")
aggrg.data<-matrix(nrow=length(unique(concdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
summonth<-aggregate(concdate[,i+1]~newdate, concdate, sum)
aggrg.data[,i]<-summonth[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

dateaggr1<-(aggregate(concdate[,2]~newdate, concdate, sum))
dateaggr<-(aggregate(concdate[,2]~newdate, concdate, sum))
dateaggr$newdate<-as.Date(paste(dateaggr$newdate, "-01", sep=""))
dateaggr$newdate<-as.POSIXct(dateaggr$newdate, format ="%Y-%m")

n<-nrow(db)
datemonth<-seq(as.POSIXct(db$datetime[1], tz="CET"), as.POSIXct(db$datetime[n], tz="CET"), "days")
b<-length(datemonth)
dateplus<-as.Date(datemonth[b])+2
daymonths<- as.numeric(round(diff(seq(as.POSIXct(datemonth[1], tz="CET"), as.POSIXct(dateplus, tz="CET"), "month")), digits=0))

method2<-(aggrg.data*(daymonths)*86400)
rownames(method2)<-((dateaggr1)[,1])
return(method2)

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
result[[i]]<-(seldata[,-which(names(seldata) %in% c("datetime", "flow", "newdate"))]*seldata[,which(names(seldata) %in% c("flow"))])/index[i]
}
if (is.null(nrow(result[[1]]))==T) {
mat.conc<-as.matrix(unlist(result))}
if (is.null(nrow(result[[1]]))==F){
mat.conc<-do.call(rbind, result)}

concdate<-cbind.data.frame(notNA$datetime, mat.conc)
colnames(concdate)[1]<-"datetime"
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y")
aggrg.data<-matrix(nrow=length(unique(concdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
summonth<-aggregate(concdate[,i+1]~newdate, concdate, sum)
aggrg.data[,i]<-summonth[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

dateaggr1<-(aggregate(concdate[,2]~newdate, concdate, sum))
dateaggr<-(aggregate(concdate[,2]~newdate, concdate, sum))

is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(dateaggr))){
if (is.leapyear(as.numeric(dateaggr$newdate[i]))== T) {
method2<-(aggrg.data*(366)*86400)}
else{
method2<-(aggrg.data*(365)*86400)
}}
rownames(method2)<-dateaggr1[,1]
colnames(method2)<-c(names(db)[3:(ncomp+2)])
return(method2)
}
}
