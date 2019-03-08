method1 <-
function(db, ncomp, period) {
notNA<-na.omit(db)
if (missing(ncomp)) {print("ncomp is missing.")}

if (missing(period)) {
mat.conc<-matrix(nrow=(nrow(notNA)), ncol=(ncomp))
for (i in 3:(2+ncomp)) {
mat.conc[,i-2]<-notNA[,i]/(nrow(notNA))
conc.sumNA<-colSums(mat.conc)
if (i==(2+ncomp))
break}
conc.sum<-as.vector(na.omit(conc.sumNA))
flow.n<-notNA$flow/(nrow(notNA))
flow.sum<-sum(flow.n)
flux.gsec<-(conc.sum*flow.sum)
differ<-as.numeric(db[nrow(db),1]-db[1,1])
method1<-(flux.gsec*(differ)*86400)
mat.meth<-matrix(nrow=1, ncol=(ncomp))
mat.met<-matrix(method1, nrow=1, ncol=ncomp)
colnames(mat.met)<-c(names(db)[3:(ncomp+2)])
method1<-as.numeric(mat.met)
names(method1)<-c(names(db)[3:(ncomp+2)])
return(method1)}

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


resultF <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
resultF[[i]]<-seldata$flow/index[i]
}
flow.n <- matrix(unlist(resultF), ncol=1, byrow=TRUE)
colnames(flow.n)<-c("flow")


flowdate<-cbind.data.frame(notNA$datetime, flow.n)
colnames(flowdate)<-c("datetime", "flow")
flowdate$newdate<-format(as.POSIXct(flowdate$datetime), format="%Y-%m")
aggrg.data1<-(aggregate(flow~newdate, flowdate, sum))
aggrg.data2<-(aggregate(flow~newdate, flowdate, sum))
aggrg.data2$newdate<-as.Date(paste(aggrg.data2$newdate, "-01", sep=""))
aggrg.data2$newdate<-as.POSIXct(aggrg.data2$newdate, format ="%Y-%m")
aggrg.flow<-as.vector(aggrg.data2[,-which(names(aggrg.data2) %in% c("newdate"))])


load<-(aggrg.flow*aggrg.data)
n<-nrow(db)
datemonth<-seq(as.POSIXct(db$datetime[1], tz="CET"), as.POSIXct(db$datetime[n], tz="CET"), "days")
b<-length(datemonth)
dateplus<-as.Date(datemonth[b])+2
daymonths<- as.numeric(round(diff(seq(as.POSIXct(datemonth[1], tz="CET"), as.POSIXct(dateplus, tz="CET"), "month")), digits=0))
method1<-(load*(daymonths)*86400)
rownames(method1)<-((aggrg.data1)[,1])
return(method1)
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
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y")
aggrg.data<-matrix(nrow=length(unique(concdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(concdate[,i+1]~newdate, concdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp+1)) break}

resultF <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
resultF[[i]]<-seldata$flow/index[i]
}
flow.n <- matrix(unlist(resultF), ncol=1, byrow=TRUE)
colnames(flow.n)<-c("flow")

flowdate<-cbind.data.frame(notNA$datetime, flow.n)
colnames(flowdate)<-c("datetime", "flow")
flowdate$newdate<-format(as.POSIXct(flowdate$datetime), format="%Y")
aggrg.data2<-(aggregate(flow~newdate, flowdate, sum))
aggrg.flow<-as.vector(aggrg.data2[,-which(names(aggrg.data2) %in% c("newdate"))])

load<-(aggrg.flow*aggrg.data)
is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(aggrg.data2))){
if (is.leapyear(as.numeric(aggrg.data2$newdate[i]))== T) {
method1<-(load*(366)*86400)}
else{
method1<-(load*(365)*86400)
}}

rownames(method1)<-aggrg.data2[,1]
colnames(method1)<-c(names(db)[3:(ncomp+2)])
return(method1)
}}
