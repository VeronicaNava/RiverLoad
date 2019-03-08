beale.period <-
function(db, ncomp, period) {
if (missing(ncomp)) {print("ncomp is missing.")}

if (period=="month") {
inter<-na.omit(db)
new<-inter
new$newdate<-format(as.POSIXct(new$datetime), format="%Y-%m")
inter$newdate<-format(as.POSIXct(inter$datetime), format="%Y-%m")
intermean<-matrix(nrow=length(is.na(unique(inter$newdate))), ncol=(ncomp+1))
for(i in 1:(ncomp+1)){
mat<-aggregate(inter[,i+1]~newdate, inter, mean)
intermean[,i]<-mat[,2]
colnames(intermean)<-c(names(db)[2:(ncomp+2)])
if (i==(ncomp+1)) break}
flow.inter<-intermean[,1]

otherunion<-db
otherunion$newdate<-format(as.POSIXct(otherunion$datetime), format="%Y-%m")
flow.union<-aggregate(otherunion$flow~newdate, otherunion, mean)
colnames(flow.union)[2]<-"flow"

sel.inter<-inter[,-which(names(inter) %in% c("datetime", "flow", "newdate"))]
load<-as.vector(inter$flow)*sel.inter

bindload<-cbind.data.frame(inter$datetime, load)
colnames(bindload)[1]<-"datetime"
bindload$datetime<-format(as.POSIXct(bindload$datetime), format="%Y-%m")
loadmean<-matrix(nrow=length(unique(bindload$datetime)), ncol=ncomp)
for(i in 1:(ncomp)){
rt<-aggregate(bindload[,i+1]~datetime, bindload, mean)
loadmean[,i]<-rt[,2]
colnames(loadmean)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp)) break}

merging<-merge(mat, flow.union, by="newdate")
union.flow<-merging[,3]
divlq<-loadmean/flow.inter
prodlq<-loadmean*flow.inter
load.factor<-as.vector(union.flow)*(divlq)
n<-nrow(inter)


covariance<-matrix(nrow=length(unique(bindload$datetime)), ncol=ncomp)
value<-unique(bindload$datetime)
numb<-length(unique(bindload$datetime))
selbind<-bindload[,-1]
for (i in 1:numb) {
ind<-which(bindload$datetime==value[i], arr.ind=T)
if (is.null(ncol(selbind))==T) {
covariance[i,]<-cov(selbind[ind],inter$flow[ind])
}
if (is.null(ncol(selbind))==F) {
covariance[i,]<-cov(selbind[ind,],inter$flow[ind])}}

variance<-aggregate(inter$flow~newdate, inter, function(x) var(x))


maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

bias.factor<-((1+((1/index)*(covariance/prodlq)))/(1+(1/index)*(variance[,2]/((flow.inter)^2))))
beale<-load.factor*bias.factor

flow.union$newdate<-as.Date(paste(flow.union$newdate, "-01", sep=""))
flow.union$newdate<-as.POSIXct(flow.union$newdate, format ="%Y-%m")


n<-nrow(db)
datemonth<-seq(as.POSIXct(db$datetime[1], tz="CET"), as.POSIXct(db$datetime[n], tz="CET"), "days")
b<-length(datemonth)
dateplus<-as.Date(datemonth[b])+2
daymonths<- as.numeric(round(diff(seq(as.POSIXct(datemonth[1], tz="CET"), as.POSIXct(dateplus, tz="CET"), "month")), digits=0))
nd<-which(!is.na(beale[,1]))
beale.ratio<-as.matrix((beale)[nd,]*86400*daymonths[nd])
rownames(beale.ratio)<-(rt$datetime[nd])
colnames(beale.ratio)<-c(names(db)[3:(ncomp+2)])
return(beale.ratio)}




else if (period=="year") {
inter<-na.omit(db)
new<-inter
new$newdate<-format(as.POSIXct(new$datetime), format="%Y")
inter$newdate<-format(as.POSIXct(inter$datetime), format="%Y")
intermean<-matrix(nrow=length(is.na(unique(inter$newdate))), ncol=(ncomp+1))
for(i in 1:(ncomp+1)){
mat<-aggregate(inter[,i+1]~newdate, inter, mean)
intermean[,i]<-mat[,2]
colnames(intermean)<-c(names(db)[2:(ncomp+2)])
if (i==(ncomp+1)) break}
conc.inter<-intermean[,-1]
flow.inter<-intermean[,1]

db$newdate<-format(as.POSIXct(db$datetime), format="%Y")
flow.union<-aggregate(db$flow~newdate, db, mean)
colnames(flow.union)[2]<-"flow"

sel.inter<-inter[,-which(names(inter) %in% c("datetime", "flow", "newdate"))]
load<-as.vector(inter$flow)*sel.inter

bindload<-cbind.data.frame(inter$datetime, load)
colnames(bindload)[1]<-"datetime"
bindload$datetime<-format(as.POSIXct(bindload$datetime), format="%Y")
loadmean<-matrix(nrow=length(unique(bindload$datetime)), ncol=ncomp)
for(i in 1:(ncomp)){
rt<-aggregate(bindload[,i+1]~datetime, bindload, mean)
loadmean[,i]<-rt[,2]
colnames(loadmean)<-c(names(db)[3:(ncomp+2)])
if (i==(ncomp)) break}


divlq<-loadmean/flow.inter
prodlq<-loadmean*flow.inter
load.factor<-as.vector(flow.union$flow)*(divlq)
n<-nrow(inter)


covariance<-matrix(nrow=length(unique(bindload$datetime)), ncol=ncomp)
value<-unique(bindload$datetime)
numb<-length(unique(bindload$datetime))
selbind<-bindload[,-1]
for (i in 1:numb) {
ind<-which(bindload$datetime==value[i], arr.ind=T)
if (is.null(ncol(selbind))==T) {
covariance[i,]<-cov(selbind[ind],inter$flow[ind])
}
if (is.null(ncol(selbind))==F) {
covariance[i,]<-cov(selbind[ind,],inter$flow[ind])}}

variance<-aggregate(inter$flow~newdate, inter, function(x) var(x))

maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

bias.factor<-((1+((1/index)*(covariance/prodlq)))/(1+(1/index)*(variance[,2]/((flow.inter)^2))))

beale<-load.factor*bias.factor

is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(flow.union))){
if (is.leapyear(as.numeric(flow.union$newdate[i]))== T) {
methodbeale<-(beale*(366)*86400)}
else{
methodbeale<-(beale*(365)*86400)
}}

rownames(methodbeale)<-(flow.union$newdate)
return(methodbeale)
}}
