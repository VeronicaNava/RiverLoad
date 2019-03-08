beale.ratio <-
function(db, ncomp, period) {
if (missing(ncomp)) {print("ncomp is missing.")}
inter<-na.omit(db)

if (missing(period)) {
mean.flow<-mean(inter$flow)
sel.data<-inter[, -which(names(inter) %in% c("datetime", "flow", "newdate"))]
mat.conc<-matrix(nrow=(nrow(inter)), ncol=(ncomp))
load<-as.matrix(sel.data*as.vector(inter$flow))
mean.load<-apply(load, 2, mean)

total.flow<-mean(db$flow)

load.factor<-total.flow*(mean.load/mean.flow)

n<-nrow(inter)
prod.lq<-mean.load*as.vector(mean.flow)

cov.lq<-cov(load, inter$flow)

var.q<-var(inter$flow)
bias.factor<-((1+((1/n)*(cov.lq/prod.lq)))/(1+(1/n)*(var.q/((mean.flow)^2))))
beale<-load.factor*bias.factor
beale<-matrix(beale, nrow=1, ncol=ncomp)
differ<-as.numeric(db[nrow(db),1]-db[1,1])
beale.ratio<-beale*86400*differ
colnames(beale.ratio)<-c(names(db)[3:(ncomp+2)])
method7N<-as.numeric(beale.ratio)
names(method7N)<-c(names(db)[3:(ncomp+2)])
return(method7N)
}


else if (period=="month") {
inter$newdate<-format(as.POSIXct(inter$datetime), format="%Y-%m")
intermean<-matrix(nrow=length(is.na(unique(inter$newdate))), ncol=(ncomp+1))
for(i in 1:(ncomp+1)){
mat<-aggregate(inter[,i+1]~newdate, inter, mean)
intermean[,i]<-mat[,2]
colnames(intermean)<-c(names(db)[2:(ncomp+2)])
if (i==(ncomp+1)) break}
flow.inter<-intermean[,1]

db$newdate<-format(as.POSIXct(db$datetime), format="%Y-%m")
flow.union<-aggregate(db$flow~newdate, db, mean)
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


mean.flow<-mean(inter$flow)
sel.data<-inter[, -which(names(inter) %in% c("datetime", "flow", "newdate"))]
mat.conc<-matrix(nrow=(nrow(inter)), ncol=(ncomp))
load<-as.matrix(sel.data*as.vector(inter$flow))
mean.load<-apply(load, 2, mean)

prod.lq<-mean.load*as.vector(mean.flow)

cov.lq<-cov(load, inter$flow)

var.q<-var(inter$flow)
bias.factor<-((1+((1/n)*(cov.lq/prod.lq)))/(1+(1/n)*(var.q/((mean.flow)^2))))

beale<-matrix(nrow=nrow(load.factor), ncol=ncol(load.factor))
for (i in 1:ncomp) {
beale[,i]<-cbind(load.factor[,i]*bias.factor[i])}

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


merging<-merge(mat, flow.union, by="newdate")
union.flow<-merging[,3]
divlq<-loadmean/flow.inter
prodlq<-loadmean*flow.inter
load.factor<-as.vector(union.flow)*(divlq)
n<-nrow(inter)


mean.flow<-mean(inter$flow)
sel.data<-inter[, -which(names(inter) %in% c("datetime", "flow", "newdate"))]
mat.conc<-matrix(nrow=(nrow(inter)), ncol=(ncomp))
load<-as.matrix(sel.data*as.vector(inter$flow))
mean.load<-apply(load, 2, mean)

prod.lq<-mean.load*as.vector(mean.flow)

cov.lq<-cov(load, inter$flow)

var.q<-var(inter$flow)
bias.factor<-((1+((1/n)*(cov.lq/prod.lq)))/(1+(1/n)*(var.q/((mean.flow)^2))))

beale<-matrix(nrow=nrow(load.factor), ncol=ncol(load.factor))
for (i in 1:ncomp) {
beale[,i]<-cbind(load.factor[,i]*bias.factor[i])}

is.leapyear=function(year) {return(((year %% 4==0) &(year%%100 !=0)) |(year %% 400==0))}
for(i in 1:(nrow(flow.union))){
if (is.leapyear(as.numeric(flow.union$newdate[i]))== T) {
methodbeale<-(beale*(366)*86400)}
else{
methodbeale<-(beale*(365)*86400)
}}


rownames(methodbeale)<-(mat$newdate)
colnames(methodbeale)<-c(names(db)[3:(ncomp+2)])
return(methodbeale)
}}
