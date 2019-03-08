ferguson <-
function(db, ncomp, period){
notNA<-na.omit(db)
db.i<-notNA[!rowSums(notNA[,-(1)] == 0) >= 1,]
index.u<-(which(db[,-which(names(db) %in% c("datetime"))]==0))
if (length(index.u)==0) {
db.u<-db}
if(length(index.u)!=0) {
db.u<-db[-index.u,]}


logaritmicQ<-as.matrix(log10(db.i$flow))
logaritmicC<-as.matrix(log10(db.i[,-which(names(db.i) %in% c("datetime", "flow"))]))
ols.model<-matrix(nrow=2, ncol= (ncomp))
for(i in 1:ncomp) {
ols.model[,i]<-as.matrix(lm(logaritmicC[,i]~logaritmicQ)$coefficients)
rownames(ols.model)<-c("intercept", "slope")
colnames(ols.model)<-c(names(db[3:(ncomp+2)]))
}
logconc<-matrix(nrow=nrow(db.u), ncol=ncomp)
for(i in 1:ncomp){
logconc[,i]<-as.matrix(((ols.model[1,i]))+ols.model[2,i]*(log10(db.u$flow)))
concfinal<-10^logconc
colnames(concfinal)<-c(names(db[3:(ncomp+2)]))
concdate<-cbind.data.frame(db.u$datetime, concfinal)
colnames(concdate)[1]<-c("datetime")
}
n<-nrow(db.u)
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y-%m-%d")
dateday<-aggregate(concdate[,2]~newdate, concdate, mean)
colnames(dateday)[1]<-c("datetime")
agg.dataC<-matrix(nrow=nrow(dateday), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(concdate[,i+1]~newdate, concdate, mean)
agg.dataC[,i]<-agg.data[,2]
concent<-cbind.data.frame(dateday$datetime, agg.dataC) 
colnames(concent)[1]<-"datetime"
colnames(concent)[2:(ncomp+1)]<-c(names(db)[3:(ncomp+2)])}
db.u$newdate<-format(as.POSIXct(db.u$datetime), format="%Y-%m-%d")
agg.dataQ<-aggregate(flow~newdate, db.u, mean)
agg.dataQ$newdate<-as.POSIXct(agg.dataQ$newdate, format = c("%Y-%m-%d"))


if (missing(period)) {
prodCQ<-as.matrix(concent[,-which(names(concent) %in% c("datetime"))]*as.vector(agg.dataQ[,"flow"]*86400))
loadtot<-(apply(prodCQ, 2, sum))
loadtot<-matrix(loadtot, nrow=1, ncol=ncomp)

var.res<-matrix(nrow=1, ncol=ncomp)
for(i in 1:ncomp) {
var.res[,i]<-as.matrix((summary(lm(logaritmicC[,i]~logaritmicQ))$sigma)**2)
colnames(var.res)<-c(names(db[3:(ncomp+2)]))
}

ferg.corr<-matrix(nrow=1, ncol=ncomp)
for(i in 1:length(var.res)) {
ferg.corr[,i]<-loadtot[i]*exp(2.651*var.res[i])}
     
method9N<-as.numeric(ferg.corr)
names(method9N)<-c(names(db)[3:(ncomp+2)])
return(method9N)
}


else if(period=="month") {
prodCQ<-as.matrix(concent[,-which(names(concent) %in% c("datetime"))]*as.vector(agg.dataQ[,"flow"]*86400))
prodCQdate<-cbind.data.frame(agg.dataQ[,1], prodCQ)
colnames(prodCQdate)[1]<-"newdate"
prodCQdate$newdate<-format(as.POSIXct(prodCQdate$newdate), format="%Y-%m")
aggrg.data<-matrix(nrow=length(unique(prodCQdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(prodCQdate[,i+1]~newdate, prodCQdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
rownames(aggrg.data)<-agg.init[,1]
if (i==(ncomp+1)) break}

var.res<-matrix(nrow=1, ncol=(ncomp))
for(i in 1:ncomp) {
var.res[,i]<-as.matrix((summary(lm(logaritmicC[,i]~logaritmicQ))$sigma)**2)
colnames(var.res)<-c(names(db[3:(ncomp+2)]))
}


ferg.corr<-matrix(nrow=nrow(aggrg.data), ncol=ncomp)
for(i in 1:ncomp) {
ferg.corr[,i]<-aggrg.data[,i]*(exp(2.651*var.res[i]))}
colnames(ferg.corr)<-colnames(aggrg.data)
rownames(ferg.corr)<-rownames(aggrg.data)
return(ferg.corr)
}


else if(period=="year"){
prodCQ<-as.matrix(concent[,-which(names(concent) %in% c("datetime"))]*as.vector(agg.dataQ[,"flow"]*86400))
prodCQdate<-cbind.data.frame(agg.dataQ[,1], prodCQ)
colnames(prodCQdate)[1]<-"newdate"
prodCQdate$newdate<-format(as.POSIXct(prodCQdate$newdate), format="%Y")
aggrg.data<-matrix(nrow=length(unique(prodCQdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(prodCQdate[,i+1]~newdate, prodCQdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db)[3:(ncomp+2)])
rownames(aggrg.data)<-agg.init[,1]
if (i==(ncomp+1)) break}

var.res<-matrix(nrow=1, ncol=ncomp)
for(i in 1:ncomp) {
var.res[,i]<-as.matrix((summary(lm(logaritmicC[,i]~logaritmicQ))$sigma)**2)
colnames(var.res)<-c(names(db[3:(ncomp+2)]))
}

ferg.corr<-matrix(nrow=nrow(aggrg.data), ncol=ncomp)
for(i in 1:ncomp) {
ferg.corr[,i]<-aggrg.data[,i]*(exp(2.651*var.res[i]))}


colnames(ferg.corr)<-c(names(db)[3:(ncomp+2)])
rownames(ferg.corr)<-agg.init[,1]
return(ferg.corr)
}}
