rating.period <-
function(db, ncomp, period){

if (period=="month"){
notNA<-na.omit(db)
db.i<-notNA[!rowSums(notNA[,-(1)] == 0) >= 1,]
index.u<-(which(db[,-which(names(db) %in% c("datetime"))]==0))
if (length(index.u)==0) {
db.u<-db}
if(length(index.u)!=0) {
db.u<-db[-index.u,]}

new<-db.i
new$newdate<-format(as.POSIXct(new$datetime), format="%Y-%m")

maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

result <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
result[[i]]<-log10(seldata[,-which(names(seldata) %in% c("datetime", "newdate"))])
}


ols.model<-vector("list", maximum)
mat<-matrix(nrow=2, ncol=ncomp)
for (j in 1:maximum){
mat.conc<-(result[j])
sel<-do.call(rbind, mat.conc)
for (i in 1:ncomp){
ols<-(lm(sel[,i+1]~sel[,1]))$coefficients
mat[,i]<-ols}
ols.model[[j]]<-mat}


db.u$newdate<-format(as.POSIXct(db.u$datetime), format="%Y-%m")
maximum2<-length(unique(db.u$newdate))
index2<-vector(length=maximum2)
for (i in 1:(maximum2)){
index2[i]<-length(which(db.u$newdate==(unique(db.u$newdate)[i])))}

result2 <- vector("list",maximum2)
for (i in 1:(maximum2)){
seldata2<-subset(db.u, db.u$newdate==unique(db.u$newdate)[i])
result2[[i]]<-log10(seldata2[,-which(names(seldata2) %in% c("datetime", "newdate"))])
}


logconc<-vector("list", maximum2)
for (j in 1:maximum2){
coefficic<-ols.model[j]
coeffsel<-do.call(rbind, coefficic)
flowsel<-result2[j]
sel<-do.call(rbind, flowsel)
matload<-matrix(nrow=nrow(sel), ncol=ncomp)
for(i in 1:ncomp){
matload[,i]<-10^(as.matrix(((coeffsel[1,i]))+coeffsel[2,i]*((sel$flow))))}
logconc[[j]]<-matload}

concfinal<-do.call(rbind, logconc)
colnames(concfinal)<-c(names(db.u[3:(ncomp+2)]))
concdate<-cbind.data.frame(db.u$datetime, concfinal)
colnames(concdate)[1]<-c("datetime")



n<-nrow(db.u)
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y-%m-%d")
dateday<-aggregate(concdate[,2]~newdate, concdate, mean)
colnames(dateday)[1]<-c("datetime")
agg.dataC<-matrix(nrow=nrow(dateday), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(concdate[,i+1]~newdate, concdate, mean)
agg.dataC[,i]<-as.matrix(agg.data[,2])
concent<-cbind.data.frame(dateday$datetime, agg.dataC) 
colnames(concent)[1]<-"datetime"
colnames(concent)[2:(ncomp+1)]<-c(names(db)[3:(ncomp+2)])}

db.u$newdate<-format(as.POSIXct(db.u$datetime), format="%Y-%m-%d")
agg.dataQ<-aggregate(flow~newdate, db.u, mean)
agg.dataQ$newdate<-as.POSIXct(agg.dataQ$newdate, format = c("%Y-%m-%d"))


prodCQ<-as.matrix(concent[,-which(names(concent) %in% c("datetime"))]*as.vector(agg.dataQ[,"flow"]*86400))
prodCQdate<-cbind.data.frame(agg.dataQ[,1], prodCQ)
colnames(prodCQdate)[1]<-"newdate"
prodCQdate$newdate<-format(as.POSIXct(prodCQdate$newdate), format="%Y-%m")
aggrg.data<-matrix(nrow=length(unique(prodCQdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(prodCQdate[,i+1]~newdate, prodCQdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db.u)[3:(ncomp+2)])
rownames(aggrg.data)<-agg.init[,1]}
return(aggrg.data)}


if (period=="year"){
notNA<-na.omit(db)
db.i<-notNA[!rowSums(notNA[,-(1)] == 0) >= 1,]
index.u<-(which(db[,-which(names(db) %in% c("datetime"))]==0))
if (length(index.u)==0) {
db.u<-db}
if(length(index.u)!=0) {
db.u<-db[-index.u,]}

new<-db.i
new$newdate<-format(as.POSIXct(new$datetime), format="%Y")

maximum<-length(unique(new$newdate))
index<-vector(length=maximum)
for (i in 1:(maximum)){
index[i]<-length(which(new$newdate==(unique(new$newdate)[i])))}

result <- vector("list",maximum)
for (i in 1:(maximum)){
seldata<-subset(new, new$newdate==unique(new$newdate)[i])
result[[i]]<-log10(seldata[,-which(names(seldata) %in% c("datetime", "newdate"))])
}


ols.model<-vector("list", maximum)
mat<-matrix(nrow=2, ncol=ncomp)
for (j in 1:maximum){
mat.conc<-(result[j])
sel<-do.call(rbind, mat.conc)
for (i in 1:ncomp){
ols<-(lm(sel[,i+1]~sel[,1]))$coefficients
mat[,i]<-ols}
ols.model[[j]]<-mat}


db.u$newdate<-format(as.POSIXct(db.u$datetime), format="%Y")
maximum2<-length(unique(db.u$newdate))
index2<-vector(length=maximum2)
for (i in 1:(maximum2)){
index2[i]<-length(which(db.u$newdate==(unique(db.u$newdate)[i])))}

result2 <- vector("list",maximum2)
for (i in 1:(maximum2)){
seldata2<-subset(db.u, db.u$newdate==unique(db.u$newdate)[i])
result2[[i]]<-log10(seldata2[,-which(names(seldata2) %in% c("datetime", "newdate"))])
}


logconc<-vector("list", maximum2)
for (j in 1:maximum2){
coefficic<-ols.model[j]
coeffsel<-do.call(rbind, coefficic)
flowsel<-result2[j]
sel<-do.call(rbind, flowsel)
matload<-matrix(nrow=nrow(sel), ncol=ncomp)
for(i in 1:ncomp){
matload[,i]<-10^(as.matrix(((coeffsel[1,i]))+coeffsel[2,i]*((sel$flow))))}
logconc[[j]]<-matload}


concfinal<-do.call(rbind, logconc)
colnames(concfinal)<-c(names(db.u[3:(ncomp+2)]))
concdate<-cbind.data.frame(db.u$datetime, concfinal)
colnames(concdate)[1]<-c("datetime")



n<-nrow(db.u)
concdate$newdate<-format(as.POSIXct(concdate$datetime), format="%Y-%m-%d")
dateday<-aggregate(concdate[,2]~newdate, concdate, mean)
colnames(dateday)[1]<-c("datetime")
agg.dataC<-matrix(nrow=nrow(dateday), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(concdate[,i+1]~newdate, concdate, mean)
agg.dataC[,i]<-as.matrix(agg.data[,2])
concent<-cbind.data.frame(dateday$datetime, agg.dataC) 
colnames(concent)[1]<-"datetime"
colnames(concent)[2:(ncomp+1)]<-c(names(db)[3:(ncomp+2)])}

db.u$newdate<-format(as.POSIXct(db.u$datetime), format="%Y-%m-%d")
agg.dataQ<-aggregate(flow~newdate, db.u, mean)
agg.dataQ$newdate<-as.POSIXct(agg.dataQ$newdate, format = c("%Y-%m-%d"))
prodCQ<-as.matrix(concent[,-which(names(concent) %in% c("datetime"))]*as.vector(agg.dataQ[,"flow"]*86400))
prodCQdate<-cbind.data.frame(agg.dataQ[,1], prodCQ)
colnames(prodCQdate)[1]<-"newdate"
prodCQdate$newdate<-format(as.POSIXct(prodCQdate$newdate), format="%Y")
aggrg.data<-matrix(nrow=length(unique(prodCQdate$newdate)), ncol=(ncomp))
for(i in 1:(ncomp)){
agg.init<-aggregate(prodCQdate[,i+1]~newdate, prodCQdate, sum)
aggrg.data[,i]<-agg.init[,2]
colnames(aggrg.data)<-c(names(db.u)[3:(ncomp+2)])
rownames(aggrg.data)<-agg.init[,1]}
return(aggrg.data)}}
