rsquared.period <-
function(db, ncomp, period) {
notNA<-na.omit(db)
db.i<-notNA[!rowSums(notNA[,-(1)] == 0) >= 1,]

if(period=="month"){
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
mat.res<-matrix(nrow=1, ncol=ncomp)
for (j in 1:maximum){
for (i in 1:ncomp){
mat.conc<-(result[j])
sel<-do.call(rbind, mat.conc)
ols<-(lm(sel[,i+1]~sel[,1]))
mat.res[,i]<-as.matrix((summary(ols)$r.squared))}
ols.model[[j]]<-mat.res}
rsquared<-do.call(rbind, ols.model)
colnames(rsquared)<-c(names(db)[3:(ncomp+2)])
rownames(rsquared)<-unique(new$newdate)

return(rsquared)}

if(period=="year"){
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
mat.res<-matrix(nrow=1, ncol=ncomp)
for (j in 1:maximum){
for (i in 1:ncomp){
mat.conc<-(result[j])
sel<-do.call(rbind, mat.conc)
ols<-(lm(sel[,i+1]~sel[,1]))
mat.res[,i]<-as.matrix((summary(ols)$r.squared))}
ols.model[[j]]<-mat.res}
rsquared<-do.call(rbind, ols.model)
colnames(rsquared)<-c(names(db)[3:(ncomp+2)])
rownames(rsquared)<-unique(new$newdate)

return(rsquared)}}
