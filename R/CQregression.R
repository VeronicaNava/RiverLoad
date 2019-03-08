CQregression <-
function(db, ncomp) {
inter<-na.omit(db)

ols.model<-matrix(nrow=1, ncol= (ncomp))
for(i in 1:ncomp) {
ols<-lm(inter[,i+2]~inter$flow)
ols.model[1,i]<-summary(ols)$r.squared
rownames(ols.model)<-c("rsquared")
colnames(ols.model)<-c(names(db[3:(ncomp+2)]))
if (i==ncomp) break
}

return(ols.model)}
