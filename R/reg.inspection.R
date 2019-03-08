reg.inspection <-
function(db, ncomp){
notNA<-na.omit(db)
inter<-notNA[!rowSums(notNA[,-(1)] == 0) >= 1,]

logaritmicQ<-as.matrix(log10(inter$flow))
logaritmicC<-as.matrix(log10(inter[,-which(names(inter) %in% c("datetime", "flow"))]))

ols.model<-matrix(nrow=6, ncol= (ncomp))
for(i in 1:ncomp) {

ols.model[1,i]<-(lm(logaritmicC[,i]~logaritmicQ)$coefficients[1])
ols.model[2,i]<-summary(lm(logaritmicC[,i]~logaritmicQ))$coefficients[1,4]
ols.model[3,i]<-(lm(logaritmicC[,i]~logaritmicQ)$coefficients[2])
ols.model[4,i]<-summary(lm(logaritmicC[,i]~logaritmicQ))$coefficients[2,4]
ols.model[5,i]<-(summary(lm(logaritmicC[,i]~logaritmicQ)))$r.squared
ols.model[6,i]<-((lm(logaritmicC[,i]~logaritmicQ)))$df.residual

rownames(ols.model)<-c("intercept","int.p", "slope","slo.p", "rsquared", "df.residual")
colnames(ols.model)<-c(names(db[3:(ncomp+2)]))
}
return(ols.model)
}
