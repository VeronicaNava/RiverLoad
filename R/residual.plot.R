residual.plot <-
function(db, numbercomponent, filepath){
notNA<-na.omit(db)
inter<-notNA[!rowSums(notNA[,-(1)] == 0) >= 1,]

logaritmicQ<-(log10(inter$flow))
logaritmicC<-(log10(inter[,-which(names(inter) %in% c("datetime", "flow"))]))
if (is.null(ncol(logaritmicC))==T) {
ols.model<-(lm(logaritmicC~logaritmicQ))
}
if (is.null(ncol(logaritmicC))==F) {
ols.model<-(lm(logaritmicC[,numbercomponent]~logaritmicQ))}

if(missing(filepath)){
par(mfrow=c(2,2))
plot(ols.model)
}
else {
jpeg(filepath, width = 2500, height = 2500, units = "px", res = 300)
par(mfrow=c(2,2))
plot(ols.model)
dev.off()

}}
