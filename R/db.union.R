db.union <-
function(flow.data, conc.data) {
flow.data[,which(names(flow.data) %in% c("datetime"))]<-as.POSIXct(flow.data[,which(names(flow.data) %in% c("datetime"))], format =("%Y-%m-%d %H:%M:%S"), tz="CET")
colnames(flow.data)[-which(names(conc.data) %in% c("datetime"))]<-c("flow")
flow.datanotNA<-na.omit(flow.data)
conc.data[,which(names(conc.data) %in% c("datetime"))]<-as.POSIXct(conc.data[,which(names(conc.data) %in% c("datetime"))],format = ("%Y-%m-%d %H:%M:%S"))
datenameflow <-grep ("datetime",names(flow.data))
datenameconc<- grep("datetime", names(conc.data))
if(length(datenameflow)<1 || length(datenameconc)<1) {
warning('Date column must be labeled with "datetime"')
}
pos<-match(flow.datanotNA$datetime, conc.data$datetime)
concentration<-(conc.data[pos,-which(names(conc.data) %in% c("datetime"))])
if(ncol(conc.data)>2) {
rownames(concentration)<-c(1:(nrow(flow.datanotNA)))}
db.comp<-cbind(flow.datanotNA, concentration)
return(db.comp)}
