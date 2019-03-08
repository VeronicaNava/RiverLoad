db.intersect <-
function(flow.data, conc.data) {
flow.data[,which(names(flow.data) %in% c("datetime"))]<-as.POSIXct(flow.data[,which(names(flow.data) %in% c("datetime"))], format = ("%Y-%m-%d %H:%M:%S"))
colnames(flow.data)[-which(names(conc.data) %in% c("datetime"))]<-c("flow")
conc.data[,which(names(conc.data) %in% c("datetime"))]<-as.POSIXct(conc.data[,which(names(conc.data) %in% c("datetime"))], format = ("%Y-%m-%d %H:%M:%S"))
datenameflow <-grep ("datetime",names(flow.data))
datenameconc<- grep("datetime", names(conc.data))
if(length(datenameflow)<1 || length(datenameconc)<1) {
warning('Date column must be labeled with "datetime"')
}
intersec<-merge(flow.data, conc.data, by="datetime")
intersect<-na.omit(intersec)
return(intersect)}
