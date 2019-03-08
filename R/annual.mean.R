annual.mean <-
function (flow.data, standev) {
flow.data$year<-format(as.POSIXct(flow.data$datetime), format="%Y")
datename <-grep ("datetime",names(flow.data))
flowname<- grep("flow", names(flow.data))
if(length(datename)<1 || length(flowname)<1) {
warning('Date column must be labeled with "datetime" and flow column with "flow"')
}
if(missing(standev)){
agg.data<-aggregate(flow~year, flow.data, mean)
colnames(agg.data)<-c("year", "flow")
agg.data$year<-as.numeric(agg.data$year)
return(agg.data)}


if(standev=="sd") {
agg.data1<-aggregate(flow~year, flow.data, mean)
agg.data2<-aggregate(flow~year, flow.data, sd)
agg.data<-cbind(agg.data1, agg.data2$flow)
colnames(agg.data)<-c("year", "flow", "sd")
agg.data$year<-as.numeric(agg.data$year)
return(agg.data)}}
