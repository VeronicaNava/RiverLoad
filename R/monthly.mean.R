monthly.mean <-
function (flow.data,standev) {
flow.data$month<-format(as.POSIXct(flow.data$datetime), format="%m")
datename <-grep ("datetime",names(flow.data))
flowname<- grep("flow", names(flow.data))
if(length(datename)<1 || length(flowname)<1) {
warning('Date column must be labeled with "datetime" and flow column with "flow"')
}

if(missing(standev)){
agg.data<-aggregate(flow~month, flow.data, mean)
colnames(agg.data)<-c("month", "flow")
agg.data$month<-as.numeric(agg.data$month)
return(agg.data)}

if(standev=="sd") {
agg.data1<-aggregate(flow~month, flow.data, mean)
agg.data2<-aggregate(flow~month, flow.data, sd)
agg.data<-cbind(agg.data1, agg.data2$flow)
colnames(agg.data)<-c("month", "flow", "sd")
agg.data$month<-as.numeric(agg.data$month)
return(agg.data)}}
