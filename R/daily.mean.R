daily.mean <-
function (flow.data, standev) {
flow.data$newdate<-format(as.POSIXct(flow.data$datetime), format="%Y-%m-%d")
datename <-grep ("datetime",names(flow.data))
flowname<- grep("flow", names(flow.data))
if(length(datename)<1 || length(flowname)<1) {
warning('Date column must be labeled with "datetime" and flow column with "flow"')
}

if (missing(standev)){
agg.data<-aggregate(flow ~ newdate , flow.data, mean)
colnames(agg.data)<-c("datetime", "flow")
agg.data$datetime<-as.POSIXct(agg.data$datetime, format = c("%Y-%m-%d"))
return(agg.data)}

else if(standev=="sd"){
agg.data1<-aggregate(flow ~ newdate , flow.data, mean)
agg.data2<-aggregate(flow ~ newdate , flow.data, sd)
agg.data<-cbind(agg.data1, agg.data2$flow)
colnames(agg.data)<-c("datetime", "flow", "sd")
agg.data$datetime<-as.POSIXct(agg.data$datetime, format = c("%Y-%m-%d"))
return(agg.data)}}
