monthly.year.mean <-
function (flow.data, standev) {
flow.data$newdate<-format(as.POSIXct(flow.data$datetime), format="%Y-%m")
datename <-grep ("datetime",names(flow.data))
flowname<- grep("flow", names(flow.data))
if(length(datename)<1 || length(flowname)<1) {
warning('Date column must be labeled with "datetime" and flow column must be labeled with "flow"')
}

if (missing(standev)){
aggrg.data<-aggregate(flow~newdate, flow.data, mean)
colnames(aggrg.data)<-c("datetime", "flow")
aggrg.data$datetime<-as.Date(paste(aggrg.data$datetime, "-01", sep=""))
aggrg.data$datetime<-as.POSIXct(aggrg.data$datetime, format ="%Y-%m")
return(aggrg.data)
}

if (standev=="sd"){
aggrg.data1<-aggregate(flow~newdate, flow.data, mean)
aggrg.data2<-aggregate(flow ~ newdate , flow.data, sd)
aggrg.data<-cbind(aggrg.data1, aggrg.data2$flow)
colnames(aggrg.data)<-c("datetime", "flow", "sd")
aggrg.data$datetime<-as.Date(paste(aggrg.data$datetime, "-01", sep=""))
aggrg.data$datetime<-as.POSIXct(aggrg.data$datetime, format ="%Y-%m")
return(aggrg.data)}}
