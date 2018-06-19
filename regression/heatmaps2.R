#!RScript
# args[1] is the data procesed
# args[2] is the path for the results
library(ggplot2)
library(plyr)
library(reshape2)
library(tools)

args=(commandArgs(TRUE))
processData <- function(data){
  #Adding a helper colum
  data$index <- 1
  data$index <- ave(data$index, cumsum(c(F, diff(data$sr) > 0)), FUN=seq_along)
  data$freq<-ave( as.numeric(data[[1]]), data[["sr"]] , FUN=length)
  data$index= data$index * 100 / data$freq
  
  #CALCULATE METRICS
  data$accuracy = (data$TP+data$TN)/(data$TP+data$TN+data$FN+data$FP)
  data$specificity=data$TN/(data$TN+data$FP)
  data$precision=data$TP/(data$TP + data$FP)
  data$recall = data$TP / (data$TP+ data$FN) # Recall of class 1
  data$npv=data$TN/(data$TN+data$FN)
  data$negNPV=data$npv-(data$index/100)
  data$negAccuracy=data$accuracy-(data$index/100)
  
  #This melting is because the data wasn't in the proper csv style
  data<-melt(data, id.vars = c("FN","FP","TN","TP","sr","t","index","freq"))
  
  return(data)
  }

#LOAD THE FILE(S)
data=read.table(args[1], header=T,sep=",");
#data=read.table("./data/Apache.csv", header=T,sep=",")

data<-processData(data);

for (v in unique(data$variable)){
  datatmp=data[which(data$variable == v),]
  plot<-ggplot(datatmp,aes(sr,index))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low = "yellow",high = "red")+
    xlab("Number of configurations in the training set") +
    ylab("Percentage of acceptable configurations")+
    theme(strip.background = element_blank())+labs(fill="Classification\nMetric")+ 
    theme(axis.text = element_text(size = rel(2)),axis.title= element_text(size = rel(2)))+
    ggtitle(v)
  
  ggsave(plot,file=paste(args[2], paste(basename(file_path_sans_ext(args[1])), ".png", sep = paste("-",as.character(v),sep="")), sep=""), width=11, height=11)
}