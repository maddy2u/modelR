PlotHistogram <- function(x, Numeric_var,na.rm = TRUE) {
  nm <- Numeric_var
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + geom_histogram(alpha = .5,fill = "dodgerblue")
    ggsave(plots,filename=paste("Histogram",nm[i],".png",sep=""))
  }
}


PlotBoxPlot <- function(x,Numeric_var,dependent,na.rm = TRUE) {
  nm<-Numeric_var
  for (i in seq_along(nm)) {
    plots<-ggplot(x, aes(x=as.factor(dependent), y=nm[i],fill=as.factor(dependent))) + geom_boxplot()
    ggsave(plots,filename=paste("Box Plot_",nm[i],".png",sep=""))
  }
}

PlotBarChart <- function(x,Categorical_var,dependent,na.rm = TRUE) {
  nm<-Categorical_var
  for (i in seq_along(nm)) {
    plots<-ggplot(x, aes(x=as.factor(dependent), y=nm[i],fill=as.factor(dependent))) + geom_bar()
    ggsave(plots,filename=paste("Bar Plot_",nm[i],".png",sep=""))
  }
}

PlotCorrelationPlot<-function(data=x,Numeric_var,method="circle")
{
  x.num<-learning_set[,Numeric_var]
  corrplot(cor(x.num),method )
}


hypothesistest<-function(data,Numeric_var,Categorical_var)
{
  pdf("Hypothesis_Tests.pdf")
  for (i in seq_along(Categorical_var))
  {
    chisq<-chisq.test(learning_set[,Categorical_var][[i]],as.factor(learning_set$dependent))
    plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
    title(paste0("Chi- Square Test -",Categorical_var[i]),cex.main=1)
    text(x = 1,y=1,labels = print(chisq))
  }
  
  # T test #
  nm<-Numeric_var
  for (i in seq_along(nm))
  {
    tryCatch({
      attri<-nm[i]
      voc.t<-learning_set %>% select(dependent,one_of(attri)) %>% na.omit()
      a<-as.numeric(filter(voc.t,dependent==1)[,attri])
      b<-as.numeric(filter(voc.t,dependent==0)[,attri])
      ttestresult<-t.test(a,b)
      plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
      title(paste0("T-Test -",attri),cex.main=1)
      text(x = 1,y=1,labels =print(ttestresult))
    },
    error = function(err) {
      
      # error handler picks up where error was generated
      print(paste("ERROR",attri))
      
    })
  }
  print ("PDF Generated with results of hypothesis testing")
  dev.off()
}


sampleset <- function(data, x){
  sampledata =sample(nrow(data),replace=F,size=(x/100)*nrow(data))
  return(sampledata)
}
