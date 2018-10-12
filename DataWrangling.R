
## Loading Required Libraries ####
required<-c("glmnet","mice","Hmisc","dplyr","ggplot2","corrplot","ROCR","gbm","e1071","party","gbm")
lapply(required,require,character.only=TRUE)

datacast<-function(learning_set,Numeric_var,Categorical_var)
{
 # learning_set.num<-learning_set[,Numeric_var]
 # learning_set.char<-learning_set[,Categorical_var]
  learning_set<-cbind(select(learning_set,-one_of(Categorical_var)),lapply(select(learning_set,one_of(Categorical_var)),as.factor))
  learning_set<-cbind(select(learning_set,-one_of(Numeric_var)),lapply(select(learning_set,one_of(Numeric_var)),as.numeric))
  return(learning_set)
}


readdatafromhive<-function(database,table,dependent)
{
  formula<-paste0("select * from ",database,".",table)
  learning_set<-dbGetQuery(hiveConnection,formula)
colnames(learning_set)[colnames(learning_set) %in% dependent] <- paste0("dependent")
  return(learning_set)
}


## Automation Pending on this section. Coming up in the next update
missingvaluetreatment<-function(data)
{
data$cat1=NULL #75% Null
data$cat2=NULL #39% Null
data$cat3=NULL #13% Null
data$num1<-as.numeric(learning_set$num1)
imputation<-select(data,cat1,cat2,cat3,as.numeric(num1),cat4,num2,num3)
mice.op<-mice(imputation, method="pmm",maxit = 5)
miceOutput <- complete(mice.op) 
dropped_vars<-c("cat1","cat2","cat3","cat4","num1","num2","num3")
data<-cbind(select(learning_set,-one_of(dropped_vars)),miceOutput)
rm(miceOutput)
rm(mice.op)
rm(imputation)
#aggr_plot <- aggr(learning_set, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(learning_set), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
return(data)
}
