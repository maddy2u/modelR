source("HiveConnection.R")
source("DataWrangling.R")
source("StatisticalAnalysis.R")
source("MLClassifier.R")

## Enter the Generic Input for the model ###

database<-"database_name"                     #Database Name
table<-"table_name"                           #Table Name
dependent<-"dependent variable"               #Dependent Variable

Categorical_var<-c("cat1","cat2","cat3","cat4") #List of Categorical Variables for your analysis
Numeric_var<-c("num1","num2","num3","num4","num5","num6","num7","num8","num9") #List down the Numerical Variables for your analysis

###### Statistical Analysis ######
learning_set<-readdatafromhive(database,table) #Data Load from Hive
learning_set<-datacast(learning_set,Numeric_var,Categorical_var)
learning_set<-missingvaluetreatment(learning_set) # Missing Value Treatment
PlotHistogram(learning_set,Numeric_var) #Univariate - Histogram Plots
PlotBoxPlot(learning_set,Numeric_var,dependent)  #Bivariate - Box Plots
PlotBarChart(learning_set,Categorical_var,dependent) #Bivariate - Bar Charts
PlotCorrelationPlot(learning_set,Numeric_var,method="shade")

hypothesistest(learning_set,Numeric_var,Categorical_var) #Hypothesis Testing

####Creating Training and Testing Datasets #####

train.row<-sampleset(learning_set,70)
train<-learning_set[train.row,]
test<-learning_set[-train.row,]
train<-na.omit(train)
### Competing Model Development ###

###  ML Algorithms  ####
model<-mlalgo(train,"logistic")#Logistic
model<-mlalgo(train,"rpart")  #Decision Tree
model<-mlalgo(train1,"randomforest")  #Random Forest
model<-mlalgo(train1,"gbm")  #Gradient Boosting

### ML Evaluation ####
resultset<-modeleval(model,test)
