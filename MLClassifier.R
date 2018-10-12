mlalgo<-function(train,model,alpha=0.5,nlambda=100,ntrees=100,shrink=0.1)
{
  dependent<-"dependent"
  switch(model,
         logistic={
           train.x<-train[!names(train) %in% dependent]
           x <-model.matrix( ~ .-1, train.x)
           model = cv.glmnet(x = x,y=as.factor(train[,dependent]) ,family="binomial",alpha=alpha,nlambda=nlambda)
           print(summary(model))
           return(model)
         },
         rpart={
           model<-rpart::rpart(dependent~.,data = train)
           print(summary(model))
           return(model)
         },
         randomforest={
           model<-cforest(formula =dependent~.,data=train,controls=cforest_unbiased())
           print(summary(model))
           return(model)
         },
         gbm={
           model<-gbm(dependent~.,as.data.frame(model.matrix(~.-1,train)),n.trees = ntrees,shrinkage=shrink,distribution = "bernoulli")
           print(summary(model))
           return(model)
         },
         svm
         ={
           
           svm.model = svm(dependent ~., data = train, scale = TRUE, na.action = na.omit, kernel = "radial")
           print(summary(model))
           return(model)
         }
  )
  
}

modeleval<-function(model,test)
{
  result<-predict(model,test)
  result<-predict(as.matrix(test1),model,type="prob")
  # conf<-table(test$dependent,result)
  # print("Accuracy");print(round(100*(conf[1,1]+conf[2,2])/(conf[1,1]+conf[1,2]+conf[2,1]+conf[2,2]),2),' %')#Accuracy
  # print("Recall");print(round(100*(conf[2,2])/(conf[2,1]+conf[2,2]),2),' %')#Recall
  # print("Precision");print(round(100*(conf[2,2])/(conf[1,2]+conf[2,2]),2),' %')#Precision
  
}



