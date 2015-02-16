# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.
# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)
#These are just wrapper methods to make taks easy when you have to type the same code over and again.
#This method will take train and test and will train the pglm logistic model and will return the positive classes probabilities.

train_and_predict_log_reg_and_ret_auc <- function(frml1,train,test,clscolpos=NULL,...)
{
  if (length(clscolpos) == 0) {
    clscolpos <- ncol(test)
  }
  tf <- train_and_predict_log_reg(frml1,train,test,...)
  tst_with_prob <- tf$test
  auc <- roc_of_models(tst_with_prob,clscolpos)
  #retun model, test data and train data so that you can reduce the size of the moel if needed.
  mdl <- tf$model
  
  list(auc=auc,model=mdl,tst_with_prob=tst_with_prob)
}

train_and_predict_log_reg <- function(frml1,train,test,predict_type='response',...)
{
  mdl <- train_log_reg(frml1,train,...)
  tst <- predict_log_reg(mdl$model,test,predict_type)
  list(test=tst$test,model=mdl,train=train)
}
#This method will train the log reg
train_log_reg <- function(frml1,train,...)
{
  frml1 <- as.formula(frml1)
  lm1 <- glm(frml1,data=train,family=binomial)
  list(model =lm1)
}
#This method will return the prediction for log reg
predict_log_reg <- function(mdl,test,predict_type='response')
{
  test$predprob <- predict(mdl,test,type=predict_type)
  list(test=test)
}
#

# Check the ROC for models. It assumes that the position of class column is psecified which will be last column in test df
# the predicted one will be after that.
roc_of_models <- function(test,clscolpos)
{
    lvls <- levels(test[,clscolpos])
    if (length(lvls) > 2){
      stop("More than 2 levels.ROC works with only 2 levels")
    }
    pred <- prediction(test[,(clscolpos+1)], test[,clscolpos])
    rocperf <- performance(pred, "tpr", "fpr")
    auc <- performance(pred,"auc")
    auc <- unlist(slot(auc, "y.values"))
    acc <- performance(pred,measure="acc",x.measure="cutoff")
    recall <- performance(pred,measure="rec",x.measure="cutoff")
    percision <- performance(pred,measure="prec",x.measure="cutoff")
    # you can calculate KS based on the tpr , fpr y values as well as you can use the roc curve.On roc curve these valus are 
    # y axis and x axis
    diff_in_vals <- as.vector(attr(rocperf,'y.values')[[1]]-attr(rocperf,'x.values')[[1]])
    KSRealized <- which.max(diff_in_vals)
    KS <- max(diff_in_vals)
    list(AUC=auc,KS=KS,rocperf=rocperf,KSRealized=KSRealized,acc=acc,recall=recall,percision=percision)
}

train_rf <- function(frml1,train,test,...)
{
  rf <- randomForest(frml1,train,importance=TRUE,...)
  test$predprob <- predict(rf,test,type='prob')[,2]
  list(test=test)
}
#
train_and_predict_random_forest_and_ret_auc <- function(frml1,train,test,clscolpos=NULL,...)
{
  if (length(clscolpos) == 0) {
    clscolpos <- ncol(test)
  }
  tf <- train_rf(frml1,train,test,...)
  ac <- roc_of_models(tf$test,clscolpos)
  #retun model, test data and train data so that you can reduce the size of the moel if needed.
  list(ac$auc)
}

