# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)

# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

#As this is a supervised method I am assuming thta you have class/response column.
# Here k means how many parameters to bring
# ranking means what ranking to use for random forest. You can pass random forest parameters as well.
findusefulfeatures <- function(X,alpha=0.05,ranking='accuracy',k=20,frml1,cp=0.01,ntree=100,Y)
{
  
  k <- ifelse(ncol(X) < k,ncol(X),k)
  
  # Do a chisquare independece test
  ci <- checkindependenceofdata(X,alpha=alpha)
  colpvals <- ci$colpvals
  print (colpvals)
  
  # Do a random forest
  rf <- rf_feature_selection (X,as.formula(frml1),k=k,ranking=ranking,ntree=ntree)
  print (paste('top ranked features based on random forest',ranking))
  print(rf$feature_rank)
  
  # Thius might generate huge data so you might not want to use it.
  #print(rf$imp_ordered)
  print ("now rankinsg based on logistic regression. This is custom log reg")

  lr <- lr_feature_selection(X,Y)
  singlecols <- lr$singlecols
  print ("importance of single columns")
  #singlecols <- singlecols[order(singlecols[,2]),]
  print(singlecols)
  interactioncols <- lr$interactioncols
  interactioncols <- interactioncols[order(interactioncols[,3]),]
  print(interactioncols)
  
  # Run decision tree and it might take a bit longer and might generate long output in case all of the columns are discriminative
  print ("generating decision trees")
  decision_tree_for_finding_features(X,cp=cp)
}
