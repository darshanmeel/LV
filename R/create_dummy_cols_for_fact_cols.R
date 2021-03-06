#This will make all the factor columns to dummy varibale binary columns based on each level.
create_dummy_cols_for_fact_cols <- function(X,clscolpos)
{
fcl <- findallfactorcols(X)
#remove class column as we do not want to binarize that columns
fcl <- fcl[-clscolpos]

for (cl in fcl){

  cols <- colnames(X)
  lvls <- levels(X[,cl])
  i <- 0
  for (lvl in lvls){
    cl_suf <- as.character(lvl)
    clname <- paste0(cols[cl],'_',as.character(i))
    X$cl1 <- as.factor(c(ifelse(X[,cl]==lvl,1,0)))
    test_data$cl1 <- as.factor(c(ifelse(test_data[,cl]==lvl,1,0)))
    valid_data$cl1 <- as.factor(c(ifelse(valid_data[,cl]==lvl,1,0)))
    cols <- append(cols,clname)
    colnames(X) <- cols
    colnames(test_data) <- cols
    colnames(valid_data) <- cols
    i <- i + 1
  }

}
#remove the original columns

X[,fcl] <- as.data.frame(NULL)


}


