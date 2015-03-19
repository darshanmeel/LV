#This is still incomplete but more or less is done.
getprobcalc <- function(X,bt,nroww,ncolw)
{
  exp_part <- t(X) %*% bt  
  prob <- exp(exp_part)*1.0/apply(exp(exp_part),1,sum)
  prob
}
getoutputinmatrixform <- function(Y,numofinputrows,ncolw)
{   
  print (numofinputrows)
  outnums <- matrix(rep(seq(0:(ncolw-1)),numofinputrows),numofinputrows,ncolw,byrow=TRUE) 
  outnums <- outnums==Y

  outnums
}
calccostoutnum <- function(X,Y,bt,ncolw,nroww)
{
  
  prob <- getprobcalc(X,bt,ncolw,nroww)
  numofinputrows <- ncol(X)

  cost <- log(prob)
  outnums <- getoutputinmatrixform(Y,numofinputrows,ncolw)
  list(prob=prob,cost=cost,outnums=outnums)
} 
  #now see how many outputs and set their values from 0 to k 
  
 
 
  


minsftmx <- function (train_data,train_class,test_data,test_class)
{
 
    if (!is.vector(train_class)){
      train_class <- train_class[,1]

    }
    
    ncls <- length(unique(train_class))
    nparams <- ncol(train_data)

    W <- matrix(rnorm(ncls*nparams,0,sd=0.001),nparams,ncls)
    
    ncolw <- ncls
    nroww <- nparams

    
    X <- as.matrix(train_data)
    inp1 <- matrix(X,ncol(X),nrow(X),byrow=TRUE)
    
    rt <- calccostoutnum(inp1,train_class,W,ncolw,nroww)
   
  objective <- function(bt)
  {
 
    cost <- rt$cost
    outnums <- rt$outnums

    cost <- outnums * cost  
    cost <- apply(cost,1,sum)  
    cost <- mean(cost) * (-1)
    cost
  }
  gradient <- function(bt)
  {
    prob <- rt$prob
    outnums <- rt$outnums
    diff = outnums - prob    
    grad <- inp1 %*% diff    
    grad <- -1 * (grad/nrow(inp1) )   
    grad <- as.vector(grad) 
    grad
  }
  testing <- function(test_data,test_class,newbt)
  {
    print ("now test the data")
    X1 <- as.matrix(test_data)
    inp1 <- matrix(X1,ncol(X1),nrow(X1),byrow=TRUE)
    if (!is.vector(test_class)){
      test_class <- test_class[,1]
    }    
    vals <- calccostoutnum(inp1,test_class,newbt,ncls,nparams)[[3]]
    print (vals)
    test_data$predclass <- apply(vals,1,function(x) which.max(x) -1)
    test_data$class <- cbind(test_data,test_class)
    print (test_data)
    print (table(test_class,test_data$predclass))
  }
 
  print (objective(W))
  print (gradient(W))
  mnfnc <- optim(as.vector(W),objective,gr=gradient,method='CG',control= list(maxit=10))
  print (mnfnc)
  neww <- matrix(mnfnc$par,nparams,ncls)
  print(neww)
  testing(test_data,test_class,neww)



}
trn <- sample(1:150,100)



#setwd <- '/Users/dsing001/Downloads/mnist/'
#test_data <- read.csv('/Users/dsing001/Downloads/mnist/test_mnist.csv')
#test_lables <- read.csv('/Users/dsing001/Downloads/mnist/test_label_mnist.csv')
#train_lables <- read.csv('/Users/dsing001/Downloads/mnist/train_label_mnist.csv')
#train_data <- read.csv('/Users/dsing001/Downloads/mnist/train_mnist.csv')
#dim(train_data)
#minsftmx(train_data,train_lables,test_data,test_lables)



data <- iris[,1:4]
lbl <- iris[,5]
lvl <- levels(lbl)
lbl <- as.character(lbl)
lbl <- as.vector(sapply(lbl, function(x) x <- match(x,lvl)-1 ))
lbl
train_data <- data[trn,]
test_data <- data[-trn,]
train_class <- lbl[trn]
test_class <- lbl[-trn]
minsftmx(train_data,train_class,test_data,test_class)
