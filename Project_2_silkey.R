library(Rmosek)
mosek_version()


#Reading CSV from local system
train_data <- read.csv('C:/Users/silke/Desktop/Courses/ML/data/BlogFeedback/blogData_train.csv',header=FALSE, sep=",")

rmse <- function(error)
{
  sqrt(mean(error^2))
}


############# Experiment 1

train_data=train_data[c(0:6000), c(51:60,281)]
columns_dropped=c('V55', 'V60')

train_data=train_data[,!(names(train_data) %in% columns_dropped)]
train_data
## set the seed to make your partition reproductible
set.seed(123)
sample_size <- (nrow(train_data) * 0.80)

train_sample <- sample(seq_len(nrow(train_data)), sample_size)

train_sample_subset <- train_data[train_sample, ]
test_sample_subset <- train_data[-train_sample, ]

View(train_sample_subset)

#Taking Feature matrix X
train_sample_subset_1=train_sample_subset[,1:ncol(train_sample_subset)-1]
train_sample_subset_1
train_sample_subset_2=train_sample_subset[,ncol(train_sample_subset)]

View(train_sample_subset_2)
train_sample_subset_2=as.matrix(train_sample_subset_2)
train_sample_subset_1=as.matrix(train_sample_subset_1)


test_sample_subset_1=test_sample_subset[,1:ncol(test_sample_subset)-1]
test_sample_subset_1
test_sample_subset_2=test_sample_subset[,ncol(test_sample_subset)]

View(test_sample_subset_2)
test_sample_subset_2=as.matrix(test_sample_subset_2)
test_sample_subset_1=as.matrix(test_sample_subset_1)


#calling func
reg_function=solve.ols(train_sample_subset_1, train_sample_subset_2)
reg_function

diff=test_sample_subset_1[,1]*0.114304591 + test_sample_subset_1[,2]*0.203466604 + test_sample_subset_1[,3]*0.009158931+ test_sample_subset_1[,4]*(-0.118237666)  + test_sample_subset_1[,5]*0.340383196 +test_sample_subset_1[,6]*(-0.343401297) +test_sample_subset_1[,7]*(-0.140148857)+  test_sample_subset_1[,8]*(-0.066939687)
diff
error = test_sample_subset_2-diff



MSE_Mosek=rmse(error)
MSE_Mosek

error

###########optimization
solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]  # number of parameters of interest
  
  #correspondence between OLS and the Quadratic Program
  xx<-crossprod(X) # X'X=Q variable in the Quadratic program
  c<--crossprod(X,y) # X'y=c variable in the Quadratic program
  xx2<-xx
  xx2[upper.tri(xx)]<-0 #mosek needs Q to be  triangular
  idx <- which(xx2 != 0, arr.ind=TRUE) #index of the nonzero elements of Q
  
  #problem definition in Mosek
  qo1<-list() #empty list that contains the QP problem
  qo1$sense<-"min" #problem sense
  qo1$c<-as.vector(c) #objective coefficients
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the col indexes j and the values v that define the Q matrix
  qo1$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE) #constrain matrix A is a null matrix in this case
  
  qo1$bc<-rbind(blc=-Inf, buc= Inf) #constraint bounds
  qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p)) #parameter bounds 
  
  r<-mosek(qo1, opts = list(verbose = verb)) #call mosek solver
  return(r)
}


# Using train_sample_subset training subset
colnames(train_sample_subset) <- c('V51','V52','V53','V54','V56','V57','V58','V59',"Target")

#Performing Linear Regression
head(train_sample_subset)
head(test_sample_subset_1)
model <- lm(formula = Target ~ .,data = data.frame(train_sample_subset))
pred_model <- predict(model,data.frame(test_sample_subset_1),se.fit = TRUE)
summary(model)
pred_model$fit

# calculating root mean squared error
dim(train_sample_subset)
dim(test_sample_subset)
final_error <- (test_sample_subset_2 - pred_model$fit)
rmseError=rmse(final_error)
rmseError
final_error=sum(final_error**2)
final_error
typeof(pred_model)
final_error
print(mean(final_error^2))