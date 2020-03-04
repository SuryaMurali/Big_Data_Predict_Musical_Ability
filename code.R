X <- #data from brain image scans pixelated, having values from 0 to 255 for every pixel
#assigning datasets
I<-X
dim(I)<-c(200,8000)
I_train<-I[1:100,]
I_test<-I[101:200,]
O<-matrix(Y,200,1)
dim(O)<-c(200,1)
O_train<-O[1:100,]
O_test<-O[101:200,]
c<-cor(I_train,O_train)
summary(c)
#randominzing training and validation sets - crossvalidation
n.test <- 10
testid <- sample(1:100, n.test, replace = FALSE)
I_training <- I_train[-testid,]
O_training <- O_train[-testid]
I_validation <- I_train[testid,]
O_validation <- O_train[testid]
#lasso
library(lars)
tr.lasso  <- lars(I_training,O_training,use.Gram=FALSE)
plot(tr.lasso)
betas    <- tr.lasso$beta
df       <- tr.lasso$df
MSE      <- tr.lasso$RSS/nrow(I_train)
bic      <- log(nrow(I_train))*df+nrow(I_train)*log(MSE)

cbind(df,MSE,bic)
bestb    <- which.min(bic)

plot(bic,cex=2)
points(bestb,bic[bestb],pch=19,cex=2)
beta_lasso <- betas[bestb,]

val.lasso <- predict.lars(tr.lasso, I_validation, s=0.3, type="fit",mode="fraction")
MSE_val_lasso<-sqrt(sum((val.lasso$fit - O_validation)^2)/n.test)

test.lasso <- predict.lars(lars(I_train,O_train,use.Gram=FALSE),I_test,s=0.3,type="fit",mode="fraction")
lasso.result <- test.lasso$fit

#alasso
beta_ols <- lm(O_train~I_train)$coef[-1]
w        <- abs(beta_ols)
Iw_train <- scale(I_train,center=FALSE,scale=1/w)
tr.alasso   <- lars(Iw_train,O_train,normalize=FALSE,use.Gram=FALSE)

plot(alasso)
betas    <- tr.alasso$beta
df       <- tr.alasso$df
MSE      <- tr.alasso$RSS/nrow(I_train)
bic      <- log(nrow(I_train))*df+nrow(I_train)*log(MSE)

cbind(df,MSE,bic)
bestb    <- which.min(bic)

plot(bic,cex=2)
points(bestb,bic[bestb],pch=19,cex=2)
beta_alasso <- betas[bestb,]

val.alasso <- predict.lars(tr.alasso, I_validation, s=0.3, type="fit",mode="fraction")
MSE_val_alasso<-sqrt(sum((val.alasso$fit - O_validation)^2)/n.test)

test.alasso <- predict.lars(lars(Iw_train,O_train,normalize=FALSE,use.Gram=FALSE),I_test,s=0.3,type="fit",mode="fraction")
alasso.result <- test.alasso$fit
