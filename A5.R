library(ISLR2)
library(glmnet)
library(pls)
library(leaps)

#View(College)
#names(College)
College <- na.omit(College)
dim(College)
sum(is.na(College))

College[, -1] <- apply(College[, -1], 2, scale)

train.size <-  dim(College)[1] / 2
train <-  sample(1:dim(College)[1], train.size)
test <-  -train
train <-  College[train, ]
test <-  College[test, ]

##############################################################

regfit.full <- regsubsets(Apps ~ ., College,
                          nvmax = 17)
summary(regfit.full)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$adjr2

which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "p")
points(13, reg.summary$adjr2[13], col = "red", cex = 2, 
       pch = 20)
coef(regfit.full,13)
which.min(reg.summary$cp)

plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "p")
points(12, reg.summary$cp[12], col = "red", cex = 2,
       pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "p")
points(10, reg.summary$bic[10], col = "red", cex=2,
       pch = 20)

##Forward backward
regfit.fwd <- regsubsets(Apps ~ ., data = College,
                         nvmax = 17, method = "forward")
forward=summary(regfit.fwd)
regfit.bwd <- regsubsets(Apps ~ ., data = College,
                         nvmax = 17, method = "backward")
backward=summary(regfit.bwd)

which.max(forward$adjr2) #model13
which.max(backward$adjr2)#model13

which.min(forward$cp)#model12
which.min(backward$cp)#model12

which.min(forward$bic)#model10
which.min(backward$bic)#model10

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(College),
                replace = TRUE)
test <- (!train)

regfit.best <- regsubsets(Apps ~ .,
                          data = College[train, ], nvmax = 17)
test.mat <- model.matrix(Apps ~ ., data = College[test, ])

val.errors <- rep(NA, 17)
for (i in 1:17) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((College$Apps[test] - pred)^2)
}
val.errors
which.min(val.errors)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(Apps ~ ., data = College,
                          nvmax = 17)
coef(regfit.best, 10)

k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 17,
                    dimnames = list(NULL, paste(1:17)))
for (j in 1:k) {
  best.fit <- regsubsets(Apps ~ .,
                         data = College[folds != j, ],
                         nvmax = 17)
  for (i in 1:17) {
    pred <- predict(best.fit, College[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((College$Apps[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)

plot(mean.cv.errors, type = "b")
#We see cross validation selects a 13 variable model.  
reg.best <- regsubsets(Apps ~ ., data = College,
                       nvmax = 17)
coef(reg.best, 13)
#We get the best subset selection on the full data set in order to obtain the 12-variable model.

#2
train.mat <-  model.matrix(Apps ~ . -1 , data = train)
test.mat <-  model.matrix(Apps ~ . -1, data = test)
grid <-  10 ^ seq(4, -2, length = 100)
mod.ridge <-  cv.glmnet(train.mat, train[, "Apps"], 
                        alpha = 0, lambda = grid, thresh = 1e-12)
lambda.best <-  mod.ridge$lambda.min
lambda.best

ridge.pred <-  predict(mod.ridge, newx = test.mat, s = lambda.best)
mean((test[, "Apps"] - ridge.pred)^2)

#3 LASSO
set.seed(1)
cv.out2 = cv.glmnet(train.mat, train$Apps, alpha = 1)
bestlam2 = cv.out2$lambda.min
bestlam2

#Fit lasso model
lasso.mod = glmnet(train.mat, train$Apps, alpha = 1)
#Make predictions
lasso.pred = predict(lasso.mod, s = bestlam2, newx = test.mat)
mean((lasso.pred - test$Apps)^2)

#4 PCR
set.seed(1)
pcr.fit = pcr(Apps~., data = train, scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
summary(pcr.fit)
#Number of component is least at 17
pcr.pred=predict(pcr.fit,test,ncomp=17)
mean((pcr.pred-test$Apps)^2)

#5 PLS
set.seed(1)
pls.fit = plsr(Apps~., data = train, scale = TRUE,validation = "CV")
validationplot(pls.fit,val.type = "MSEP")

summary(pls.fit)
#Number of component is least at 11
pls.pred = predict(pls.fit, test, ncomp = 11)
mean((pls.pred - test$Apps)^2)

