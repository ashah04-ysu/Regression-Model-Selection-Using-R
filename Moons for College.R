# Lab: Linear Models and Regularization Methods

## Subset Selection Methods

### Best Subset Selection

library(ISLR2)
View(College)
names(College)
dim(College)
sum(is.na(College$Apps))
###
College <- na.omit(College)
dim(College)
sum(is.na(College))
###
#install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(Apps ~ ., College)
summary(regfit.full)
###
regfit.full <- regsubsets(Apps ~ ., data = College,
                          nvmax = 17)
reg.summary <- summary(regfit.full)
###
names(reg.summary)
###
reg.summary$rsq
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "p")
###
which.max(reg.summary$adjr2)
coef(regfit.full,11)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
       pch = 20)
###
plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "p")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
       pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "p")
points(6, reg.summary$bic[6], col = "red", cex=2,
       pch = 20)
###
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
###
coef(regfit.full, 6)

### Forward and Backward Stepwise Selection

###
regfit.fwd <- regsubsets(Apps ~ ., data = College,
                         nvmax = 17, method = "forward")
output1=summary(regfit.fwd)
regfit.bwd <- regsubsets(Apps ~ ., data = College,
                         nvmax = 17, method = "backward")
output2=summary(regfit.bwd)
which.min(output1$cp)
which.min(output2$cp)
coef(regfit.fwd,10)
coef(regfit.bwd,10)
which.min(output1$bic)
which.min(output2$bic)
which.max(output1$adjr2)
which.max(output2$adjr2)

###

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

### Choosing Among Models Using the Validation-Set Approach and Cross-Validation

###
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(College),
                replace = TRUE)
test <- (!train)
###
regfit.best <- regsubsets(Apps ~ .,
                          data = College[train, ], nvmax = 17)
###
test.mat <- model.matrix(Apps ~ ., data = College[test, ])
###
val.errors <- rep(NA, 17)
for (i in 1:17) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((College$Apps[test] - pred)^2)
}
###
val.errors
which.min(val.errors)
coef(regfit.best, 7)
###
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
###
regfit.best <- regsubsets(Apps ~ ., data = College,
                          nvmax = 17)
coef(regfit.best, 7)
###
k <- 10
n <- nrow(College)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 17,
                    dimnames = list(NULL, paste(1:17)))
###
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
###
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)

par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
###
reg.best <- regsubsets(Apps ~ ., data = College,
                       nvmax = 17)
coef(reg.best, 10)

## Ridge Regression and the Lasso

###
x <- model.matrix(Apps ~ ., College)[, -1]
y <- College$Apps

### Ridge Regression

###
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
###
dim(coef(ridge.mod))
###
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
###
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
###
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
###
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
###
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
mean((mean(y[train]) - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
                      exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
        x = x[train, ], y = y[train])[1:20, ]
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]

### The Lasso

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)
plot(lasso.mod)
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]
lasso.coef[lasso.coef = 0]
### PCA 
pca.fit=prcomp(x, center = TRUE,scale. = TRUE)
pca.fit
summary(pca.fit)
plot(pca.fit)
pca.fit=prcomp(x, center = TRUE,scale. = TRUE, tol=0.1)
pca.fit
summary(pca.fit)
pca.fit=prcomp(x, center = TRUE,scale. = TRUE, rank.=4)
pca.fit$rotation[,1]
summary(pca.fit)
names(pca.fit)
pca.fit$sdev
pca.fit$center
pca.fit$scale

## PCR and PLS Regression


### Principal Components Regression

###
library(pls)
set.seed(2)
pcr.fit <- pcr(Apps ~ ., data = College, scale = TRUE,
               validation = "CV")
###
summary(pcr.fit)
pcr.fit
###
validationplot(pcr.fit, val.type = "MSEP")
###
set.seed(1)
pcr.fit <- pcr(Apps ~ ., data = College, subset = train,
               scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
###
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)
mean((pcr.pred - y.test)^2)
###
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 5)
summary(pcr.fit)

### Partial Least Squares

###
set.seed(1)
pls.fit <- plsr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
###
pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred - y.test)^2)
###
pls.pred
