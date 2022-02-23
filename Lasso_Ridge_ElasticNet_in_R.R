
#install.packages("glmnet")
#install.packages("dplyr")
library(glmnet)
library(dplyr)

data(swiss)
# ?swiss
head(swiss)

# Data Setting

y <- swiss$Fertility
x <- swiss %>% select(Agriculture, Examination, Education, Catholic, Infant.Mortality) %>%
  data.matrix()



set.seed(100)
cv.model <- cv.glmnet(x, y, nfold=10)
plot(cv.model)

#  minimum value of MSPE

index.min <- which.min(cv.model$cvm)
index.min

#  Penalty parameter to minimize MSPE

cv.model$lambda[index.min]

#####Lasso#####

# Repeat 100 times

lambda.vec.lasso <- rep(0, 100)
for (i in 1:100)
{
  set.seed(i + 100)
  cv.lasso <- cv.glmnet(x, y, nfold=10)
  index.min.lasso <- which.min(cv.lasso$cvm)
  lambda.vec.lasso[i + 100] <- cv.lasso$lambda[index.min.lasso]
}

#  Average the estimated penalty parameters
mean(lambda.vec.lasso)

#  Fitted value of Fertility

opt.lambda.lasso <- mean(lambda.vec.lasso)
lasso2 <- glmnet(x, y, lambda = opt.lambda.lasso)
fit.lasso <- predict(lasso2, newx = x, s=opt.lambda.lasso)

# MSE

mse.lasso  <- mean((y - fit.lasso)^2)
mse.lasso


#####Ridge#####
  

# Repeat 100 times

lambda.vec.ridge <- rep(0, 100)
for (i in 1:100)
{
  set.seed(i + 100)
  cv.ridge <- cv.glmnet(x, y, nfold=10, alpha = 0)
  index.min.ridge <- which.min(cv.ridge$cvm)
  lambda.vec.ridge[i + 100] <- cv.ridge$lambda[index.min.ridge]
}

#  Average the estimated penalty parameters
mean(lambda.vec.ridge)

# Fitted value of Fertility

opt.lambda.ridge <- mean(lambda.vec.ridge)
ridge2 <- glmnet(x, y, lambda = opt.lambda.ridge)
fit.ridge <- predict(ridge2, newx = x, s=opt.lambda.ridge)

#  MSE

mse.ridge  <- mean((y - fit.ridge)^2)
mse.ridge

#####Elastic Net#####

alpha = 0.2

# Repeat 100 times

lambda.vec.enet_0.2 <- rep(0, 100)
for (i in 1:100)
{
  set.seed(i + 100)
  cv.enet_0.2 <- cv.glmnet(x, y, nfold=10, alpha = 0.2)
  index.min.enet_0.2 <- which.min(cv.enet_0.2$cvm)
  lambda.vec.enet_0.2[i + 100] <- cv.enet_0.2$lambda[index.min.enet_0.2]
}

#  Average the estimated penalty parameters
mean(lambda.vec.enet_0.2)

# Fitted value of Fertility

opt.lambda.enet_0.2 <- mean(lambda.vec.enet_0.2)
enet_0.2 <- glmnet(x, y, lambda = opt.lambda.enet_0.2)
fit.enet_0.2 <- predict(enet_0.2, newx = x, s=opt.lambda.enet_0.2)

#  MSE

mse.enet_0.2  <- mean((y - fit.enet_0.2)^2)
mse.enet_0.2

alpha = 0.4

# Repeat 100 times

lambda.vec.enet_0.4 <- rep(0, 100)
for (i in 1:100)
{
  set.seed(i + 100)
  cv.enet_0.4 <- cv.glmnet(x, y, nfold=10, alpha = 0.4)
  index.min.enet_0.4 <- which.min(cv.enet_0.4$cvm)
  lambda.vec.enet_0.4[i + 100] <- cv.enet_0.4$lambda[index.min.enet_0.4]
}

#  Average the estimated penalty parameters
mean(lambda.vec.enet_0.4)

#  Fitted value of Fertility

opt.lambda.enet_0.4 <- mean(lambda.vec.enet_0.4)
enet_0.4 <- glmnet(x, y, lambda = opt.lambda.enet_0.4)
fit.enet_0.4 <- predict(enet_0.4, newx = x, s=opt.lambda.enet_0.4)

#  MSE

mse.enet_0.4  <- mean((y - fit.enet_0.4)^2)
mse.enet_0.4


alpha = 0.6

#  Repeat 100 times

lambda.vec.enet_0.6 <- rep(0, 100)
for (i in 1:100)
{
  set.seed(i + 100)
  cv.enet_0.6 <- cv.glmnet(x, y, nfold=10, alpha = 0.6)
  index.min.enet_0.6 <- which.min(cv.enet_0.6$cvm)
  lambda.vec.enet_0.6[i + 100] <- cv.enet_0.6$lambda[index.min.enet_0.6]
}

# Average the estimated penalty parameters
mean(lambda.vec.enet_0.6)

#  Fitted value of Fertility

opt.lambda.enet_0.6 <- mean(lambda.vec.enet_0.6)
enet_0.6 <- glmnet(x, y, lambda = opt.lambda.enet_0.6)
fit.enet_0.6 <- predict(enet_0.6, newx = x, s=opt.lambda.enet_0.6)

#  MSE

mse.enet_0.6  <- mean((y - fit.enet_0.6)^2)
mse.enet_0.6


alpha = 0.8

#  Repeat 100 times

lambda.vec.enet_0.8 <- rep(0, 100)
for (i in 1:100)
{
  set.seed(i + 100)
  cv.enet_0.8 <- cv.glmnet(x, y, nfold=10, alpha = 0.8)
  index.min.enet_0.8 <- which.min(cv.enet_0.8$cvm)
  lambda.vec.enet_0.8[i + 100] <- cv.enet_0.8$lambda[index.min.enet_0.8]
}

# Average the estimated penalty parameters
mean(lambda.vec.enet_0.8)

#  Fitted value of Fertility

opt.lambda.enet_0.8 <- mean(lambda.vec.enet_0.8)
enet_0.8 <- glmnet(x, y, lambda = opt.lambda.enet_0.8)
fit.enet_0.8 <- predict(enet_0.8, newx = x, s=opt.lambda.enet_0.8)

#  MSE

mse.enet_0.8  <- mean((y - fit.enet_0.8)^2)
mse.enet_0.8

# Result

minimum_MSE <- min(mse.lasso, mse.ridge, mse.enet_0.2, mse.enet_0.4, mse.enet_0.6, 
                   mse.enet_0.8)

cat( 'MSE of Lasso Model:', mse.lasso, '\n',
     'MSE of Ridge Model:', mse.ridge, '\n\n',
     'MSE of Elastic Net Model(0.2):', mse.enet_0.2, '\n',
     'MSE of Elastic Net Model(0.4):', mse.enet_0.4, '\n',
     'MSE of Elastic Net Model(0.6):', mse.enet_0.6, '\n',
     'MSE of Elastic Net Model(0.8):', mse.enet_0.8, '\n\n',
     'The minimum value of MSE: Lasso Model', minimum_MSE)

coef(lasso2, s = opt.lambda.lasso)





