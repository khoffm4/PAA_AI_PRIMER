library(glmnet)
set.seed(2025)
n = 1000

x_1 = rnorm(n,mean =0, sd= 1)
x_2 = rnorm(n,mean =0, sd= 1)
x_3 = rnorm(n,mean=0,sd = 1)
epsilon = rnorm(n,mean = 0 ,sd=1)

beta_1 = 0.5
beta_2 = 0.9
y = x_1 * beta_1 + x_2 * beta_2 + epsilon
#note: Intercepts have to manually be added in the glmnet package
X = cbind(x_1, x_2,x_3)


#LASSO regression
fit <- glmnet(X, y)

#Plot the LASSO paths
plot(fit)

#LASSO path in matrix form 
#Each row is the regression coefficient for each variable and each column represents a differnet point on the LASSO path. Compare with plot above to get the full picture
lass_path_mat = as.matrix(fit$beta)
lass_path_mat

#LASSO with optimal model chosen by cross validation
cv_model  = cv.glmnet(X,y,alpha= 1,standardize = T)
best_lambda <- cv_model$lambda.min

#Prediction Error over lambda 
plot(cv_model)
best_model <- glmnet(X, y, alpha= 1,lambda = best_lambda)
coef(best_model)

#when lambda=0 you get the same solution as normal linear regression (alpha=0 is ridge regression)
linear_reg_lasso <- glmnet(X, y, alpha = 1, lambda = 0)
coef(linear_reg_lasso)
summary(lm(y~X))


#In glmnet, lambda is sometimes on the log scale. To align this with the lasso path, take the exponential
exp(best_lambda)

#Make predictions
y_hat = predict(best_model, X)

#Compare predictions to true Y
plot(y_hat, y)






