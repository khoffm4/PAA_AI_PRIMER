library(glmnet)
n = 1000

x_1 = rnorm(n,mean =0, sd= 1)
x_2 = rnorm(n,mean =0, sd= 1)
x_3 = rnorm(n,mean=0,sd = 1)
epsilon = rnorm(n,mean = 0 ,sd=1)

beta_1 = 0.5
beta_2 = 0.9
y = x_1 * beta_1 + x_2 * beta_2 + epsilon
X = cbind(x_1, x_2,x_3)


#LASSO regression
fit <- glmnet(X, y)

#Plot the LASSO paths
plot(fit)

#LASSO path in matrix form 
#Each row is the regression coefficient for each variable and each column represents a differnet point on the LASSO path. Compare with plot above to get the full picture
lass_path_mat = as.matrix(fit$beta)

#LASSO with optimal model chosen by cross validation
cv_model  = cv.glmnet(X,y)
best_lambda <- cv_model$lambda.min
best_lambda

best_model <- glmnet(X, y, alpha = 0.1, lambda = best_lambda)
coef(best_model)
