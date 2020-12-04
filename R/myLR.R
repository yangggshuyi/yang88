# logistic regression 牛顿法求解 ---------------------
gradient = function(X,y,beta0) {
    w = exp(X %*% beta0)/(1 + exp(X %*% beta0))-y
    grad = t(w) %*% X/length(y)
    return(t(grad))
}

hessian = function(X,y,beta0) {
    w = exp(X %*% beta0)/(1+exp(X %*% beta0))^2
    W = diag(c(w),nrow = length(c(w)))
    return(t(X) %*% W %*% X/length(y))
}

loss = function(X,y,beta0) {
    sum = 0
    for (i in 1:length(y))
      sum = sum + (log(1+exp(X[i, ]%*%beta0))-y[i]*X[i,]%*%beta0)
    return(sum)
}

newton = function(X, y, beta0 , tol = 1e-4, iters = 10) {
    i = 0
    while (loss(X,y,beta0)>tol & i < iters) {
      beta0 = beta0 - solve(hessian(X,y,beta0)) %*% gradient(X,y,beta0)
      i = i+1
    }
    return(beta0)
}

sigmoid = function(x) {
    return(1/(1+exp(-x)))
}

myLR <- function(train_x, train_y, test_x){
    X = train_x
    y = train_y
    p = dim(X)[2]
    n = dim(X)[1]
    test_n = dim(test_x)[1]
    test_x =  cbind(rep(1, test_n), test_x)
    X = cbind(rep(1, n), X)
    beta0 = as.matrix(rep(0, p+1))   # 初始化参数
    new_beta = newton(X = X, y = y, beta0)  # 更新参数
    yhat = sigmoid(test_x %*% new_beta)
    return(list('yhat' = yhat, 'param' = new_beta))
}

