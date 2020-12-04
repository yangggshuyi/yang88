myLDA <- function(train_X, train_y, test_x){
  y = train_y
  X1 = train_X[train_y==1, ]
  X2 = train_X[train_y==0, ]

  pi1 <- mean(y)
  n1 <- sum(y)
  n2 <- sum(y == 0)
  pi2 <- 1 - mean(y)

  mu1 <- apply(X1, 2, mean)

  mu2 <- apply(X2, 2, mean)
  mu_1and2 <- (mu1 + mu2) / 2

  x_mu1 <- sweep(X1 ,2, mu1)
  x_mu2 <- sweep(X2 ,2, mu2)

  sigma1 <- t(x_mu1) %*% x_mu1 / n1
  sigma2 <- t(x_mu2) %*% x_mu2 / n2
  sigma_merge <- (n1-1) * sigma1/(n1 + n2-2) + (n2-1) * sigma2/(n1 + n2 - 2)

  # 3.LDA
  if(length(test_x) > 1){
    test_mu1andm2 <- sweep(test_x ,2, mu_1and2)
  }else{
    test_mu1andm2 <-test_x - mu_1and2
  }
  y_hat <- test_mu1andm2 %*% solve(sigma_merge) %*% (mu1 - mu2) + log(n1/n2)
  y_pred <- ifelse(y_hat > 0, 1, 0)
  return(list('yhat' = y_hat, 'ypred' = y_pred))
}


