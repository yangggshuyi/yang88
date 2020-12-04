
# 1.knn算法 ---------------------
myKNN <- function(train_x, train_y,test_x, k = 5){
  test_size = dim(test_x)[1]
  size = dim(train_x)[1]

  # 1.计算两个矩阵每个的距离
  l2_distance = -2 * test_x %*% t(train_x) +
    matrix(rep(rowSums(train_x^2), test_size), nrow = test_size, byrow = TRUE)+
    matrix(rep(rowSums(test_x^2), size), nrow = test_size)

  # 2.定义函数排序
  top_k <- function(test1,top_k = k){
    order_dist = order(test1, decreasing = F)[1:top_k]
    return(names(sort(table(train_y[order_dist]),decreasing = T)[1]))
  }

  # 3.预测结果
  yy_pred = as.vector(apply(l2_distance, MARGIN = 1, FUN = top_k))
  return(yy_pred)
}

