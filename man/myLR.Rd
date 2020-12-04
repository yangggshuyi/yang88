\name{myLR}
\alias{myLR}
\alias{myLDA}
\alias{myKNN}
\title{fit classifiction model with logistic regression or lda or knn!}
\usage{
myLR(train_x, train_y, test_x)

myLDA(train_X, train_y, test_x)

myKNN(train_x, train_y,test_x, k = 5)
}

\arguments{
\item{train_x}{input matrix}

\item{train_y}{response variable}

\item{test_x}{test matrix}

\item{k}{k in knn}

}

\description{
fit classifiction model with logistic regression or lda or knn!
}

\examples{
data(iris)
X = as.matrix(subset(iris,Species == "setosa" | Species == "versicolor")[1:4])
y = append(rep(0,50), rep(1,50))

### logistic regression
ypred1 <- myLR(X, y ,test_x = X)
ypred1$param
table(ypred1$yhat>0.5, y)

### knn
ypred2 <- myKNN(X, y,test_x = X, k = 5)
table(ypred2, y)

### lda
source('myLDA.R')
ypred3 <- myLDA(X, y,test_x = X)
table(ypred3$ypred, y)
}
