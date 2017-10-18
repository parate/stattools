matrix.center <- function(X){

    n <- nrow(X)
    ones <- c(rep(1,n))

    # centering by creating a n x n matrix of ones,
    # then calculating the inner product with X
    # -> repeats the column sums n times to create
    # a matrix of column sums. Then, divide by n
    # and subtract from X
    X - ones%*%t(ones)%*%X/n

    #alternative approach: X - rep(1,times=n)%*%t(colMeans(X))
}