#'This function centers and reduces a matrix X
#'
#' @param X a matrix
#' @param preserve.names a boolean indicating whether names should be preserved
matrix.center.reduce <- function(X, preserve.names = TRUE){

    if(!is.matrix(X)) stop("The supplied argument is not a matrix")

    #determining the number of rows
    n <- nrow(X)

    # centering the matrix
    Xtilde <- matrix.center(X)

    # the variance matrix
    sigma <- t(Xtilde) %*% X / n

    #building the centered reduced matrix
    Xr <- Xtilde %*% diag(1/sqrt(diag(sigma))) / sqrt(n-1)

    if(preserve.names) {
        colnames(Xr) <- colnames(X)
    }

    Xr
}