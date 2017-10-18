vector.distance <- function(u, v, w, norm = 1,max=FALSE) {

    if(length(u) != length(v)) stop("Vectors have to be of equal length.")



    if(max == F){
        sum((abs(u - v)^norm))^(1/norm)
    } else {
        max(abs(u-v))
    }
}