## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeVector <- function(m = numeric()) {
    my <- NULL
    set <- function(y) {
        x <<- y;
        m <<- NULL;
    }
    get <- function() x;
    setmean <- function(mean) my <<- mean;
    getmean <- function() m;
    list(set=set, get=get, 
         setmean=setmean,
         getmean=getmean)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cachemean <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
           return(m)
    }
    data <- x$get()
    x <- mean(data, ...)
    x$setmean(m)
    m
}
