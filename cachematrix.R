## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeVector <- function(my_x = matrix()) {
    my_inv <- NULL
    set <- function(x) {
        my_x <<- x;
        my_inv <<- NULL;
    }
    get <- function() my_x;
    setinv <- function(inv) my_inv <<- inv;
    getinv <- function() my_inv;
    list(set=set, 
         get=get, 
         setinv=setinv,
         getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cachemean <- function(x, ...) {
   my_inv <- x$getinv()
    if(!is.null(my_inv)) {
        message("Looking up data from cache")
           my_inv
    }
    data <- x$get()
    my_inv <- solve(data, ...)
    x$setinv(my_inv)
    my_inv
}
