## Makes a kind of matrix, caches it and 
## prepares for caching its inverse
## params: x .. the matrix which inverse should be cached
## returns: list of functions for getting and setting the matrix and its inverse
##          these functions will be used in the cacheSolve function;
##          the list itself represents the cached matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y             # cache new matrix
        inverse <<- NULL    # clear the former inverse
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of a matrix or returns the inverse from cache
## prepared by the makeCacheMatrix function
## params: x .. the matrix prepared by the makeCacheMatrix function
##         ... .. any additional parameters for the solve function
## returns: the inverse of the matrix
cacheSolve <- function(x, ...) {
    # try to retrieve the inverse from the cache
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    
    # the inverse is not present; compute and cache it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
