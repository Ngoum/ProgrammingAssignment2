## Pair of R functions "makeCacheMatrix" and "cacheSolve" to compute and
## cache the value of the inverse of an invertible matrix

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse
##  - set: sets the value of the matrix
##  - get: gets the value of the matrix
##  - setinv: sets the value of the inverse
##  - getinv: gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## create a special "matrix"
    inv <- NULL
    set <- function(y) {                       # set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                        # get the value of the matrix
    setinv <- function(invers) inv <<- invers  # set the value of the inverse
    getinv <- function() inv                   # get the value of the inverse
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## cacheSolve computes the inverse (if not already available)
## of the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()         # check if inverse is available
    
    if(!is.null(inv)) {       # if available, return cached data
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)   # compute inverse
    x$setinv(inv)             # commit inverse to cache
    inv                        
}