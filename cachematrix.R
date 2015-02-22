## These functions creates an object which stores a matrix
## and its inverse, calculating the inverse it has no
## already been generated.

## makeCacheMatrix creates an object capable of storing 
## a matrix and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setInverse <- function(inv) invX <<- inv
    getInverse <- function() invX
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve checks for a cached value of the inverse
## matrix of matrix passed in.  If not available, the 
## inverse of a matrix is calculated and stored.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (is.null(inv)){
        message("Calculating Inverse Matrix")
        m = x$get()
        inv = solve(m)
        x$setInverse(inv)
    }
    inv
}
