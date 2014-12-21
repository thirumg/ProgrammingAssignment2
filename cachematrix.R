## Put comments here that give an overall description of what your
## functions do

## Makes a "special" using the given matrix x.
## The "special" matrix can hold a matrix and optionally its inverse.
## The "special" matrix is a list with four members:
##     set - to set the value of the matrix. Any previous inverse will be
##           cleared
##     get - to the value of the matrix
##     setinverse - set the inverse of the matrix
##     getinverse - to get the inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() {
        x
    }

    set <- function(v) {
        x <<- v
        inv <<- NULL
    }

    setinverse <- function(i) {
        inv <<- i
    }

    getinverse <- function() {
        inv
    }

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves (i.e. computes the inverse of) the matrix in x.
## x must have been constructed using makeCacheMatrix().
## If the matrix has been solved already, it returns the value
## computed earlier.

cacheSolve <- function(x, ...) {
    result <- x$getinverse()
    if (is.null(result)) {
        result <- solve(x$get(), ...)
        x$setinverse(result)
    }
    result
}
