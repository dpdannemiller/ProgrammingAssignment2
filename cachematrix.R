## cachematrix

## The 2 functions in this file work together to store a matrix and its inverse.
## The matrix inverse is stored as part of the matrix object. This allows the
## inverse to be fetched repeatedly without recomputation.

## History:
## Written by David Dannemiller, 8/22/2014
## Programming Assignment #2
## R Programming Language, Coursera

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## x - matrix
    ## i - matrix inverse, or null if inverse needs to be calculated

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve will retrieve the inverse
## from the cache (avoids unnecessarily recomputing the inverse).

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
