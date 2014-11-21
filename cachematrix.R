## Caching the Inverse of a Matrix

## This assignment is to write a pair of frunctions
## that cache the inverse of a matrix

## Create a function that can be used to repeteadly
## solve the inverse  of a matrix, but only calculates
## the inverse once

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <- NULL
        }
        get <- function () x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set=set, get=get,
                setInverse=setInverse,
                getInverse=getInverse)
}


## Return the inverse of an CacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invFunc <- x$getInverse()
        if(!is.null(invFunc)) {
                message("getting cached data")
                return(invFunc)
        }
        data <- x$get()
        invFunc <- solve(data, ...)
        x$setInverse(invFunc)
        invFunc
}
                

