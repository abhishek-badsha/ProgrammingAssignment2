## Description:
## This function creates a special "matrix" object that can cache its inverse. 
## 
## Usage:
## makeCacheMatrix(x)
##
## Arguments
## x Matrix for which the inverse is required to cache

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Description:
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 
## 
## Usage:
## cacheSolve(x, ...)
##
## Arguments
## x special "matrix" obtained from makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
