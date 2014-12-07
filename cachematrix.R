## Matrix inversion is usually a costly computation when computed repeatedly.
## The next functions compute the inverse of a square matrix and cache the results.

## This function creates a special "matrix" object that can cache its inverse,
## where input matrix x is optional

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" x returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve returns the inverse from the cache, else the inverse is computed and returned.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
