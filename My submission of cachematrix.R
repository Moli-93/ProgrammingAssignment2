## A pair of functions that can cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its own inverse.

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(matrix){
                m <<- matrix
                inv <<- NULL
        }
        get <- function() {m}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}



## This functions computes the inverse of the special "matrix" created by 
## the function above (makeCacheMatrix). If the inverse has already 
## been calculated (and the matrix hasn't changed), then it will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
}
