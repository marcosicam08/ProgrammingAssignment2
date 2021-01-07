## In this assignment, the matrix supplied is always invertible is assummed.
## Below are a pair of functions that cache the inverse of a matrix.

## A special matrix object is produced in this function that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                  x <<- y
                  inv <<- NULL
                }
                get <- function() x
## Below is the method to use to set and return the inverse of the matrix
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}

## This computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getInverse()
                if (!is.null(inv)) {
                  message("getting cached matrix")
                  return(inv)
                }
                mat <- x$get()
                inv <- solve(mat, ...)
                x$setInverse(inv)
                inv
}
