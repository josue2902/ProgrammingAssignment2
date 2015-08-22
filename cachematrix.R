## Caching the Inverse of a Matrix:
## Matrix inversion usually a costly computation but we can have some 
## benefit to caching the inverse of Matrix intead of compute it repeatedly.
## Below are we have some functions that used on the creation of a special object to 
## stores a matrix and caches its inverse.

## In the function we will creates a special "matrix" object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invst <- NULL
        set <- function(y) {
                x <<- y
                invst <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invst <<- inverse
        getInverse <- function() invst
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function computes inverse of "matrix" created  
## If the inverse has been calculated (and the matrix has not value cachet)
## Then it will be retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Here we Return matrix inverse of 'x'
        invst <- x$getInverse()
        if (!is.null(invst)) {
                message("getting cached data")
                return(invst)
        }
        mat <- x$get()
        invst <- solve(mat, ...)
        x$setInverse(invst)
        invst
}
