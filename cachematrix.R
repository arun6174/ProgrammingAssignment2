## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly. The
## following two functions are used to compute and cache the inverse of a matrix.


## The following function creates a special "matrix" object that can cache its inverse.
## It creates and returns a list containing four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function returns the inverse of the given matrix. It first checks if
## the inverse has already in the cache and if so, it returns the cached value skipping
## the computation. If not, it computes the inverse and also puts it in the cache via
## setinverse function. The function assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
    if(!is.null(inv)) {
    	message("getting cached data...")
		return(inv)
    }
    matrix <- x$getmatrix()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
