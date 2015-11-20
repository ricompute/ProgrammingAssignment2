## This file contains two functions which can calculate and cache the inverse
## of a matrix.

## makeCacheMatrix is a function which creates a special "matrix" object which
## is really a list containing a function to set the value of the matrix, get 
## the value of the matrix, set the value of the inverse of the matrix, and get 
## the value of the inverse of the matrix.

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


## cacheSolve computes and caches the inverse of the special "matrix" retunred 
## by makeCacheMatrix above. If the inverse has already been calculated, the 
## cached inverse is retreived and returned.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
