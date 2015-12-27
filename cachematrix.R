## This is an assignment for coursera to write a pair of functions that cache the inverse of a matrix.

## purpose: Matrix inversion is usually a costly computation and there may be some benefit  
## to caching the inverse of a matrix rather than computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## We assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL   # set the cache to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  # set the cache to NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve ## cache it
    getInverse <- function() m
    list(set = set, get = get, # create a list with 4 elements
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() # load m with the inverse (if it exists)
    if(!is.null(m)) {  # check for cache previously stored
        message("getting cached data")
        return(m) # break out and return m
    }
    data <- x$get()
    m <- solve(data, ...)  # invert the matrix
    x$setInverse(m) # store the inverse
    m # insure the function returns the inverse
}

