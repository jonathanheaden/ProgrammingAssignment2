## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix() #inverse of x
    set <- function(newInverse) {
        m <<- newInverse
    }
    get <- function() x
    setinverse <- function(inverseOfM) m <<- inverseOfM
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse )
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedInverse <- x$getinverse()
    if(!is.null(cachedInverse)) {
        cachedInverse$get()
    }
    else {
        newInverse <- solve(x)
        x$setinverse(newInverse)
        newInverse
    }
}

