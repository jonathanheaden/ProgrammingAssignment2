## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a closure to store 4 functions; get & set the matrix & cached inverse 
## Inverse of x needs to be initialised (and reset in set function) to null to allow testing for existence of cached result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    #internalMatrix <<- x
    set <- function(newMatrix) {
        x <<- newMatrix
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseOfM) m <<- inverseOfM
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse )
    
}


## Write a short comment describing this function
## This function works on a special makeCacheMatrix object created with the above function. 
## This function checks if there is a previuosly computed inverse of the matrix and returns that if present
## otherwise it computes the inverse and stores that in cache
## the object resets the cached inverse if the internal matrix is changed 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedInverse <- x$getinverse()
    matrixToSolve <- x$get()
    if(!is.null(cachedInverse)) {
        cachedInverse$get()
    }
    else {
        newInverse <- solve(matrixToSolve)
        x$setinverse(newInverse)
        newInverse
    }
}

