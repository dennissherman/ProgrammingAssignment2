## Coursera R Programming Assignment 2

## Create a cacheable matrix object. 
## Use a factory pattern to create an object that can
## hold a matrix and its inversion. When inverting
## the matrix, hold the inversion in a cache, and use
## it when possible.

## Cacheable Matrix factory. 
## set() the matrix
## get() the matrix
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<-y
        m <<-NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Solve (invert) the matrix. If that has been done already, 
## return the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # return from cache if available
    if (is.null(inv)) {
        mtx <- x$get()
        inv <- solve(mtx)
        x$setinv(inv)
    }
    inv
}

# test function, compare first and second run times

# mtx is an invertible matrix
timer <- function(mtx) {
    
    t <- makeCacheMatrix(mtx)
    
    start.time <- Sys.time()
    cacheSolve(t)
    print( Sys.time() - start.time)
    
    start.time <- Sys.time()
    cacheSolve(t)
    print( Sys.time() - start.time)
    
    start.time <- Sys.time()
    cacheSolve(t)
    print( Sys.time() - start.time)
    
}
