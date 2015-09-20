## Coursera R Programming Assignment 2
## 2015-09-20 dsherman

## Create a wrapper for a matrix capable of holding a
## inverting a matrix. When inverting the matrix, hold
## the inversion in a cache, and use the cached inversion
## for repeated calls.

## Cacheable Invertible Matrix: 
## set(theMatrix)
## get() returns theMatrix
## setInverse(invMatrix)
## getInverse() returns invMatrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<-y
        m <<-NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setInverse = setInverse, getinv = getinv)
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
        x$setInverse(inv)
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
