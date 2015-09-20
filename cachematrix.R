## Coursera R Programming Assignment 2
## 2015-09-20 dsherman


## Invert a matrix, and hold the inversion in a cache.

## Create a "cacheable matrix", a wrapper for a matrix 
## capable of holding the matrix and it's inversion. 
## * set(theMatrix)
## * get() returns theMatrix
## * setInverse(invMatrix)
## * getInverse() returns invMatrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<-y
        m <<-NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(
          set = set
        , get = get
        , setInverse = setInverse
        , getInverse = getInverse
        )
}

## Use the cacheable matrix to Solve (invert) the matrix. If that has been done already, 
## return the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInverse()
    
    # return from cache if available
    if (is.null(invMatrix)) {
        theMatrix <- x$get()
        invMatrix <- solve(theMatrix)
        x$setInverse(invMatrix)
    }
    invMatrix
}

# test function, compare first and second run times

# theMatrix is an invertible matrix
timer <- function(theMatrix) {
    
    t <- makeCacheMatrix(theMatrix)
    
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
