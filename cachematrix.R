## Coursera R Programming Assignment 2
## 2015-09-20 dsherman


## Invert a matrix, and hold the inversion in a cache.

## Create a "cacheable matrix", a wrapper for a matrix 
## capable of holding the matrix and it's inversion. 
## * set(theMatrix)
## * get() returns theMatrix
## * setInverse(invMatrix)
## * getInverse() returns invMatrix
makeCacheMatrix <- function(theMatrix = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        theMatrix <<-y
        inverseMatrix <<-NULL
    }
    get <- function() {
        theMatrix
    }
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    getInverse <- function() {
        inverseMatrix
    }
    list(
          set = set
        , get = get
        , setInverse = setInverse
        , getInverse = getInverse
        )
}

## Use the cacheable matrix to Solve (invert) the matrix. 
## If the inversion has been done already, 
## return the result from the cache.
cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- cacheMatrix$getInverse()
    
    # If cache wasn't populated, populate it
    if (is.null(invMatrix)) {
        theMatrix <- cacheMatrix$get()
        invMatrix <- solve(theMatrix)
        cacheMatrix$setInverse(invMatrix)
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
