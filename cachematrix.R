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

# test function, compare first and second run times to show 
# cache in use

# create a test matrix with random numbers
# > set.seed(11235813)
# > r = rnorm(1000000)
# > mat1 = matrix(r, nrow=1000, ncol=1000)

# > test(mat1)

# matrix1 is an invertible matrix
# expect first run of each matrix to take longer than repeats
test <- function(matrix1) {
    
    t <- makeCacheMatrix(matrix1)

    for ( i in 1:3) {
        timer(t)
    }
    print("reset")
    t$set(matrix1)
    for ( i in 1:3) {
        timer(t)
    }
}

## utility to time execution
timer <- function(cm) {
    start.time <- Sys.time()
    cacheSolve(cm)
    print( Sys.time() - start.time )
}
