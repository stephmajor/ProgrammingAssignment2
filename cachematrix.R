## First create a special type of matrix that will cache its inverse once it is computed
## Then a matching solve function is created to compute and cache the inverse (or use existing cached value)

## Makes a special matrix where the inverse is cached when possible
makeCacheMatrix <- function(x = matrix()) {
    # create a place holder for the inverse of x
    inverse <- NULL
    
    # accessors for the matrix
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    
    # accessors for the inverse
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    # return the list of all accessor functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special matrix "x" (using cached value when available)
cacheSolve <- function(x, ...) {
    # Start by seeing if a cached value is availale
    cached <- x$getInverse()
    if (!is.null(cached)) {
        message("using the cached inverse")
        return(cached)
    }
    
    # The cache is NULL, so let's calculate it, store it and finally return it
    m <- x$get()
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    
    # Finally return the inverse we just calculated
    inverse
}
