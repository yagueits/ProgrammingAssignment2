## As asked for the assignmente, these functions computes the inverse of the previous list calculated by makeCacheMatrix.
## If the inverse has already been calculated, "cachesolve" get the inverse from the cache. 
## 

## This function creates a list (as in the example of the course), from when you can get the value ot the inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) I <<- Inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Depending on if the Inverse has already been calculated, this funciton calculates or gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
