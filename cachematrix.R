## The functions below perform matrix inversion and cache
##the inverse of a matrix.  This assumes that the matrix supplied
##is always invertible.

## makeCacheMatrix creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix 
##returned by makeCacheMatrix.  If the inverse has
##already been calculated, then cacheSolve returns 
##the inverse from the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
