## These functions create a wrapper for a matrix with a cache-able
## inverse, and retrieve the inverse upon request. The inverse is lazily
## generated, i.e. it is only calculated upon request

## makeCacheMatrix() generates a wrapper for a matrix, with the 
## associated inverse stored alongside if required. This contains 
## getters and setters for both the matrix and the inverse. If the 
## setter for the matrix is called, then the inverse is reset to NULL

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    set <- function(y){
        x<<-y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse<<- new_inverse
    getinverse <- function() inverse
    list(set = set,get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() queries the CacheMatrix list and returns the inverse.
## If the inverse has been cached it returns the cached matrix, but
## if not it calculates the inverse, sets the cache, and returns the 
## new data

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)){
        return(m)
    }
    new_inverse <- solve(x$get())
    x$setinverse(new_inverse)
    new_inverse
}
