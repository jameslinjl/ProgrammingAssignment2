## Coursera R Programming Assignment 2
## The functions are used for cacheing potentially useful values
## within an object so they don't need to be re-calculated later

## instantiates matrix with cache and associated functions

makeCacheMatrix <- function(x = matrix()) {
    cacheinv <- NULL
    set <- function(y) {
        x <<- y
        cacheinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) cacheinv <<- inv
    getinv <- function() cacheinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## checks if cache is hot and either returns cached value
## or does computation and stores in cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("retrieving from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
