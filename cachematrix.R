## This code will have two functions with the purpose of inverting 
## a matrix. Because inverting a matrix can be time consuming this 
## code will cache the result and then check future queries to see
## if it can use the cached data instead of recalculating the inverse.

## makeCacheMatrix takes a matrix as an argument and then creates a
## list of 4 functions to set or get the matrix and set or get the
## inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will calculate the inverse of the matrix created with
## makeCacheMean. First it checks to see if the inverse has already
## been calculated. If the inverse has been calculated it will get 
## the data from the cache instead of recalculating. If there is no
## data in the cache it will calculate the inverse and save it to the
## cache with the setinv function.
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
