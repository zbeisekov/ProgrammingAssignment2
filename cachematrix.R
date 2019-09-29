## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list, which caches inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns inverse matrix for x, which is 'cachematrix'. 
## if there is no cached inverse matrix, it calculates new one.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    inv <- solve(x$get(), diag(nrow(x$get())), ...)
    x$setinv(inv)
    inv
}
