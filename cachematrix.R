## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an object, which keeps both
## original matrix and inverse one. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## variable to keep inverse matrix. 
    ## set original matrix. Note: it does not calculate the inverse matrix
    ## by default.
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## get original matrix
    setinv <- function(inverse) inv <<- inverse ## set inverse matrix
    getinv <- function() inv ## get inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns inverse matrix for x, which is 'cachematrix'. 
## if there is no cached inverse matrix, it calculates new one.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    
    ## check if there is the previously set value, 
    ## because calculating inverse matrix might be expensive.
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    
    m <- x$get()
    
    ## calculate inverse matrix only if the original one is not null
    if (!is.null(m)) {
        m <- solve(m, ...) ## reusing m, so we can return its value
        x$setinv(m)
    }
    m
}
