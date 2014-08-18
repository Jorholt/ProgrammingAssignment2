## These functions caches the inverse of a matrix

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) mx <<- solve
    getmatrix <- function() mx
    list(set = set, get = get,
         setmatrix = setmatrix
         getmatrix = getmatrix)
}


## This function computes the inverse of the matrix from the prevoius
## function. If the inverse already exists, the inverse is retrived
## from the cache.

cacheSolve <- function(x, ...) {
    mx <- x$getmatrix()
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    data <- x$get()
    mx <- solve(data, ...)
    x$setmatrix(mx)
    mx
        ## Return a matrix that is the inverse of 'x'
}
