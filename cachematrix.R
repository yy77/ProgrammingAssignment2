## Matrix inversion will be cached to save unnecessary calculation

## create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invert) inv <<- invert
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## compte the inverse of the special matrix object created by the function above, it will return cached inversion if possible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inversion for the matrix")
        return(inv)
    }
    m <- x$get()
    ## assume that matrix is always invertible
    inv <-solve(m, ...)
    x$setinv(inv)
    inv
}
