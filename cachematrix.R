## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that caches its inverse
## Arg: x: matrix
## Output: a list of functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the special matrix in argument
## This function caches the inverse and returns it if already calculated
## Args:
##    x: special matrix to inverse
##    ...: args passed to solve function
## Output: the matrix inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
