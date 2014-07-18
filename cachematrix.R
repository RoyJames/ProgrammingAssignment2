## The function makeCacheMatrix receives a matrix and returns a list
## containing the internal functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inversion) inv <<- inversion
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function cacheSolve receives a list of functions defined above
## and calculate the inversion of a matrix and cache the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
