## Both functions compute and cache the inverse of a matrix.

## Takes in a matrix and returns a list of functions (As indicated)
makeCacheMatrix <- function(x = matrix()) {
        # Caches a new data matrix without calculating its inverse
        # using (<<-) instead of (<-)
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the data matrix
        get <- function() x
        # Caches the inverse using (<<-) instead of (<-)
        setinverse <- function(inverse) inv <<- inverse
        # Get the cached inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Takes in a matrix and returns its cached inverse
## Or calculates the inverse and caches it
cacheSolve <- function(x, ...) {
        # See if inverse is already calculated and cached?
        inv <- x$getinverse()
        # If yes, return it and exit.
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If no, get the matrix, calculate the inverse cache it
        # and return it.
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
