## This file has two functions that let you cache the inverse of a matrix
## instead of recalculating it every time, since computing an inverse can
## be slow for big matrices.

## This function doesn't actually calculate anything itself. It just sets
## up a "container" for a matrix that also has a spot to store its inverse
## once we figure it out. It gives back 4 little functions so we can:
## - put a matrix in (set)
## - get the matrix back out (get)
## - save the inverse once we've calculated it (setinverse)
## - grab the saved inverse later (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function actually gets the inverse. First it checks if we already
## saved one earlier (using getinverse). If we did, it just hands that
## back and skips the math. If not, it calculates the inverse with solve(),
## saves it for next time, and then returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}