# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # cache for the inverse matrix
    cachex <- NULL

    # Setter
    set <- function(y) {
        x <<- y
        cachex <<- NULL
    }
    
    # Getter
    get <- function() x

    # Setter
    setinverse <- function(inverse) cachex <<- inverse
    # Getter
    getinverse <- function() cachex

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinverse(inv)

    # Return
    inv
}
