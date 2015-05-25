## This funtion creates a Matrix
## set the matrix value     
## get the matrix value     
## set the value of inverse of the matrix    
## get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matr) {
        x <<- matr
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function returns the inverse of the matrix created by makeCacheMatrix.
## Check if the inverse is solved already	
## if YES, just return the inverse, 
## otherwise, the function must solve it.
cacheSolve <- function(x, ...) {
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
