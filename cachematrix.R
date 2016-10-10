## These functions illustrate how R implements object orientation by creating a
## constructor class (makeCacheMatrix) and operating on objects of this type.


## Builds a set of functions and returns the functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                    # initialize two objects: x and m
        set <- function(y) {                         
                x <<- y
                m <<- NULL
        }
        get <- function() x                          # get the value of the matrix
        setinverse <- function(solve) m <<- solve    # set the value of the inverse
        getinverse <- function() m                   # get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Takes the argument that is returned by makeCacheMatrix() in order to retrieve the 
## inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}