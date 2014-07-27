## Since finding inverse of a matrix is an expensive operation, here we
## try to cache the inverse once it is calculated so that it need not 
## be calculated again the next time

## function set() stores the matrix, get() returns the matrix
## setinverse() Stores the inverse of the matrix
## getinverse() returns the inverse of the matrix if already cached

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse has been calculated before 
## and returns the inverse 

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
