## These functions reduce the computation needed to find
## the inverse of a matrix by caching the inverse the 1st
## is it calculated

## cache the inverse of x and provide methods for
## setting and getting that inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## get the inverse of x, using getinverse to get
## it from cache or setinverse to cache it

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        message("getting data")
        return(i)
}

## testing data

y = matrix(c(1,2, 6,8), nrow=2, ncol=2)
x <- makeCacheMatrix(y)