## Put comments here that give an overall description of what your
## functions do

#creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#computes the inverse of the special "matrix" returned by makeCacheMatrix
#If inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i <-x$getinverse()
        if(!is.null(i)) {
                message("getting chached data")
                return(i)
        }
        data <- x$get()
        i <- getinverse(data, ...)
        x$setinverse(i)
        i
}