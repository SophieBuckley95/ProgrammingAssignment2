## A function to cache the inverse of a special "matrix" object. A function to
## calculate the inverse of the special "matrix" returned by the previous
## function, but to retrieve it from the cache if it has already been calculated


## This function creates a special "matrix" which is a list containing functions
## to set and get the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getsolve <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calcualtes the mean of the special "matrix" created with the
## above function, but first checks to see if it has already been calculated
## and stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
