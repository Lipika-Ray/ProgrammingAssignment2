## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly. 
## Below are two functions: First function 'makeCacheMatrix' is used to create a special "matrix" object 
## that can cache its inverse. 
## Second function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## The first function makeCacheMatrix creates a special 'matrix' which is really a list 
## containing function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invr <<- solve
        getinverse <- function() invr
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it 'get's  the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the 'setinverse' function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        matr <- x$get()
        invr <- solve(matr, ...)
        x$setinverse(invr)
        invr
}
