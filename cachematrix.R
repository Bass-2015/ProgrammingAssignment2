## Matrix Inversion is usually a costly computation and there may be some
## benefit to caching the Inverse of a Matrix rather than compute it
# repeatedly.  Two functions - makeCacheMatrix and cacheSolve - can be used
# to cache the Inverse of a Matrix.


## This function creates a Matrix object that can cache its Inverse.
## As described in the Coursera R-Programming course (offered by JHU), this
## function does 4 tasks, as follows:
## 1. Set the value of the Matrix
## 2. Get the value of the Matrix
## 3. Set the value of the Inverse of the Matrix
## 4. Get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    invers <- NULL
    set <- function(y) {
            x <<- y
            invers <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invers <<- inverse
    getinverse <- function() invers
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function computes the Inverse of the Matix returned by makeCacheMatrix
## above.  If the Inverse has already been calculated (and the Matrix has not)
## changed), the the cacheSolve function would retrieve the Inverse from the
## cache.

cacheSolve <- function(x, ...) {
    invers <- x$getinverse()
    if(!is.null(invers)) {
          message("getting cached data")
          return(invers)
    }
    data <- x$get()
    invers <- solve(data)
    x$setinverse(invers)
    
          ## Return a matrix that is the inverse of 'x'
  invers
}

