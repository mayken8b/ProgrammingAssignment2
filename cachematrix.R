## The makeCacheMatrix function sets a matrix and also
## returns a list of values with which cacheSolve can
## interact to check for a cached inverse matrix 
## before solving for the inverse, eliminating unnecessary solving.

## The makeCacheMatrix function caches a matrix when called.
## It also provides functions to return the matrix, cache an inverse
## of the matrix, and return the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function(y) {  ## cache a matrix
        x <<- y
        invmatrix <<- NULL
    }
    get <- function() x  ## retrieve a cached matrix.
    
    ## The setinverse function caches the inverse of the cached matrix.
    ## This will require the cacheSolve function to complete.
    setinverse <- function(inverse) invmatrix <<- inverse

    getinverse <- function() invmatrix  ## return a cached inverse
    
    ## The following list is the returned values for cacheSolve to use.
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function checks for a cached inverse matrix that has already
## been determined.  If one exists already in makeCacheMatrix, it is returned.
## If one does not exist, it is calculuated, cached in makeCacheMatrix with  
## setinverse, and returned as well.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinverse()
    if(!is.null(invmatrix)) {  ## check for a cached inverse to return
            message("getting cached data")
            return(invmatrix)
    }
    data <- x$get()
    invmatrix <- solve(data, ...)  ## solve the inverse if it didn't exist
    x$setinverse(invmatrix)  ## cache the solved inverse
    invmatrix   ## return the solved matrix
}
