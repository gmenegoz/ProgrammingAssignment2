## This pair of functions computes the inverse of a given matrix.
## The inverse matrix is  then cached, to avoid computing it repeatedly without reason

## makeCacheMatrix returns a list that stores the input matrix 'matr'
## and caches its inverse

makeCacheMatrix <- function(matr = matrix()) {
    inv <- NULL
    get <- function() matr
    set <- function(y){
        matr <<- y
        inv <<- NULL
    }
    getinverse <- function() inv
    setinverse <- function(inverted) inv <<- inverted

    list(get = get, set = set,
         getinverse = getinverse,
         setinverse = setinverse)
}


## cacheSolve employs solve to compute the inverse of a matrix.
## If the inverse was previously stored, it fetches the cached value

cacheSolve <- function(xmatr, ...) {
        ## Return a matrix that is the inverse of 'xmatr'
        inverted <- xmatr$getinverse()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- xmatr$get()
    inverted <- solve(data, ...)
    xmatr$setinverse(inverted)
    inverted
}
