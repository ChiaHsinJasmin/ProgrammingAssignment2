#Programming Assignment 2: Lexical Scoping

## Functions cacheSolve could cache the inverse of the matrix from makeCacheMatrix

## makeCacheMatrix should be able to create a specific matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(Inverse) inverse <<- Inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve should be able to compute the inverse of the matrix returned by makeCacheMatrix
## It should retrieve the inverse from the cache if the inverse has been calculated by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
