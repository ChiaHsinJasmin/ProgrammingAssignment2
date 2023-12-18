# Programming Assignment 2: Lexical Scoping

## Functions cacheSolve could cache the inverse of the matrix from makeCacheMatrix

## makeCacheMatrix should be able to create a specific matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        library(matlib)
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve should be able to compute the inverse of the matrix returned by makeCacheMatrix
## It should retrieve the inverse from the cache if the inverse has been calculated by makeCacheMatrix

cacheSolve <- function(x, ...) {
        library(matlib)
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
