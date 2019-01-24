## Coursera - Johns Hopkins - R Programming - Week 3 - Assignment 2

## This pair of functions computes the inverse of a matrix and creates an object
## to associate the matrix with its inverse in the cache. The computation 
## function checks for a pre-existing computed inverse to avoid unneccessary 
## repeated computation.

## makeCacheMatrix creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){   # Defaults to 1x1 NA matrix
    inverse <- NULL                          # Will hold matrix inverse
    set <- function(y){
        x <<- y                              # Set matrix in parent environment
        inverse <- NULL                      # Resets inverse
    }
    get <- function(){x}                     # Returns x produced by set funct
    setinverse <- function(z){
        inverse <<- z                        # Set inverse in parent environment
    }
    getinverse <- function(){inverse}        # Returns the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the 'matrix' created by makeCacheMatrix. 
## If the inverse of a given matrix has already been computed, it retrieves
## this from the cache rather than re-computing it.

cacheSolve <- function(x, ...){
    inverse <- x$getinverse
    if(!is.null(inverse)){                   # Checks for a preexisting inverse
        message("getting cached data")
        return(inverse)                      # Returns the cached inverse
    }
    nocache <- x$get()
    inverse <- solve(nocache, ...)           # If no cached matrix, solve
    x$setinverse(inverse)
    inverse
}