# Coursera course R-Programming
# Programming Assignment 2 (week3)
# Author: Gonzalo Munoz


# makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setinverse <- function(inverse_input){
        inverse <<- inverse_input
    } 
    
    getinverse <- function(){
        inverse
    }
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


# cacheSolve returns the inverse matrix of a given matrix (having previously "Cacheed" it)
# Before calculating the inverse of the input matrix, if first check whether we had previously calculated its inverse 
# In case we did, it returns it inverse value (which was stored in the cache list) and does no calculations
# In case we did not, it computes the inverse and stores it in the cache list
# WARNING: We assume that the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}




