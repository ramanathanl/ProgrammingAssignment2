
## By : Ramanathan.L
## Date: April, 20 2016
##
## Description:
## The Functions included helps to create a 
## creates a special "matrix" object that can cache its inverse.
##
## version Control:
## ----------------
## Initial Version - April 24,2016 

## The Function makeCacheMatrix helps to create the special Matrix
## Object that can be cached and has a list of Functions
##      1. Set : To assign the Matrix value
##      2. Get : Retreive the Matrix
##      3. Setinv : Set the Inverse value
##      4. Getinv : Get the Inverse value
##

makeCacheMatrix <- function(x = matrix()) {
        

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)}


## The Function cachesolve helps to compute the inverse of the 
## special Matrix Object passed to it along with cachcing.
## It checks if value is availabe in cache, if it does
## it is retrieved else it is computed
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
