## This file is the submission of Programming Assignment 2
## It contains 2 functions; makeCacheMatrix and cacheSolve

## The following function creates a cache matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { # set data to inverse
        x <<- y
        i <<- NULL
    }
    get <- function() x # get data to inverse
    setsolve <- function(solve) i <<- solve # set inverse matrix
    getsolve <- function() i # get inverse matrix
    list(set = set, get = get, # create list of data and inverse matrix
    setsolve = setsolve,
    getsolve = getsolve)

}


## The following function will calculate the inverse of the matrix
## created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve() # Retrieve cached data
        if(!is.null(i)) { # If cached data exists
            message("getting cached data")
            return(i) # Return cached data
        }
        data <- x$get() #If cached data does not exist, get data to calculate
        i <- solve(data, ...) # Use data to solve for inverse matrix
        x$setsolve(i)   # Set the calculated matrix in cache
        i   # Return inverse matrix
}
