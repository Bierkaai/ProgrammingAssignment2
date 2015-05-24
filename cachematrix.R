## cacheMatrix / cacheSolve: create objects to store matrix values and matrix inverse. 
## Because matrix inversion is a complex operation, time can be saved by caching a matrix' inverse

## makeCacheMatrix creates a matrix object (list of functions) that allow storage of a matrix and
## its inverse and provides getter and setter functions for access to these stored values

makeCacheMatrix <- function(x = matrix()) {
        
        # Check wether input actually is a matrix, throw error if it is not
        # This function's responsibility is not conversion of arbitrary types to matrices
        if (!is.matrix(x)) {
                stop('Argument should be matrix')
        }
        
        # Initiate inverse variable as NULL
        inverse <- NULL
        
        # Create an internal set function to set the matrix values and unset the cached inverse
        set <- function(newmat) {
                # Check wether matrix is supplied as argument
                if (!is.matrix(newmat)) {
                        stop('Argument to set should be matrix')
                }
                # reset cached inverse
                inverse <<- NULL
                # set matrix
                x <<- newmat
        }
        
        # Getter for matrix values: anonymous function returning internal mat
        get <- function() x
        
        # Create getters and setters for inverse
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        
        ## Create list of functions to return
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve solves the linear system represented by the matrix, i.e. it inverses the matrix
## Before calculating the inverse, the function checks whether there is a cached inverse available
## if there is, time is saved by returning the cached inverse.

## Arguments: a matrix, created with makeCacheMatrix. Additional arguments are passed on to solve()
cacheSolve <- function(x, ...) {
        ## Get cached data from matrix object
        inv <- x$getinverse()
        
        ## Check whether there is data in the cache and return cached inverse if it is available
        if (!is.null(inv)) {
                message('Cached inverse returned')
                return(inv)
        }
        
        ## If there is no cached data, we have to calculate the inverse by getting the matrix data 
        ## and passing it to solve()
        mat <- x$get()
        inv <- solve(mat, ...)
        
        ## Cache the inverse for the next time it is needed and return the inverse
        x$setinverse(inv)
        inv
}
