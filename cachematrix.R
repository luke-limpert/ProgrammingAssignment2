## Put comments here that give an overall description of what your
## functions do

## Stores the matrix and caches the inverse, a simple list containing a
## function to accomplish the 4 parts below

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    ##Part 1: Set the value of the matrix 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##Part 2: Get the value of the matrix
    get <- function() x
    
##The structure is similar to the example of the initial
##makeVector example
    
    #Part 3: Set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    #Part 4: Get the value of the inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)        
}


## cacheSolve computes the inverse of the matrix, otherwise
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## run the getInverse function
    i <- x$getinverse()
    ## If there is nothing stored in i for the inverse
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## run the get function and solve for the inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}