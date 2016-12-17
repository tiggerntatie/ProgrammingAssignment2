## cachematrix: collection of functions that implement a caching matrix 
## object

## makeCacheMatrix: return a special matrix object that exists as a vector 
## of functions to set/get the value of the matrix and set/get the value 
## of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        x.inverse <- NULL
        set <- function(y) {
                x <<- y
                x.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x.inverse <<- inverse
        getinverse <- function() x.inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: return the inverse of a special matrix object that was created
## with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x.inverse <- x$getinverse()
        if(!is.null(x.inverse)) {
                message("getting cached data")
                return(x.inverse)
        }
        data <- x$get()
        x.inverse <- solve(data, ...)
        x$setinverse(x.inverse)
        x.inverse
}
