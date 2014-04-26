## R Programming Assignment 2
## Use a "special matrix" that can cache the inverse and a function
## to solve.

## Create a special "matrix" object that can cache it's inverse
## Returns a list with attributes
## $set set the underlying matrix
## $get get the underlying matrix
## $setinverse set cached value of inverse
## $ getinverse retrieve cache valued of inverse, if set

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## computes the inverse of 'x' where x is the "special matrix"
## created using makeCacheMatrix. 
## Returns the inverse of the special matrix, using cached value
## if previously set
cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x' 
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}
