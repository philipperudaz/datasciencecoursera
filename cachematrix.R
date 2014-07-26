## Put comments here that give an overall description of what your
## functions do

x <- matrix(rnorm(4), nrow = 2)

## this function will store the cached  inverse matrix, 
## set the matrix, get the matrix, set the inverse, 
## and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
       
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will solve the inverse of the matrix and return the cached inverse, 
## if the inverse is already calculated. If not, it calculates it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

