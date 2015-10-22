## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special matrix which can cache its inverse.
# There are 4 functions defined in the special matrix
# get returns the special matrix
# set sets the matrix. If the matrix is equal to the existing one, nothing is changed and the cached values are not cleared
# getinv returns the cached inverse
# setinv sets the inverse of the matrix
 

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	z <- x

	set <- function(y) {
	        
	        if ( matequal(z,y) ==FALSE ){
	        	z <<- y
	        	inv <<- NULL
	        }

	}

	get <- function() z
	
	setinv <- function(inverse) inv <<- inverse
	
	getinv <- function() inv

	matequal <- function(x,y) is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

	list(set = set, get = get, setinv = setinv, getinv = getinv, matequal = matequal)

}


## Write a short comment describing this function

# This function computes the inverse of a matrix
# It excepts the special matrix created in the previous example as an argument
# It checks ff the inverse of special matrix has already been computed and cacched. If so it returns cached value
# Otherwise it computes the inverse and caches its value apart from returning it to the callee.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinv()

    if(!is.null(inverse)) {
            message("Getting Cached Data")
            return(inverse)
    }

    matrix <- x$get()

    inverse <- solve(matrix, ...)
    
    x$setinv(inverse)
    
    return(inverse)        
}
