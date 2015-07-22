## write a pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## set inverse to null if not calculated yet
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        #return a list of above functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## check if inverse already available in the cache
        i <- x$getinv()
        ## return the inverse if already avaiable in cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## get the matrix
	data <- x$get()
	## invert the matrix
        i <- solve(data, ...)
        ## set the inverse in the cache
        x$setinv(i)
        ## return the inverse
        i
}

## to test the functions 
### mat <- matrix(rnorm(9),3,3)
### mmat <- makeCacheMatrix(mat)
### matinv <- cacheSolve(mmat)

## check answer
### matinv
### solve(mat)