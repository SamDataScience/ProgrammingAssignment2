# For a potentially time-consuming computation, it makes sense to cache the value of the computed object so that 
# when we need it, it can be looked up in the cache rather than recompute it again. 
# The following two functions are created to computed and cache the inverse of a matrix object.

# makeCacheMatrix : This function creates a special matrix object and caches its inverse.
# the function returns a list containing a function to 
#set the value of the Matrix
#get the value of the Matrix
#set the value of the Inverse
#get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {

		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse  <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse  = getinverse) 
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# if the matrix inverse has already been computed before, then the cached value will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached Matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
		inv
}
