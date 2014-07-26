# makeCacheMatrix : A function that creates a special matrix object and caches its inverse
#cacheSolve: A function that computes the inverse of the special "matrix" returned by makeCacheMatrix.


# makeCacheMatrix : A function that creates a special matrix object and caches its inverse.
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

#cacheSolve: A function that computes the inverse of the special "matrix" returned by makeCacheMatrix.
# if the matrix inverse has already been calculated before, then the cached value will be returned.

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
