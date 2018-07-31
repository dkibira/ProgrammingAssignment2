## Put comments here that give an overall description of what your
## functions do
# the functions compute the inverse of a matrix and cache it 

## Write a short comment describing this function
# this function does compute the inverse of a matrix and catches it 
makeCacheMatrix <- function(x = matrix()) {
	m  <- NULL
 	set <- function (y) {
 		x <<- y
		m <<- NULL
 	}
      get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
      list(get=get, setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function
# check if the inverse is already available or not
# it returns already solved inverse
# if not available, it determine the inverse of the passed matrix

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
	  x$setinverse(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
