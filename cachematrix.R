## Put comments here that give an overall description of what your
## functions do
# the functions compute the inverse of a matrix and cache it 

## Write a short comment describing this function
# create a matrix  object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){					# define a function where the argument is a matrix
	m  <- NULL
 	set <- function (y) {			
 		x <<- y
		m <<- NULL
 	}
      get <- function() x
	setinverse <- function(solve) m <<- solve				# determine the inverse of a matrix
	getinverse <- function() m
      list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}	

# function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {							# check if the inverse is already available or not
                message("getting cached inverse")
                return(m)							# return already solved inverse
        }
        data <- x$get()
        m <- solve(data)							# determine the inverse if not already available
	  x$setinverse(m)
        m
}

# run the function for any square matrix x
cacheSolve(makeCacheMatrix(x))

