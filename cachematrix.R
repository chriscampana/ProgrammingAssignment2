## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix, which is actually a list that contains functions to 
## 1) Set value of matrix
## 2) Get value of matrix
## 3) Set inverse of matrix
## 4) Get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
}


## this function calculates the inverse of the special matrix created with the function above.  But it checks if the inverse is already cached in memory.  If so, it will skip calculating the inverse.  If not, it will calculate and cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting chached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
