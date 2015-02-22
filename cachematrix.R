## Put comments here that give an overall description of what your
## functions do


## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solveVal) inv <<- solveVal
	getinverse <- function() inv

	## the output of this function is a list containing the 4 functions defined within it
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

	inv2<- x$getinverse()
	if(!is.null(inv2)) {
		message("getting cached data")
		return(inv2)
	}
	data <- x$get()
	inv2 <- solve(data)
	x$setinverse(inv2)
	## Returns the inverse which had been calculated
	inv2
}
