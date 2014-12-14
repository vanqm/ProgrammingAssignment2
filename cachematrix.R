
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## set the value of the vector
## get the value of the vector
## set the value of the solve
## get the value of the solve

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Test case 1
# mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
# mat2 <- makeCacheMatrix(mat)
# cacheSolve(mat2)
## Result
#      [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

## Create a special matrix that is invertable
## x: is a matrix that is invertable - nrow == ncol
makeCacheMatrix <- function(x = matrix()) {
	# From description of Project Assignment 2:
	# For this assignment, assume that the matrix supplied is always invertible.
	# So we do not need checking the matrix is invertable or not
	
	m <- NULL
	# 'set' function will set the new matrix to 'x'
	set <- function(y) {
		# cache the value of matrix
		x <<- y
		m <<- NULL
	}
	# get the value of matrix that cached
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	
	# return a list contains set, get, setsolve, getsolve functions
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## Return the inverse of matrix
## x: a list - return from 'makeCacheMatrix' function
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	m <- x$getsolve()
	# check 'm' is NULL or not
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	# return the inverse
	m
}
