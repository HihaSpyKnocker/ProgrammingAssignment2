## I have tested makeCacheMatrix and cacheSolve using testcases provided by 
## Alan E. Berger, made available at: 
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

## makeCacheMatrix creates a special "matrix", which is a list containing
## a function to: 
## 1: set the values of the matrix
## 2: get the values of the matrix
## 3: set the value of the inverse 
## 4: get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y) {	
		x <<- y				
		inv <<- NULL		# clear cache, when new inverse is set	
	}
	
	get <- function()x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
}


## cacheSolve calculates the inverse of the special "matrix" created by 
## makeCacheMatrix. It first checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	
	if(!is.null(inv)) { 	
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get() 		
	inv <- solve(data, ...)	# compute the inverse of the matrix 
	
	x$setinverse(inv)				
	inv
	
}
