## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix is used to create a list which can be used to  set/get value of a matrix, and set/get value of the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) { 
	m <- NULL 
	set <- function(y) {
	x <<- y ##set value of the matrix but not the inverse
	m <<- NULL
	}
	get <- function() x #get value of the matrix
	setinverse <- function(inverse) m <<- inverse #set value of the inverse of the matrix
	getinverse <- function() m #get value of the inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve is used to return the inverse value of the matrix. 

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	## First, check whether the inverse has already been computed. If the matrix
	## is already computed, then gets the results and skips the computation. If
	## not, then compute the inverse of the matrix, set the value of the inverse 
	## using setinverse function. 
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
		}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m ## Return a matrix that is the inverse of 'x'
}
