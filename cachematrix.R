## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by the makeCacheMatrix function
## However, if the inverse has already been calculated, then the casheSolve function will retrieve the inverse from the cache

## makeCacheMatrix returns a list containing functions to set and get the values of a invertible matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve firstly checks to see if the inverse of the matrix has already been calculated and cached
## If it has not been calculated and cached, the function sets the value of the inverse in the cache using the setinverse function and also returns the value

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}