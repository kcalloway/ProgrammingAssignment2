##  Creates a matrix that will cache the inversion result 
##  	when calling cacheSolve
##  Args:
##  x: An invertible matrix
##  
##  Returns:
##  	An object that encapsulates the matrix with metadata that makes matrix inversion cacheable
##  
##  Note: The return value is NOT a matrix
makeCacheMatrix <- function(x = matrix()) {
	inverted.matrix <- NULL
	
	Setter <- function(y)  {
		x <<- y
		inverted.matrix <<- NULL
	}
	
	Getter <- function() x
	SetInversion <- function(inversion) inverted.matrix <<- inversion
	GetInversion <- function() inverted.matrix
	list(Setter = Setter, Getter = Getter, SetInversion = SetInversion, GetInversion = GetInversion )
}


##  Computes the inverse of a cacheable matrix
##  Args:
##  x:   A cacheable Matrix as generated from the "makeCacheMatrix" function
##  ...: Any secondary args that would be ordinarily passed to the "solve" function
##  
##  Returns:
##  	The inversion of the matrix inside of the cacheableMatrix
cacheSolve <- function(x, ...) {
	inverted.matrix <- x$GetInversion()
	
	if (!is.null(inverted.matrix)) {
		message("Loading CachedMatrix")
		return(inverted.matrix)
	}
	
	data <- x$Getter()
	inverted.matrix <- solve(data, ...)
	x$SetInversion(inverted.matrix)
	inverted.matrix
}