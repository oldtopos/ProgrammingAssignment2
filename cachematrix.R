## 
##  cacheInverse retains an inverted matrix using a function created with makeCacheMatrix
## 
##  repeated calls to cacheInverse will return the same inverted matrix unless set has been
##  called on makeCacheMatrix since the last invocation of cacheInverse
##
##  see and run testCacheInverse for invocations
##

## 
##  testCacheInverse run a series of tests iteration times
##

testCacheInverse <- function( iterationLimit = 2 )
{
	foo <- makeCacheMatrix()
	
	iteration = 0
	
	while( iteration < iterationLimit )
	{
		foo$set( matrix( runif(25,-50.0,50.0), 5, 5 ) )
		print( cacheInverse( foo ) )	# calculate and return inverse
		print( cacheInverse( foo ) )	# return inverse with cache fetch message
	
		iteration <- iteration + 1
	}
	
	rm( foo )
}


## 
##  makeCacheMatrix stores the matrix x and its inverse in the environment
##  of the function.
##
##  set stores the passed matrix and clears the stored inverse
##  get returnes the passed matrix
##  setInverse stores the matrix inverse
##  getInverse returns the stored inverse, or NULL
##

makeCacheMatrix <- function(x = matrix()) 
{
	inverse <- NULL
	
	set <- function(y) 
	{
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	setInverse <- function(invertedMatrix) inverse <<- invertedMatrix
	getInverse <- function() inverse
	
	list(set = set, get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


##
##  cacheInverse looks for a cached inverse in x and return it if stored
## 
##  If no inverse is stored, the inverse of the matrix stored in x is calulated,
##  cached in x, and returned to the caller
##

cacheInverse <- function(x, ...) 
{
	inverse <- x$getInverse()
	
	if(!is.null(inverse)) 
	{
		message("getting cached data")
		return(inverse)
	}
	
	data <- x$get()
	inverse <- solve(data)
	x$setInverse(inverse)
	inverse
}
