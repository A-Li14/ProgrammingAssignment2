## makeCacheMatrix creates an R object that stores a matrix and another object.
## cacheSolve takes in a Cache Matrix and checks whether it has a cached object.
## If nothing is cached, it computes and caches the inverse of the object.

## Creates a matrix capable of caching another R object for retrieval later.

makeCacheMatrix <- function(x = matrix()) {

	 m <- NULL
	 set <- function(y) {
		x <<- y
	    m <<- NULL
	 }

	 get <- function() x
	 setcache <- function(cache) m <<- cache
	 getcache<- function() m
	 list(set = set, get = get, 
          setcache = setcache, getcache = getcache)

}


## Returns the cache of a Cache Matrix, x, or computes and caches
## the inverse of x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getcache()
	if(!is.null(m)) {
		message("returning cached object")
		return(m)	
	}
	message("computing inverse")
	data <- x$get()
	m <- solve(data,...)
	x$setcache(m)
	m

}
