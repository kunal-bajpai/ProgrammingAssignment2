## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creates a list with methods to get and set data and inverted matrix in inv
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setinv <- function(i) inv <<- i
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#Checks if value is cached and returns it if it is else computes the invert, caches it and returns
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data = x$get()
        inv <-solve(data,...)
        x$setinv(inv)
        inv
}
