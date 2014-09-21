## Followings are created to calculate Inverse of a given matrix.
## Matrix Inverse is calculated, cached for future use and then returned.
## If the matrix Inverse had been already calculated earlier, then it is retrieved from cache and then returned.

## Function makeCacheMatrix:
## It takes as input a matrix which is Invertible.
## It defines a function to set an Invertible matrix. It defines a function to retrieve the matrix.
## It defines a function to set Inverse of the matrix in cache. It defines a function to retrieve the Inverse of the matrix from cache.
## It returns a named list of functions defined above.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(m){
		x <<- as.matrix(m)
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse)	i <<- as.matrix(inverse)
	getInverse <- function() i
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function cacheSolve:
## It takes as input a list returned by makeCacheMatrix having get and set functions .
## Using get methods it stores matrix and its inverse in local variables.
## It checks if inverse is there in cache, if it is there, it retrieves that and returns.
## If the inverse is not in cache, it calculates it, sets it using set function from the list and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$get()
	i <- x$getInverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	i <- solve(m, ...)
	x$setInverse(i)
	i		
}
