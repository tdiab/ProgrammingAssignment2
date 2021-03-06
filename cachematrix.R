## Put comments here that give an overall description of what your
## functions do
## These commits are made via the site directly...
##----------------------------------------------------------------
## The following pair of functions will return the inverse of an
## invertible matrix, once the inverse is calculated, it will be
## cached. If the inverse has already been calculated, the cached 
## inverse will be returned, otherwise, a new inverse will be 
## calculated, cached and returned.
##----------------------------------------------------------------

##----------------------------------------------------------------
## Write a short comment describing this function
## This function returns a list of objects, 
## 1. a set function that will set the value of the matrix
## 2. a get function that will return the matrix
## 3. a setinverse function that will set the inverse of the given matrix
## 4. a getinverse function that will get the inverse of the matrix 
## Return a list (set, get, setinverse, getinverse)
##
## Assumption: x is an invertible matrix
##
##---------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {

	## initialize the inverse (inv) of the matrix (x) 
        inv <- NULL
        
        ## Cache both variables , x = the matrix, and inv = the inverse 
	set <- function(inverse){
		x <<- inverse
		inv <<- NULL
	}
	
	## get the original data (x)
	get <- function() x
	
	## set the inverse to the calculated inverse
	setinverse <- function(inverse) inv <<-inverse
	
	## get the inverse 
	getinverse <- function() inv

	## Returen a list of objects, mainly functions 
	list(	set=set,
		get=get,
		setinverse=setinverse,
		getinverse=getinverse)

}


##----------------------------------------------------------------- 
## Write a short comment describing this function
## This function checks if the inverse of the provided matrix has 
## already been calculated and cached, if so returns it, otherwise,
## calculates the inverse, cache it then return it.
##
## Check if the matrix is already solved, then return the inverse, 
## otherwise solve it
##-----------------------------------------------------------------


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Get the inverse from the provided object (of class list)
	inv <- x$getinverse()

	## Checks if the inverse has been cached
	if(!is.null(inv)){
		## Inverse has been cached, present a message and return the cached value
		message("Getting cached inverse")
		return(inv)
	}

	## Inverse has NOT been cached
	## Retrieve the data for the original matrix from the list (x)
	data <- x$get()

	## Calculate the inverse of the original matrix, using the built-in solve function
	inv <- solve(data)

	## Set the calculated inverse of the provided matrix
	x$setinverse(inv)

	## Return the calculated inverse
	inv

}
