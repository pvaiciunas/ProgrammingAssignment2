## The following functions help aid in reducing computations for 
## matrix inversion by caching the solution of an inversion, and 
## then only recomputing if the solution does not yet exist.
## It is assumed that all matrices passed to these functions
## are invertible.

## This function creates a list object that contains the necessary
## functions to set and retrieve the solution to a matrix inversion

makeCacheMatrix <- function(x = matrix()) {
	
	solution <- NULL
	setMatrix <- function(y) {
		x <<- y
		solution <<- NULL
	}
	
	getMatrix <- function() x
	setSolution <- function(solver) solution <<- solver
	getSolution <- function() solution

	list(setMatrix = setMatrix, 
		getMatrix = getMatrix,
		setSolution = setSolution,
		getSolution = getSolution)
		
}


## This function solves the matrix passed to it by the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	solution <- x$getSolution()
	if(!is.null(solution)) {
		message("getting cached solution")
		return(solution)
	}
	
	data <- x$getMatrix()
	solution <- solve(data, ...)
	x$setSolution(solution)
	solution
}
		
			
		