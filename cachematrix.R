makeCacheMatrix <- function(v_matrix = matrix()) {

	## Description: defines the functions gets/sets to get the value of the matrices
	## Arguments: Takes a matrix.
	## Return Value: Returns a list of functions available.
	## Function get(): returns the value of the current matrix
	## Function set(): sets the value of the matrix  the value of the current matrix
	## Function getInverse(): returns the value of the inversed value of the current matrix
	## Function setInverse(): sets the inverse value of the matrix
	## Function getPrev(): returns the previous value of the Matrix. 
    

	g_matrix <<- v_matrix

	## get/set the value of the v_matrix from/in the global environment

	get  <-function () g_matrix
	set  <-function ( m ) {
		
		g_inverse <<- NULL          ## set the inverse values to null
		g_matrix <<- m	            ## set the new value of the global matrix
	}
	
	setInverse <- function(m) g_inverse <<- m
	getInverse  <-function() g_inverse
	getPrev  <-function() prev_matrix

	## return a vector of get/set functions available
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, getPrev = getPrev)
}

cacheSolve <- function(cacheMatrixFunc, ...) {
	## Description : Calculates the inverse of the matrix using the solve() function.
	## Returns: inverse value of the Matrix


	## get the existing Inversed value.

	inv <- cacheMatrixFunc$getInverse() 

	## check to see  - 
	## IF 
	## 1. the matrix has changed, if it has changed then recompute the inverse and store the matrix to the variable prev_matrix and calculate inverse
	## 2. the inverse is not yet computed i.e g_inverse is null then calculate the inverse

	if ( is.null(inv) | !identical(prev_matrix , g_matrix) )  {
		print ("recalculating ...")
		
		prev_matrix <<- g_matrix	## make a copy of the matrix being inversed and then compute its inverse
		
		cacheMatrixFunc$setInverse(solve(g_matrix))    	#calculate the inverse and store it to the global variable
		inv <- cacheMatrixFunc$getInverse() 		## get the newly created inverse
	}

	inv
}
