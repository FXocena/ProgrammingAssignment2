## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set/get value of matrix
## set/get value of inversed matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		if(length(dim(y))!=2) {
			message("Not a matrix!!!")
			return
		} else if(diff(dim(y))!=0){
			message("Not a square matrix!!!")
			return			
		} else {
			x <<- y
			inv <<- NULL
		}
	}
	get <- function() x
	setinv <- function(inverse) m <<- inverse
	getinv <- function() inv
	list(set = set, get = get, 
		setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached matrix")
		return(inv)
	}
	xMatrix <- x$get()
	inv <- tryCatch(solve(xMatrix, ...), error = function(e) 0)
	x$setinv(inv)
	inv
}
