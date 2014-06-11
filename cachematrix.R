##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
##This is an empty matrix
i <- matrix()

##Set the value of matrix.
set <- function(y) {
y <- matrix(y, nrow=2, ncol=2)
x <<- y
i <<- matrix()

}

##Get the value of matrix.
get <- function() x
setinverse <- function(x) {
i <<- solve(x)

}
##Get the value of inverse
getinverse <- function() i
list(set = set, get = get, 
setinverse = setinverse,
getinverse = getinverse)


}
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above with 
##the requirements that the code checks if the inverse was already calculated or not.

cacheSolve <- function(x, ...) {
i <- x$getinverse()
##Case when the inverse was already calculated. 
if(!is.null(i)) {
message("Getting cashed data")
return(i)

}
##Case when the inverse wasn't calculated. Calculation will be done.
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}
