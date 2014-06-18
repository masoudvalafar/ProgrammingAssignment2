## Put comments here that give an overall description of what your
## functions do
# The first fuction 'makeCacheMatrix' creates a list
# this object saves the matrix and the inverse of it when it is calculated
# it saves a new matrix when the set function is called and 
# sets the inverse to null
# the get function returns the matrix
# the setInverse and getInverse functions set and return the inverse matrix
# if the inverse is not calculated, it returns null

## Write a short comment describing this function
# variable x contains the matrix
# when the object is first created (by calling set), 
# the 'inverse' variable is set to null
# at the end of the method, the setInverse and getInverse are defined
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(m) inverse <<- m
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# when an object of type cachematrix is created (it is actually a list)
# the inverse is set to null
# when the following method is called, 
# it first check whether the inverse exist or not
# (if inverse is null, it does not exist)
# if it exist it is returned
# if it does not exist, it is calculated and set in the list 
# (by calling setInverse)
# so that next time the inverse should not get calculated gain
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse of the matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
}
