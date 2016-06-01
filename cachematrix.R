## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes matrix as in input

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL # sets the value to null on first call
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
          get <- function() x
          setInverse <- function(inverse) inv <<- inverse   #cal inverse    
          getInverse <- function() inv  #return inverse
          list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function
##this function takes input as the matrix and calculates the inverse and stores into the inv variable first time
# if again the function is called, the cached variable is used to prodcue the output

cacheSolve <- function(x, ...) {
#checks whether the inverse hasbeen already saved or not
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Fetching already cached data")
        return(inv)
  }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
