## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix for the input
makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL # sets the value to null on first call
         set <- function(y) {
          x <<- y
         inve <<- NULL
  }
          get <- function() x
          setInverse <- function(inverse) inve <<- inverse   #cal inverse    
          getInverse <- function() inve #return inverse
          list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function
##this function takes input as the matrix and calculates the inverse and stores into the inv variable first time
# if again the function is called, the cached variable is used to produce the output. Checking the null values in the
## in the variable tells us whether inverse has been computed before or not. This is how caching is involved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #checks whether the inverse has been already saved or not
        inve <- x$getInverse()
        if (!is.null(inve)) {
                message("Fetching already cached data")
        return(inve)
  }
        mat <- x$get()
        inve <- solve(mat, ...)
        x$setInverse(inve)
        inve
}
