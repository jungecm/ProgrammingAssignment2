## This pair of functions works in conjuction to create an object with set and get methods
##   for a matrix and its inverse.  
## The inverse is cached so that it does not need to be recalculated, and the cached 
##   inverse is cleared if the matrix is changed using the object's set methods.  


## This function creates a special "matrix" object that can cache its inverse.
##   (This description is copied from the assignment)

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     ## The starting value of the inverse is NULL
     
     set <- function(y) {
          x <<- y
          inv <<- NULL
          ## When the matrix is changed, the cached inverse is cleared
     }
     
     get <- function() x
     ## Returns the matrix itself
     
     setInverse <- function(inverse) inv <<- inverse
     ## Changes the cached inverse
     
     getInverse <- function() inv
     ## Returns the cached inverse
     
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
     ## Returns the "matrix object"
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##   above. If the inverse has already been calculated (and the matrix has not changed), 
##   then cacheSolve should retrieve the inverse from the cache.
##   (This description is copied from the assignment)

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     ## Looks up the value of the cached inverse
     
     if(!is.null(inv)) {
          ## If there is a cached inverse, it is returned and the function exits
          message("getting cached data")
          return(inv)
     }
     
     ## If there is no cached inverse, the following executes:
     
     data <- x$get()
     ## Looks up the matrix itself
     
     inv <- solve(data, ...)
     ## Finds the inverse using the solve() function
     
     x$setInverse(inv)
     ## Sets the cached Inverse using the "matrix object"'s setInverse method
     
     inv
     ## Returns the newly computed inverse
}
