## Compute the Inverse of an Invertible Matrix, once it is computed,
## it will be cached for further usage. Assuming the Matrix we pass to the function is Invertible. 

#makeCacheMatrix is a function which returns a list of four following functions:
#1.	set function which set the value of the matrix
#2.	get function which get the value of the matrix
#3.	setInverse function which set the value of the inverse matrix
#4.	getInverse function which get the value of the inverse matrix
#The makeCacheMatrix function returns a list of four functions described above
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) i <<- solve
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#The following function create the inverse of the matrix. 
#However, it first checks to see if the inverse has already been created 
#and cached. If so, it gets the inverse matrix from the cache and skips 
#the computation. Otherwise, it creates the inverse of the matrix and sets 
#the inverse matrix in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cashed data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
