## Here are two functions that are used to create a special object that stores a numeric matrix and caches its inverse.
## An example usage of these functions is as follows:
##   #Create an instance of our special matrix class
##   my_matrix = makeCacheMatrix(matrix(1:4,2,2))
##   # Show the matrix
##   my_matrix$get()
##   #Solve for the inverse of our matrix instance
##   my_matrix_inverse = cacheSolve(my_matrix)
##   #Solve for the inverse again.  In this case, we should get a message that the cached value is being used.
##   my_matrix_inverse = cacheSolve(my_matrix)
## The output matrix for this example should be the 2D matrix equivalent to matrix(c(-2,1,1.5,-0.5),2,2)

## The makeCacheMatrix function creates a special matrix that is a class that includes operations for storing matrix values
## and computing the inverse of the matirx
## It includes methods for the following:
##   set the entries of the matrix
##   get the entries of the matrix
##   set the value of the inverse
##   get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    matrix_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(new_matrix_inverse) matrix_inverse <<- new_matrix_inverse
  get_inverse <- function() matrix_inverse
  # Return the methods of this class
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the set_inverse function.
cacheSolve <- function(x, ...) {
  ## Check whether the matrix has already been computed and cached.
  matrix_inverse <- x$get_inverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  ## Otherwise, compute the inverse, store it in the special matrix, and return it.
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$set_inverse(matrix_inverse)
  matrix_inverse
}
