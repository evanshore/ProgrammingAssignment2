## We want to create and use a cached inverted matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
     
     ## Make sure a matrix is entered
     if(!is.matrix(matrix)) {
          stop("That's not a matrix. Please enter a matrix")
     }
     
     ## Create object inverted.matrix
     inverted.matrix <- NULL

     ## Create functions to set/get both the matrix and inverse
     set.matrix <- function(y) {
          matrix <<- y
          inverted.matrix <<- NULL
     }
     get.matrix <- function() matrix
     set.inverse <- function(solve) inverted.matrix <<- solve
     get.inverse <- function() inverted.matrix

     list(
          set = set, 
          get = get, 
          setmean = setmean,
          getmean = getmean
          )
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(user.matrix, ...) {

     ## Define inverted matrix object using get.inverse     
     inverted.matrix <- user.matrix$get.inverse()
     
     ## If it's there, return the inverted matrix
     if(!is.null(inverted.matrix)) {
          message("Getting cached inverse matrix")
          return(inverted.matrix)
          }
     
     ## If it's not there, calculate it
     inverse.this <- user.matrix$get()
     inverted.matrix <- solve(inverse.this, ...)
     user.matrix$set.inverse(inverted.matrix)
     inverted.matrix
}