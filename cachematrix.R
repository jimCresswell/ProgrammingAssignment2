#' Functions, makeCacheMatrix and cacheSolve, to calculate inverted matrices
#' using caching to speed up the Calculation where possible.
#' 
#' @example Example use:
#' m <- makeCacheMatrix()
#' m$set(matrix(c(2, 0, 0 , 2), 2, 2))
#' m$get()
#' cacheSolve(m) ## Calculated result.
#'      [,1] [,2]
#' [1,]  0.5  0.0
#' [2,]  0.0  0.5
#' cacheSolve(m) ## Cached result.
#' getting cached data
#'      [,1] [,2]
#' [1,]  0.5  0.0
#' [2,]  0.0  0.5
#' m$set(matrix(c(3, 0, 0 , 3), 2, 2)) ## Cache reset.
#' cacheSolve(m)
#'           [,1]      [,2]
#' [1,] 0.3333333 0.0000000
#' [2,] 0.0000000 0.3333333


#' Cache object for invertible matrices and their inverses.
#'
#' @param original.matrix the original, invertible matrix.
#'
#' @return An interface (list) of getter and setter functions for the original and inverted matrices.
makeCacheMatrix <- function(original.matrix = matrix()) {
  
  # Cached invered matrix.
  inverted.matrix <- NULL
  
  set <- function(new.original.matrix) {
    original.matrix <<- new.original.matrix
    inverted.matrix <<- NULL # Reset the cached inverse on a change of the original matrix.
  }
  get <- function() {
    original.matrix
  }
  
  set.inverted <- function(new.inverted.matrix) {
    inverted.matrix <<- new.inverted.matrix
  }
  get.inverted <- function() {
    inverted.matrix
  }
  
  # Expose the interface as a list.
  list(
    set = set,
    get = get,
    set.inverted = set.inverted,
    get.inverted = get.inverted
  )
}


#' Calculate the inverse of an invertible matrix.
#' Uses a cached value where possible, else
#' caches the calculated value for future use.
#'
#' @param matrix.cache 
#' @param ... Pass through arguments.
#'
#' @return The calculated or cached inverted matrix.
cacheSolve <- function(matrix.cache, ...) {

  # Check for cached matrix inverse, return early if found.
  inverted.matrix <- matrix.cache$get.inverted()
  if (!is.null(inverted.matrix)) {
    message("getting cached data")
    return(inverted.matrix)
  }
  
  # No cache found; calculate, cache and return inverse.
  original.matrix <- matrix.cache$get()
  inverted.matrix <- solve(original.matrix, ...)
  matrix.cache$set.inverted(inverted.matrix)
  inverted.matrix
}
