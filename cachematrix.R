##These fuctions will cache the inverse of a matrix and return it IF the matrix has not changed.

## This function will be used to cache the matrix and inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
              inverse_x <- NULL
              set <- function(y) {
                print("setting up!")
                x <<- y
                inverse_x <<- NULL
              }
              get <- function() x
              set_inverse <- function(inverse_matrix) inverse_x <<- inverse_matrix
              get_inverse <- function() inverse_x
              list(set = set, get = get,
                   set_inverse = set_inverse,
                   get_inverse = get_inverse)
}


## This function will be used to calculate the inverse of the matrix if its not been cached yet.
## If the inverse has already been cacehed AND if matrix has not changed since, then
## the function simply returns the cached inverse.
cacheSolve <- function(x, ...) {
        y <- makeCacheMatrix(x)  
        cached_x <- y$get()
        cached_inverse <- y$get_inverse()
        if(!is.null(cached_inverse) && matequal(x,cached_x)){
          print("Getting cached inverse")
          cached_inverse <- y$get_inverse()
          return(cached_inverse)
        }
        matrix_x <- y$get()
        inverse_x <- solve(matrix_x)
        y$set_inverse(inverse_x)
        inverse_x
}

matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)