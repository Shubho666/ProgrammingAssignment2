## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains a list of functions for setting and getting the matrix, setting and getting the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set_matrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(cached) inv_matrix <<- cached
        get_inverse <- function() inv_matrix
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {
                message("getting cached inverse matrix")
                return(inv_matrix)
        }
        mat_data <- x$get_matrix()
        inv_matrix<- solve(mat_data, ...)
        x$set_inverse(inv_matrix)
        inv_matrix

}
