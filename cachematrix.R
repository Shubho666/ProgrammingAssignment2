## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function


## makeCacheMatrix contains a list of functions for setting and getting the matrix, setting and getting the inverse
## set_matrix gets the matrix and stores it in x.... it also resets the previous inverse (if calculated) to null
## get_matrix shows the matrix
## set_inverse stores the inverse calculated in inv_matrix
## get_inverse retrives the inverse stored (if not stored/calculated before then the value of inv_matrix is null)
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

## returns the inverse of a invertible matrix or the cached matrix if we call this function for the same input matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()  ## we get the inverse, if not calculated earlier then the value is null
        if(!is.null(inv_matrix)) {
                message("getting cached inverse matrix")
                return(inv_matrix)  ## if repeating the call then prints an appropriate message and the inversed matrix
        }
        mat_data <- x$get_matrix()  ## if new matrix has been set then inverse is again calculated ,stored  and returned
        inv_matrix<- solve(mat_data, ...)
        x$set_inverse(inv_matrix)
        inv_matrix

}
