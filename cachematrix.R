## Put comments here that give an overall description of what your
## functions do

"makeCacheMatrix is a function that generates a matrix object that 
is capable of caching its inverse, while cacheSolve is a function that 
calculates the inverse of the matrix returned by the makeCacheMatrix 
function."

## Write a short comment describing this function

"makeCacheMatrix is a function that generates a matrix object that 
 is capable of caching its inverse. The aforementioned function 
 first intializes two objects, which are the x and matrix_inverse. 
 After initializing these two objects, makeCacheMatrix then 
 defines the set() function, which assigns the input argument 
 to the x object, and the NULL value to the matrix_inverse object. 
 Once the set() function is defined, makeCacheMatrix declares the 
 getter for x, and the setter for matrix_inverse. Afterwards, each 
 element is assigned a name to the function defined above, in the 
 form of a list."

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(inputted_matrix)  {
    x <<- inputted_matrix
    matrix_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) matrix_inverse <<- inverse
  get_inverse <- function() matrix_inverse
  list(set = set, get = get, 
       set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

"cacheSolve is a function that calculates the inverse of the 
 matrix returned by the makeCacheMatrix function, mentioned
 beforehand. The aforementioned function calls the get_inverse()
 function, then verifies whether the value of the input object
 is NULL. If the value of the input object is not NULL, 
 cacheSolve retrieves the matrix returned by the makeCacheMatrix
 function, and computes the inverse of the aforementioned matrix. 
 Afterwards, the inverse of the matrix is set in the input object,
 with the set_inverse() function, and then returned to parent
 environment."

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$get_inverse()
  if(!is.null(matrix_inverse))  {
    message("retrieving cached data")
    return(matrix_inverse)
  }
  retrieve_matrix <- x$get()
  matrix_inverse <- solve(retrieve_matrix, ...)
  x$set_inverse(matrix_inverse)
  matrix_inverse
}

