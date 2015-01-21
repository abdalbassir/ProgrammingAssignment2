## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # inverse variable represents the inverse of the matrix,
  # that we want to cache  
  inverse <- NULL # set inverse variable to NULL
  
  # set function: set x variable to input variable y and cache it,
  # and set inverse variable to NULL, and cache it
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  # get function: get x variable (the matrix)
  get <- function() x
  
  # setInverse function: set inverse variable to input variable setInv
  # and cache it
  setInverse <- function(setInv) inverse <<- setInv
  
  # getInverse function: get inverse variable (the inverse of the matrix)
  getInverse <- function() inverse
  
  # This is the last line in makeCacheMatrix function, that means what
  # the function returns: a list containing the defined functions:
  # set, get, setInverse, getInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get hidden inverse matrix 
  inverse <- x$getInverse()
  
  # Check if inverse is NULL (e.g. Not yet computed)
  # if it is not NULL, return the hidden inverse matrix, 
  # and print out a message "getting cached data"
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # inverse is NULL, get the hidden matrix using get function
  mdata <- x$get()
  
  # Compute the inverse of the matrix (mdata variable)
  # using R function "solve"
  inverse <- solve(mdata, ...)
  
  # Cashe the inverse of the matrix calling setInverse function
  x$setInverse(inverse)
  
  # Print out the inverse matrix
  inverse
}

# END

