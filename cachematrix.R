## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initialize
  inv <- NULL 
  
  #1. set the matrix/function
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  #2. get the matrix
  get <- function()x
  #3. set inverse
  setinv <- function(inverse) inv <<-inverse
  #4. get inverse
  getinv <- function() inv
  #5. list
  list(set = set, get= get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #1. check if inv was previously computed and return if that's the case
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    #2. if inv hasn't been computed, get the matrix
    data <- x$get()
    #3. calculate the inverse
    inv <- solve(data,...)
    #4. cache the result
    x$setinv(inv)
    #5. return result of inv
    inv
    
}
