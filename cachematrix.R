## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    ## MakeCacheMatrix is a function to cache its inverse 

  inv <- NULL     ## firstly, an undentified value (NULL) is assigned to inverse as the default value 
  
  set <- function (y){     ## a function named set is defined to set the matrix
          x <<- y
          inv <<- NULL
    } 
  get <- function() x   ## This function is defined to read the matrix
  setinv <- function(inverse) inv<<- inverse   ## This function assigns the inverse of the matrix to inv
  getinv <- function() inv     ## This function read the value of inv 
  list(set = set, get = get, ## The output of this function is a list that contains 4 functions defined previously 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()     ## read the inverse of the matrix from the list that is the output of the makeCacheMatrix function 
  if(!is.null(inv)) {   ## check if the value of inv is Null or not 
    message("getting cached data") ## if it is not Null then it return the cache value 
    return(inv)
  }
  data <- x$get() ## else if the value of inv is not defined this value computed using solve function
  inv <- solve(data, ...) 
  x$setinv(inv)
  inv
}
