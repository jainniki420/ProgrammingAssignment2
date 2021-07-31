#A pair of function that cache the inverse of a matrix.
#This function creates a special "matrix" object that can cache its inverse. 

#makeCacheMatrix consists of set, get, setInverse, getInverse
makeCacheMatrix <- function(x= matrix()) {
  inv <- NULL    #initializing inverse as NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x    #function to get matrix x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv 
  list(set= set, get= get, setInverse= setInverse, getInverse= getInverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {    #gets Cache data
#Return a matrix that is the inverse of 'x'.
  inv <- x$getInverse()
  if(!is.null(inv)) {    #checking whether inverse is NULL
    message("getting cached data")
    return(inv)    #returns inverse value
  }
  data <- x$get()
  inv <- solve (data, ...)    #calculates inverse value
  x$setInverse (inv)
  inv    #return a matrix that is the inverse of 'x'.
}
