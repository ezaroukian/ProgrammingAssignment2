## Together, these functions allow matrix inverses to be cached and retrieved. 
## A matrix generated with makeCacheMatrix whose inverse is calculated with cacheSolve
## will cache its inverse, which can later be retreived through cacheSolve.


#Generates a inverse-cacheable matrix in the form of a list of matrix get/set and inverse get/set functions
makeCacheMatrix <- function(x = matrix()) {
  #inverse is initially NULL
  i <- NULL
  #set() makes a global version of the matrix and a global NULL inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get() returns the global matrix created by set
  get <- function() x
  #setinver() takes the inverse and stores it globally
  setinver <- function(inv) i <<- inv
  #getinver() retrieves the global inverse
  getinver <- function() i
  #returns list of functions to get/set actual matrix and its inverse
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


# Takes a SQUARE matrix created by makeCacheMatrix (i.e. a list of get/set functions), 
# returns its globally-cached inverse (as a list of get/set functions), if necessary first computing and caching the inverse
cacheSolve <- function(x, ...) {
  #gets the globally-cached inverse
  i <- x$getinver()
  #if inverse has previously been computed, returns cached inverse matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #otherwise gets acutal matrix, computes inverse, stores inverse globally
  data <- x$get()
  i <- solve(data, ...)
  x$setinver(i)
  return(i)
}
