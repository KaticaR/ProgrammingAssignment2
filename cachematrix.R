## makeCacheMatrix and cacheSolve are functions that save time 
## There main goal is to calculate inverse matrix of a square matrix x
## And if the inverse matrix already exists in the cache memory it 
## will be taken from there, if not, inverse will be calculated and 
## remembered in the cache.In both cases inverse matrix is returned.

## makeCacheMatrix is a function that links matrix x with a special 
## list of 4 functions: set, get, setinv and getinv. 
## Set and setinv use special operator <<- to assign values to x and 
## inv in cache memory. Get and getinv are getting x and inv from cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is a function that takes a special list from makeCacheMatrix
## as argument, and checks if inverse matrix of x is already calculated 
## and stored in cache and gets it if it is, if not, then it gets calculated
## with solve function end gets stored in cache. In both cases invers matrix
## is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
