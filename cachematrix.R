## Put comments here that give an overall description of what your
## functions do

## creates a special matrix object that can save the cache
makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function(x) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## solves for inverse with a check-cashe-first policy
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inv")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}