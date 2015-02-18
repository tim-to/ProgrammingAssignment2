## For expensive computations, caching is a great technique to speed things up. 
## These function will cache the inverse of a matrix then return a cached copy
## to improve performance of that matrix.
## Both functions assume the input matrix is a square matrix that can be 
## inversed. 

## This function will cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  #invalidate the cache by nullifying it both in local and parent's scopes.
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  ## setsolve store the incoming value to the enclosing environment so it is 
  ## store beyond the scope of this method. This makes the value stored available 
  ## even after this method finished running. 
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function will compute the inverse of a matrix if it isn't already 
## cached. If it is cached, it will return the cached version.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## caculate the inverse of the input matrix and store it in x's enclosing environment.
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
