## For expensive computations, caching is a great technique to speed things up. 
## These function will cache the inverse of a matrix then return a cached copy
## to improve performance of that matrix.
## Both functions assume the input matrix is a square matrix that can be 
## inversed. 

## This function will cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  #invalidate the cache by nullifying it in local scope.
  m <- NULL  
  set <- function(y) {
    ## Assign the original matrix to the enclosing environment. 
    x <<- y 
    ## Nullify the cache in the enclosing environment.
    m <<- NULL
  }
  ## Return the original matrix
  get <- function(){
    x
  } 
  
  ## setsolve store the incoming value to the enclosing environment so it is 
  ## store beyond the scope of this method. This makes the value stored available 
  ## even after this method finished running. 
  setsolve <- function(solve){
    m <<- solve
  }
  
  ## Return the stored inverse matrix if m is set. Otherwise return null as initially
  ## set or by user calling set()
  getsolve <- function(){
    m
  } 
  ## return a list of functions just defined using labels the same as the function name
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function will compute the inverse of a matrix if it isn't already 
## cached. If it is cached, it will return the cached version.

cacheSolve <- function(x, ...) {
  ## Retrieve what is stored in x's cache - which can be null.
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Retrieve the original matrix stored in x's cache
  data <- x$get()
  ## calculate the inverse of the original matrix and store it in x's enclosing environment.
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
