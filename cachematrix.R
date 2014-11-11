## This function will return a list containg functions to
## set and get the value of the matrix, and set and get the solve value
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(cache) m <<- cache
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
  
}


## The following function checks for a cached value and if present returns
## the cached value.  If the cached value is null, the solve value is
## calculated and is cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}


