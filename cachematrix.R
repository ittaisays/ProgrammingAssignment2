#Makes an object to feed into cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL #clears the cache, since a new matrix was set
  }
  #get retrieves the matrix
  get <- function() x
  #to put the solved matrix in the cache, solve is assigned to m
  setInverse <- function(solve) m <<- solve
  #to fetch the cached matrix
  getInverse <- function() m
  #returns the list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # in this function, "x" is a makeCacheMatrix object, not a matrix
  m <- x$getInverse() #assigns cached value into this function
  if(!is.null(m)) { #if m isn't null, that means we have data in the cache
    message("getting cached data")
    return(m) #return m so the rest of the code doesn't run
  }
  data <- x$get() #assigns the matrix to a variable in this function
  m <- solve(data, ...) #solves the matrix to get its inverse
  x$setInverse(m) #sets the inverse into the cache
  m #prints the cache, which now contains the solved matrix
}
