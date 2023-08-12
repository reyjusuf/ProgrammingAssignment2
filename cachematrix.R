#This program creates a couple of function that can cache the inverse of a matrix once it's calculated
#to save time when the user wants to calculate the inverse of the matrix again.

#It does so by creating a special matrix object that stores the value of the matrix and its inverse (the makeCacheMatrix function).
#When the cacheSolve function is called, it will check whether the inverse of that particular matrix had been calculated before.
#If it has, it will take the inverse value that had been stored in the cache.

#----------------------------------------------------------
#makeCacheMatrix: This function creates a matrix object that caches its Inverse.
#It returns a list that can set and get the value of the matrix as well as set and get its Inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#----------------------------------------------------------
#cacheSolve: This function returns the inverse of the matrix created by makeCacheMatrix.
#If the inverse has already been calculated, the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
