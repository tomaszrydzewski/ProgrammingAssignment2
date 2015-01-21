## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix functions creates a special matrix that caches its inverse 

## cacheSolve is the function we call on our special matrix  - it will return an inverse of the matrix
## if the inverse was already computed it will return the cached value otherwise it will compute it


## Write a short comment describing this function
#In this function we create a special matrix object that can get/set the matrix value and get/set the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the cache to null
  m <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  # Cache the inverse
  setinverse <- function(inverse) m <<- inverse
  # Get the cached inverse
  getinverse <- function() m
  # Return an object with the 4 functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function takes our special matrix, checks if the inverse has already been computed
#if it has it returns the computed inverse otherwise itcomputes it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
        ## If the inverse is not null means we have it cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
        ## If we are here means we haventhad a cached matrix, we need to compute it and cache it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

