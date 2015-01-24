## makeCacheMatrix will return a list of four functions to set/get a matrix and its inverse.
## cacheSolve will solve for the inverse of the matrix given the vector from makeCacheMatrix
## and cache it and return the value. If the inverse is already cached, cacheSolve will simply return it.

## This function creates a special "matrix" object that can cache its inverse.
## Input: a matrix
## Output: Returns a list of functions [get, set, setinv, getinv]
## Usage example: 
## > cm <- makeCacheMatrix(mymatrix)
## > cm$get() # this will return mymatrix
## > cm$set(othermatrix) # this will wipe out inv and change the matrix stored

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
## Input: A list created from makeCacheMatrix(x = matrix())
## Output: the inverse of the CacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinv(inv)
        inv
}
