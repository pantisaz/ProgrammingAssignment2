## This first function creates a cache matrix
## A cache matrix stores data in a serparate environment so that future requests for that data can be served faster
## To make a cache matrix we use the <<- operator to allocate different addresses in memory for the s and X variables

makeCacheMatrix <- function(X = matrix()) {
  s <- NULL
  set <- function(Y) {
    X <<- Y
    s <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function finds the inverse of the matrix created in the first function
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache with the setinverse function

cacheSolve <- function(X=matrix(), ...) {
  s <- X$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- X$get()
  s <- solve(matrix, ...)
  X$setinverse(s)
  s                      ## Returns a matrix that is the inverse of 'X'
}  
