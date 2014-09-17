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

cacheSolve <- function(X=matrix(), ...) {
  s <- X$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- X$get()
  s <- solve(matrix, ...)
  X$setinverse(s)
  s
}