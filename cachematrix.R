## The makeCacheMatrix function returns a list holding your matrix, and a getter and setter 'method'
## It holds the necessary data in a variable within its scope (analogous to an instance variable)
## The cacheSolve function grabs the cached variable from makeCacheMatrix, or computes and stores the inverse

## Create a cachable matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solve a matrix and store its inverse in the cache, or get the cached version

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If m already exists (via the getinverse 'method'), get the cached matrix
  if(!is.null(m)) {
    ## Print a message, to 'prove'your getting the cached version
    message("getting cached data")
    return(m)
  }
  ## if not (you didn't return early), solve the matrix
  data <- x$get()
  m <- solve(data, ...)
  ## And store it in the cachable
  x$setinverse(m)
  m
}
