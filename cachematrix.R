## Functions that provides a wrapper of a numeric matrix
## and provides ability to cache the inverse of matrix
## Inverse computation is CPU intensive task and hence caching
## it makes a lot of sense

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## To store the inverse matrix
  
  ## Setter of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Getter of the matrix
  get <- function() x
  
  ## Setter/Getter of the Inverse matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
  )
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## If inverse exist, return
  ## Else compute and store it in cache
  i <- x$getinverse();
  if (!is.null(i)) {
    message("getting inverse matrix from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}