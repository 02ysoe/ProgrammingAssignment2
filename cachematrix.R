## In this assignment, the point is to construct a function that caches the value of the inverse of a matrix,
## so that you dont have to compute it over again the next time you need it. Instead, the cached value (the
## inverse of a matrix) can be retrieved in other functions, when needed, instead of recomputing.

##### A function to create a special "matrix" object that can cache its inverse #####
## Similar to the example in the assignment instructions on caching the mean of a vector, the below function
## creates a 'special' matrix object to cache its inverse. Similar to the provided example, the function
## first sets the value of the matrix ('set'), gets the value of the matrix ('get'), sets the value of the inverse
## of the matrix ('setinverse') and gets the value of the inverse of the matrix ('getinverse').

makeCacheMatrix <- function(x = matrix()) {
  cache_inv <- NULL
  set <- function(y) {
    x <<- y
    cache_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache_inv <<- inverse
  getinverse <- function() cache_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##### A function to compute the inverse of the special "matrix" returned by makeCacheMatrix above #####
## The CacheSolve functions first checks whether the inverse of the matrix has already been calculated and cached
## and then goes on to retrieve the inverse from the cache, instead of recalculating. If the inverse have not yet
## been calculated/cached, the 'solve(data,...)' calculates the matrix inverse and caches the value via 
## 'x$setinverse(cache_inv)' for future use.

cacheSolve <- function(x, ...) {
  cache_inv <- x$getinverse()
  if (!is.null(cache_inv)) {
    message('getting cached data')
    return(cache_inv)
  }
  data <- x$get()
  cache_inv <- solve(data,...)
  x$setinverse(cache_inv)
  cache_inv
}
