##this function makes a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##this function calculates the inverse of the matrix above. If the inverse has already been calculated, it returns the cached inverse. 
cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
     
}