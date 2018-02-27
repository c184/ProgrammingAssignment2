# The idea behind these two functions is that
# you don't need to compute repeatedly the inverse
# of a matrix if you have already done that.
# This helps you to speed up your programs.
# I remenber you that the <<- operator can be used
# to assign a value to an object in an environment
# that is different from the current environment.

# This first function creates a list (NOT a matrix)
# containing 4 function to:
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                      
  set <- function(y) {             
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This second funtion first checks to see if the inverse has already been computed.
# If so, it gets it from the cache using the getinverse function.
# Otherwise, it computes the inverse of the matrix and sets the value
# of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
