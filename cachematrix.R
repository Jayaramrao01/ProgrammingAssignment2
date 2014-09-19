# Assignment - 2
#----------------
# Caching the inverse of matrix, to avoid repeated computation
#-------------------------------------------------------------

# List is created with function to set, get, set inverse value and get inverse value
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  
  get <- function() a
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# function to return the inverse of a matrix
cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  if(!is.null(inv)) {
    print("getting cached data.")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setinverse(inv)
  inv
}
