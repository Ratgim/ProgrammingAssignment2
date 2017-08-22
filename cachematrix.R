
#Create a List of functions that set& get a matrix and its inverse (which has to be calculated by another function)
makeCacheMatrix <- function(x = matrix()) {
 if (is.matrix(x)==TRUE){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}else
  print("Input is not a matrix")
}

#Calculate the inverse of a matrix stored with makeCacheMatrix if it hasn't been calculated before.

cacheinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}

#Example

Y<-matrix(runif(16),4,4)

Y_list<-makeCacheMatrix(Y)

cacheinv(Y_list)
cacheinv(Y_list)
