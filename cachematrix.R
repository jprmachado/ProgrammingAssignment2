## Matrix Inversion in R


## Creates a matrix and put in cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function computes the inverse matrix returned by makeCacheMatrix() if the matrix is not been calculed, otherwhise
## return the cached matrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("cached matrix")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)

  x$setinv(inv)
  
  return(inv)
}
