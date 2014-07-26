makeCacheMatrix <- function(x = matrix()) {
  # cache for inverse matrix
  inv <- NULL

  # getter & setter methods for matrix
  set <- function(y){
    # set new matrix
    x <<- y

    # reset cache
    inv <<- NULL
  }
  get <- function() x

  # getter & setter methods for inverse matrix
  setInverseMatrix <- function(inverseMatrix) inv <<- inverseMatrix
  getInverseMatrix <- function() inv

  list(
    set=set,
    get=get,
    setInverseMatrix=setInverseMatrix,
    getInverseMatrix=getInverseMatrix
    )

}



cacheSolve <- function(x, ...) {
  # get current cache value
  inv <- x$getInverseMatrix()

  # return cache value if already calculated
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }

  # solve inverse matrix
  data <- x$get()
  inverseMatrix <- solve(data)

  # set cache
  x$setInverseMatrix(inverseMatrix)

  # return result
  inverseMatrix
}
