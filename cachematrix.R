## These 2 functions implement a more efficient cached version of the solve()
## function (at least for inverting square invertible matricies)

## makeCacheMatrix is essentially an object builder.
## It takes a square invertible matrix and returns a 'cacheMatrix' object with the following methods (invoked with $ notation)
##   get(): takes no arguments and returns the stored matrix
##   set(): takes a matrix and stores it in the object. It also sets the inverseMatrix data to NA/UNDEFINED
##   getInverse(): takes no arguments and returns the inverted matrix stored on the object.
##                 *Note this is not the preferred method to retrieve this information as it will return NULL is not defined
##                  The correct way to retrieve the inverse of the target matrix is to call cacheSolve() described below
##   setInverse(): Takes a matrix ande stores it on the object as the inverted matrix.
##                 *Note this method is invoked by cacheSolve() and should not be run individually as there is no validation
##                  of the matrix which would normally be calculated in cacheSolve()
##
##
makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL #inverseMatrixData storage variable
  set <- function(y) {
    x <<- y 
    iMatrix <<- NULL #nullify the inverseMAtrixData in order to force a solve() execution next time since something changed
  }
  get <- function() x
  setInverse <- function(inverseMatrix) iMatrix <<- inverseMatrix 
  getInverse <- function() iMatrix
  list(set = set, get = get, #return the object, i.e. collection of methods to be accessed
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is an efficient version of the solve() function by using the object created using makeCacheMatrix()
## It takes a 'makeCache' object (created using makeCacheMatrix()) and retrieves the inverse matrix data contained there.
## If the data is undefined (either it was never set or the target matrix has changed), it will the execute solve()
## on the matrix. It then stores the result on the object and returns the inverse data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getInverse()
  if(!is.null(iMatrix)){ #if the inverseMatrixData off the object is defined, then return that without running solve()
    message("using cached data") #so fast!
    return(iMatrix)
  }
  #else, the inverseMatrixData is undefined so we have to actually run solve() on the matrix
  data <- x$get()
  iMatrix <- solve(data) #tick tock....tick tock....
  x$setInverse(iMatrix) #cache the result for use next time!
  iMatrix
}
