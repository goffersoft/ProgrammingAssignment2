## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly
## the following pair of functions cache the inverse of a matrix.
##
## 1) makeCacheMatrix <- returns a decorated list that would allow
##                       for caching the inverse of  amatrix.
##                       Returns a list of functions 
##                         set -> function to set a sq. matrix(input)
##                                and clear the cached value
##                         get -> function to get the sq. matrix
##                         getinv -> function to get the inv of the matrix
##                         setnv -> function to set the inverse of the matrix
## 1) cacheSolve <- return value is passed to this functions.
##                  This function computes the inverse of the matrix 
##                  only once and returns the cached value on subsequent calls.


## makeCacheMatrix <- returns a decorated list that would allow
##                    for caching the inverse of  amatrix.
##                    Returns a list of functions 
##                      set -> function to set a sq. matrix(input)
##                             and clear the cached value
##                      get -> function to get the sq. matrix
##                      getinv -> function to get the inv of the matrix
##                      setinv -> function to set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x))
    stop("x must be a matrix")

  if(length(dim(x)) != 2 || dim(x)[1] != dim(x)[2])
    stop("x must be a square matrix")

  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(invmat) inv <<- invmat

  list(get = get, set = set,
       getinv = getinv,
       setinv = setinv)
}

## cacheSolve <- x is the reutrn value of the makeCacheMatrix function.
##               This function computes the inverse of the matrix 
##               only once and returns the cached value on subsequent calls.
cacheSolve <- function(x, ...) {
  if(!is.list(x))
    stop("x must be a list")
  
  attrnames = names(x)
  
  if((sum(attrnames == c('get', 'set', 
                        'getinv', 'setinv'))) != 4) {
    stop('make sure x is a list returned from the makeCacheMatrix function')
  }
  
  invmat <- x$getinv()

  if(!is.null(invmat)) {
    message('using cached value of inv matrix')
    return(invmat)
  }
  
  mat <- x$get()

  if(!is.matrix(mat))
    stop("x must be a matrix")
  
  if(length(dim(mat)) != 2 || (dim(mat)[1] != dim(mat)[2]))
    stop("input data must be a square matrix")

  invmat <- solve(mat, ...)
  x$setinv(invmat)
  invmat
}
