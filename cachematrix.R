#######################################################################
## Student Name : Reginald Carey
## Course : Coursera - Data Specialization : R Programming
## School : Johns Hopkins University
## Date : January 20, 2014
## Assignment : #2 - Peer Review Assignment
#######################################################################

## The two functions, makeCacheMatrix() and cacheSolve() work together
## to provide a cached inverted matrix.  The makeCacheMatrix() is used
## to hold the actual source matrix and some computed result (such as
## the matrix inverse).  It is assumed that changing the input via a
## set() call on makeCacheMatrix will invalidate the computed result.


## makeCacheMatrix - create a cacheable version of a matrix.  Provide
## traditional setters and getters for the base object to which
## compuations are applied.  Also provide setters and getters for the
## computed result.  This function utilizes the local environment
## to store matrices.  It is possible to pass in a storage matrix
## by providing a parameter when calling makeCacheMatrix.  However, one
## must be careful to ensure that the passed in object is in an
## accessible environment when the setters & getters for it are
## called.
##
## INPUT: an optional matrix
## RETURNS: a list of closures (functions)

makeCacheMatrix <- function(x = matrix()) {
  computed <- NULL
  set <- function(m) { x <<- m; computed <<- NULL }
  get <- function() { x }
  setComputed <- function(i) { computed <<- i }
  getComputed <- function() { computed }
  list(get = get, set = set, getComputed = getComputed, setComputed = setComputed)
}


## cacheSolve - compute the inverse of a matrix and cache the results.  This
## method assumes that the first parameter is a list of methods encapsulating
## a matrix and the result of some computation.  This function is fault tolerant
## in that a regular matrix can be passed as the first argument.  In that case
## the inverse is computed over the passed in "normal" matrix and a warning
## is generated.  This function works intimately with the output of the
## makeCacheMatrix function - which provides the underlying cache framework.
##
## INPUT: a cacheableMatrix (see makeCacheMatrix())
##        ... any additional parameters to the solve() method
## RETURN: a matrix which is an inverse of the input - if it is invertable.

cacheSolve <- function(x, ...) {
  cacheable = x
  
  if (typeof(x) != "list") {
    warning("Expected a 'special' matrix created with makeCacheMatrix(). Computed matrix inverse without caching.")
    # convert original x into a cacheable matrix x. This keeps the subsequent code simple.
    cacheable <- makeCacheMatrix(x)
  }
  
  inv <- cacheable$getComputed()
  if (is.null(inv)) {
    inv <- solve(cacheable$get(), ...)
    cacheable$setComputed(inv)
  }
  inv
}
