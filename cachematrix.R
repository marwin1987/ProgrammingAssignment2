#instruction 1: making the makeCacheMatrix
#the makeCacheMatrix computes a matrix that can cache its inverse 
#Meaning the makeCacheMatrix computes a special matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#instruction 2: creating a new function called makeCacheMatrix
#in cacheSolve the inverse of the makeCacheMatrix is done

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#References helpfiles programming the script:
#http://support.sas.com/documentation/cdl/en/imlug/59656/HTML/default/viewer.htm#langref_sect144.htm --> understanding the INV function even better
#the example made by R. D. Peng himself:  https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md
#using the comments: https://github.com/rdpeng/ProgrammingAssignment2/commit/e4eed4153bd237a2dd4532fc3f40ce0e22fd0cb7
#the solve() function: https://stat.ethz.ch/R-manual/R-devel/library/base/html/solve.html
#more solve() http://www.endmemo.com/program/R/solve.php
