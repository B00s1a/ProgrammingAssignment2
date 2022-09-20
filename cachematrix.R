# function to create the cached matrix object 
makeCacheMatrix <- function(x = matrix()) {
  cachedInversedMatrix <- NULL   # setting the cached inversed matrix to the NULL by default
  set <- function(newMatrix) {   # function to set the new matrix if needed
    x <<- newMatrix              
    cachedInversedMatrix <<- NULL  # when new matrix is set, the cache should be reset as well
  }
  get <- function() x             # function to get the matrix 
  setInversed <- function(inverseOfX) cachedInversedMatrix <<- inverseOfX 
                                  # function to write the inversed matrix into the cache
  getInversed <- function() cachedInversedMatrix
                                  # function to retrieve the inversed matrix from the cache
  list(set = set, get = get,
       setInversed = setInversed,
       getInversed = getInversed) # return the list of functions above 
}

# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  cachedInversedMatrix <- x$getInversed()   # get cached inversed matrix
  if(!is.null(cachedInversedMatrix)) {      # if was calculated before
    message("getting cached data")          # then return the cache
    return(cachedInversedMatrix)
  }
  matrixToInverse <- x$get()                # else, get the original matrix 
  cachedInversedMatrix <- solve(matrixToInverse, ...)   # inverse the matrix
  x$setInversed(cachedInversedMatrix)       # write the result into cache
  cachedInversedMatrix                      # return cache
}

# Test
# a <- makeCacheMatrix(x = matrix(c(2,0,0,0,2,0,0,0,2), 3, 3))
# cacheSolve(a)
# cacheSolve(a)
# 
# b <- makeCacheMatrix(x = matrix(c(3,0,0,0,3,0,0,0,3), 3, 3))
# cacheSolve(b)
# cacheSolve(b)

