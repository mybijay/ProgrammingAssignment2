##This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ##variable to hold the inverse of the metric
  set <- function(y) {
    x <<- y ## cache  matrix provided in input y as x
    inv <<- NULL ## reset inv
  }
  
  get <- function() x ## return matrix
  setinverse <- function(newInv) inv <<- newInv ## allow to store user provided inverse as inv
  getinverse <- function() inv ## return inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function calculates the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixData <- x$get() ## retrieve the data
  inv <- solve(matrixData) ## solve function returns the inverse of input matrix(built-in function)
  x$setinverse(inv) ## cache it
  inv
  
}



##Test Script
testMatrix<-matrix(rnorm(40000,200,200),ncol=4,nrow=4)
newMatrix<-makeCacheMatrix(testMatrix)
cacheSolve(newMatrix)



start.time <- Sys.time()
  b2<-solve(testMatrix)
  b2
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
b3<-newMatrix$getinverse()
b3
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


