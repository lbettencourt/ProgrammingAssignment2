## Prog Assign #2 -- Laura Bettencourt
## Shout out to Bill Hilton in the Discussion Forum for
## his most excellent explanation of the assignment examples
## 
## For this assignment:  assume that the matrix supplied is always invertible

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## set m to null for every iteration
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## TESTING
## testmatrix <- matrix(data=c(1,1,4,0,3,1,4,4,0), nrow=3, ncol=3, byrow=FALSE, dimnames=NULL)

## bigmax<-makeCacheMatrix(testmatrix)
## bigmax$get()

## cacheSolve(bigmax)  #submit twice to see cache

## redefine matrix

## testmatrix <- matrix(data=c(1,1,6,0,3,1,6,4,0,7,8,9,1,0,5,6), nrow=4, ncol=4, byrow=FALSE, dimnames=NULL)
## bigmax<-makeCacheMatrix(testmatrix)
## bigmax$get()
## cacheSolve(bigmax)  #submit twice to see cache

