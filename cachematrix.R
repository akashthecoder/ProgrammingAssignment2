## The below functions facilitate to save the computation time by acaching the result
## if the result has already been computed then its just retrieved


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

   a = NULL
  
   # Set function to set the cache value
   set = function(b) {
     
     x <<- b
     a <<- NULL
   }
   # get function to get the cache value
   get <- function() x
   
   # set function to set the inverse of matrix
   setinverse <- function(inverse) a <<- inverse
   
   #get function to get teh inverse of matrix
   getinverse <- function() a
   
   #list to create a matrix object
   list(set=set, get=get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}


## Computes the inverse. If, the inverse has already been calculated and not chnaged 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # check if the matrix is null if not then return null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  
  #compute inverse and set the value
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## below commands will run the code and testify that it works fine.

## M = matrix(1:4,ncol=2,nrow=2)
## testm = makeCacheMatrix(M)
## cacheSolve(testm)
## testm$get()
## testm$getinverse()
## cacheSolve(testm)
##
#