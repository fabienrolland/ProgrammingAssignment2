## Coursera R Programming week 3 assignment : caching the inverse of a matrix

## 	This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 		## define the argument with default mode of "matrix"
  inv <- NULL 										              ## initialize inv as NULL; will hold value of matrix inverse
  set <- function(y) {								          ## define the set function to assign new
    x <<- y											                ## value of matrix in parent environment
    inv <<- NULL									              ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x								            ## define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse	## assigns value of inv in parent environment
  getinverse <- function() inv						      ## gets the value of inv where called
  list(set = set, get = get,						        ## you need this in order to refer to the functions with the $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {					      ## get the value of the invertible matrix from the makeCacheMatrix function
  inv <- x$getinverse()			
  if(!is.null(inv)) {								            ## if inverse matrix is not NULL
    message("getting cached data")					    ## Type message: Getting Cached Invertible Matrix 
    return(inv)
  }
  data <- x$get()									              ## get the original Matrix Data 
  inv <- solve(data, ...)							          ## use solve function to inverse the matrix
  x$setinverse(inv)									            ## set the invertible matrix 
  inv												                    ## return the invertible matrix
}													                      ## Return a matrix that is the inverse of 'x'
