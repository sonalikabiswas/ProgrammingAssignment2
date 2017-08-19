## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets the matrix to cache and gets it, also it set the inverse of matrix and gets it

makeCacheMatrix <- function(x = matrix()) {
invMatrix <- NULL 

   set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x                             
  setInverse <- function(inverse) invMatrix <<- inverse 
  getInverse <- function() invMatrix                    
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve fetches the inverse of matrix from cache using getInverse function , if not present then gets the matrix from get function 
##and calculates the inverse and set the inverse using setInverse function

cacheSolve <- function(x, ...) {
      invMatrix <- x$getInverse()
      if(!is.null(invMatrix)) {                       
       message("getting matrix")        
       return(invMatrix)
      }
        
   data <- x$get()                     
  invMatrix <- solve(data, ...)             
  x$setInverse(invMatrix)                         
  return(invMatrix)             
        ## Return a matrix that is the inverse of 'x'
}

