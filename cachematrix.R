## Put comments here that give an overall description of what your
## functions do

## Create an object matrix that will contain its inverse and cache it to avoid re processing

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
  message(is.null(m))
  set <- function(y){
          x <<- y
          m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)

}


##Uses makeCacheMatrix to find the inverse of a matrix and save the result in cache to avoid re procesing 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 m <- x$getInverse()
  
  if(!is.null(m)){
    message("Gets the invert from cache")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
 
}
