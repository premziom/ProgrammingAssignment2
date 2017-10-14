
## Put comments here that give an overall description of what your
## It is my function to cache an inverse matrix and then return the inverse from cach or calculate the new inverse if the cache is null
##


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  cache <- NULL
  set<-function(y){
    x <<-y
    cache <<-NULL
  }
  #get the value of the matrix
  get<-function() x
  #set the value of the inversion
  setInverse<-function(inverse) cache <<-inverse
  #get the value of the inversion
  getInverse <- function() cache  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  #get matrix from cache
  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached matrix")
    return(cache)
  }
  #calculate inverse matrix
  data <- x$get()
  cache <- solve(data, ...)
  x$setInverse(cache)
  cache
}
