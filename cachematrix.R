## Creates "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y) {
          x <<- y  
          m <<- NUL
  }
  get<-function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <-function() m
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

## Check to see if x matrix inverse has been cached if true return the cache value
## Else solve and cache inverser
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
        
  matrix <- x$get() 
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m   
}

