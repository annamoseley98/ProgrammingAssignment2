
## Create a matrix that is able to cache its inverse, by including how to get and set the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  ## Inverse initialised to null
  inv <- NULL
 
  ## Setter and getter functions for matrix
  set <- function(y){
    x <<-y
    inv<<-NULL
  }
  get <- function() x
  
  ## Setter and getter functions for inverse
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  ## Return combined list of setter and getter functions
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the inverse of a matrix, by first checking to see if the inverse is cached and calculating the inverse if not

cacheSolve <- function(x, ...) {
  ## Check if there is a stored inverse and return if so
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Fetching cached data...")
    return(inv)
  }
  
  ## If no stored inverse, calculate inverse then cache and return it
  z <- x$get()
  inv <- solve(z)
  x$setinv(inv)
  inv
}
