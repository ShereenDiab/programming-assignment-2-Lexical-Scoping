## 2 functions that cache the invese of a matrix

## create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL          ## initiate the inverse property
set <- function( matrix ) {   ## method to set the matrix
 m <<- matrix
i <<- NULL}
  
get <- function() {m}  ## method to get the matrix and return it

  setInverse <- function(inverse) {    ## method to set the inverse of the matrix
  i <<- inverse}
  
getInverse <- function() {i}    ## method to get the inverse of the matrix
  
list(set = set, get = get,       ## method to list the function of the matrix
     setInverse = setInverse,
     getInverse = getInverse)}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix is the same
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  m <- x$getInverse()         ## return a matrix that is the inverse of x
  
  if( !is.null(m) ) {              ## return the inverse if it is already set
    message("getting cached data")
    return(m)}
  data <- x$get()           ## get the matrix from the object
  
  m <- solve(data) %*% data      ## Calculate the inverse using matrix multiplication
  
  x$setInverse(m)                 ## Set the inverse to the object
  m}                           ## Return the matrix
