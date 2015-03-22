## Overall, these two functions fulfill matrix inversion. Due to the large workload of matrix inversion calculation, the programee
## should be designed to reduce work of calculation by store the matrix entered as well as its inversion in a cache place and
## if the matrix entered the second time is the same as the cached one, the programme will directly print the stored result.

## This function send the matrix entered for inversion calculation and at the same time store it in cache

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Set the value of matrix
  get <- function() x
  ## Get the value of matrix
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This calculation makes comparison between the cached matrix and the new one that has been entered. If they are the same, the
## message "getting cached data" is printed and the stored result will be returned.

cacheSolve <- function(x, ...) {
     i <- x$getinv()
  if(!is.null(as.vector(i))) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  print(data)
  i <- solve(data)
  x$setinv(i)
  i
}
