makeCacheMatrix <- function (x= matrix()) #to create a special matrix object
{
  j <- NULL
  set <- function(y)
  {
    x <- y
    j <- NULL
  }
  get <- function()x
  setInverse <- function(inverse)
    j <- inverse
  getInverse <- function()j
  list(set=set, egt=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x,) #to compute
{
  j <- x$getInverse()
  if(!is.null(j))
  {
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,)
  x$setInverse(j)
  j
}
