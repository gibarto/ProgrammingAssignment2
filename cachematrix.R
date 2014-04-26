## Caching the inverse of a matrix to avoid having to re-solve

## Make and store a matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ## set solution to NULL at start
  set <- function(y) {  ## move x (fun) and s(sol) outside env for retrieval
    x <<- y
    s <<- NULL
  }
  get <- function() x  ## gets x back
  setsol <- function(solve) s <<- solve ## solves s
  getsol <- function() s  ## gets s back after setsol
  list(set = set, get = get,  ## return the results in a list
       setsol = setsol,
       getsol = getsol)

}

## after setting matrix with makeCacheMatrix, use this to get the solution
## so that if it's already solved, you don't have to wait

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsol()  ## check the cache
  if(!is.null(s)) {  ## if already stored, return what's stored
    message("getting cached data")
    return(s)
  }
  data <- x$get()  ## set and return the solution if it's not already stored
  s <- solve(data, ...)
  x$setsol(s)
  s
}
