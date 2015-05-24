#functions will cach and return inverse of a square matrix

#holds both the regular matrix and inverse
makeCacheMatrix<- function(x = matrix(c(1,2,3,4),nrow=2)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setMtx <- function(mtx) m <<- mtx
  getMtx <- function() m
  list(set = set, get = get,
       setMtx = setMtx,
       getMtx = getMtx)
}

#detremines if a new inverse must be calculated, caches inverse into makeCacheMatrix, prints results
cacheSolve <- function(mCm = makeCacheMatrix(),  ...) {
   
    m0 <- mCm$getMtx()
    message = "cached inverse matrix is"
    
    if (is.null(m0)){
      m0 =  solve(mCm$get())
      mCm$setMtx( m0 )
      message = "inverse matrix is"
    } 
    print (message)
    m0
}
