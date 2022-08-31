## makeCacheMatrix - Set / Get matrix data and it's inverse in cache
## cacheSolve - Retrieve the inverse matrix from cache or calculates it if the
## cache is empty. Once calculated, its cached


## Set / Get matrix data and it's inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  
  mTrx <- NULL
  
  setData <- function(y){
    x <<- y
    mTrx <<- NULL
  }
  
  getData <- function() x
  
  setMatrxInv <- function(y_matrix) mTrx <<- y_matrix
  
  getMatrxInv <- function() mTrx
  
  list(setData=setData, getData=getData,
       setMatrxInv=setMatrxInv, getMatrxInv=getMatrxInv)
  
}


## cacheSolve - Retrieve the inverse matrix from cache or calculates it if the
## cache is empty. Once calculated, its cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c <- x$getMatrxInv()
  
  if(!is.null(c)){
    print("Retrieving from cache...")
    return(c)
  }
  
  mData <- x$getData()
  i <- solve(mData)
  x$setMatrxInv(i)
  
  i
}

Footer
