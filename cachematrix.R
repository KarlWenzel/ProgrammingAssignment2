## WELCOME PEER GRADER!
##
## COMMENTS PERTAINING TO ENTIRE FILE
## The purpose for the following functions is to create a caching mechanism to store
## (invertable) R matrices and their respective inverse matrices.

## COMMENTS PERTAINING TO 1ST FUNCTION
## function: makeCacheMatrix
##
## param x: optional matrix arguement, defaults to an empty matrix
##
## this function is used to create a CacheMatrix object which has the
## ability to get/set a cached matrix, as well get/set its cached inverse

makeCacheMatrix <- function( x = matrix() ) 
{  
  xInverse <- NULL;
  
  set <- function( xSaveMe ) { x <<- xSaveMe; xInverse <<- NULL; };
  get <- function() { x; };  
  setInverse <- function( xInverseSaveMe ) { xInverse <<- xInverseSaveMe; };
  getInverse <- function() { xInverse; };
  
  list( set=set, get=get, setInverse=setInverse, getInverse=getInverse );
}

## COMMENTS PERTAINING TO 2ND FUNCTION
## function: cacheSolve
##
## param x: a CacheMatrix object created from the makeCacheMatrix() function (see above)
## param ...: further arguments passed through to the local solve() function call.
##
## this function returns the inverse of matrix x.  a cached value of the inverse is returned
## if available, otherwise the inverse is calculated locally and cached before being returned.

cacheSolve <- function( x, ... ) 
{
  xInverse <- x$getInverse();
  
  if ( !is.null( xInverse ) )
  {
    message("getting cached data");
    return( xInverse );
  }
  
  xInverse <- solve( x$get(), ... );
  x$setInverse( xInverse );
  xInverse;  
}
