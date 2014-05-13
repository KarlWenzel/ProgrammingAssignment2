## The purpose for the following functions is to create a caching mechanism to store
## (invertable) R matrices and their respective inverse matrices.


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


## function: cacheSolve
##
## param x: a CacheMatrix object created from the makeCacheMatrix() function (see above)
## param ...: further arguments passed to or from other methods
##
## this function returns the inverse of matrix x.  a cached value of the inverse is returned
## if available, otherwise the invserse is calculated locally.

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
