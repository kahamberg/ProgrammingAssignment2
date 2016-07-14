## This for R progamming assingment 2 
## this will compute the inverse of an invertable matrix, using a cached copy of the
## inverse if the inv function is called multiple times with the same matrix (kinda)

## This function will cache an original matrix and processed matrix ( the inverse for this class)


makeCacheMatrix <- function(mat = matrix()) {
   matProc <- NULL
   # save a new version of the original matrix, 
   # and null out the processed value (inverse)
   set <- function(y) {
      mat <<- y 
      matProc <<- NULL 
   }
   
   # get the original matrix
   get <- function() {
      mat 
   }
   
   # set the processed (inverse for this class project) matrix
   setProc <- function( procVal ) {
      matProc <<- procVal 
   }
   
   # get the processed (inverse for this class project) matrix
   getProc <- function() {
      matProc     # return the processed matrix (inverse for this class)
   }
   
   list ( set = set, 
          get = get,
          setProc = setProc,
          getProc = getProc ) 
}


## This function will compute an inverse of a square matrix,
## it uses a chached copy if the inverse has already been computed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   inv <- x$getProc() ;
   if ( is.null(inv) ) {
      ## the inverse has not been computed, so do it...
      mat <- x$get()    # get the original matrix
      inv <- solve(mat) # compute the inverse
      x$setProc(inv)    # cache the inverse
      message('computing inv')
   } else {
     message('using cached data')
   }
   
   inv         # return the result
      
}
