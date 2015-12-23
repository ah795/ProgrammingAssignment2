##Goal of the assignment is to write a pair of functions that cache the inverse of a matrix


##First function is makeCacheMatrix:  
##This function creates a special "matrix" object that can cache its inverse.

##Computing the inverse of a square matrix can be done with the solve function 

##The first function, makeCacheMatrix creates a special "vector", 
##which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix<- function (x=matrix()) {
  
##sets matirx inverseMatrix to null--creates a place holder for future values
  inverseMatrix<- NULL
##defines a function to set the matrix x, to a new matrix y
##resets the inverseMatrix to NULL
  set <- function(y) {
    x<<- y
    inverseMatrix<<- NULL
  }

##return the matrix x 
  get<- function()x
##set the inverseMatrix to inverse
  setInverse <- function(inverse) inverseMatrix <<- inverse
##returns the matrix inverseMatrix
  getInverse <- function() inverseMatrix
##creates a special list contianing all the function just defined
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


##Second Function is cacheSolve: 
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

##Returns the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse
  if(! is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv<- solve(data,..)
  x$setInverse(inv)
  inv
}
