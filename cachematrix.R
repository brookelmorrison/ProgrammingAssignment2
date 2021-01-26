## R uses lexical scoping to find the values of free variables. Lexical scoping 
## means that R will search for these values in the environment in which the
## function was defined. The below functions demonstrate the use of lexical 
## scoping in R.

## This function creates a matrix object that can cache its inverse and returns 
## an object that is of the type makeCacheMatrix. First, it 
## initializes objects x and m. x is initialized as an empty matrix and m is 
## initialized as NULL within the makeCacheMatrix environment. Then it defines 
## the set function. Set assigns y to the parent environment as an object 
## named x and assigns NULL to the parent environment as an object named m. 
## Next, it defines the get function, which, due to lexical scoping retrieves x 
## from the parent environment of makeCacheMatrix. Then it defines setinverse. 
## After setinverse completes, it assigns the input argument to m in the parent 
## environment. Then, it defines getinverse, which retrieves m from the 
## makeCacheMatrix environment. Next, it assigns and names the setters and 
## getters we defined to a list and returns it to the parent environment. 
## Naming these functions allows the use of the $ extract operator.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated then 
## cacheSolve should retrieve the inverse from the cache. First, cacheSolve 
## tries to find the inverse of the object that is passed as the argument 
## to the function. Then, if the result is not null, it displays a message of 
## "getting cached data" while it returns the inverse of the matrix to the 
## parent environment. If getinverse is NULL, then it gets the matrix object, 
## calculates the inverse of the matrix, sets m to the inverse of the matrix, 
## returns m to the parent environment, and prints it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
