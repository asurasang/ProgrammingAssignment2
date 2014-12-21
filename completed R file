## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
  s=NULL
  set=function(y)
  {
    x<<-y
    s<<-NULL
  }
  get=function() x
  setsolve <- function(solve) s <<-solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve
  )
}


## Write a short comment describing this function
#The following function calculates the value of the inverse matrix. 
#It first checks to see if the the value of the inverse matrix has already been calculated. 
#If so, it gets the value of the inverse matrix from the cache and skips the computation. 
#Otherwise, it calculates the value of the inverse matrix of the given matrix,
#and sets the value of the inverse matrix in the cache via the setsolve function.#

cacheSolve <- function(x, ...) 
{
  s <- x$getsolve()
  if(!is.null(s)) 
    {
    message("Return a matrix that is the inverse of 'x'")
    return(s)
    }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setsolve(s)
  s
}  

#Test#
#set.seed(1)
#aMatrix=matrix(rnorm(4),2,2)
#set.seed(1)
#aaa=makeCacheMatrix(matrix(rnorm(4),2,2))
#bMatrix=cacheSolve(aMatrix)
#aMatrix%*%bMatrix
