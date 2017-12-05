## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(ncol = dim(x)[2], nrow =dim(x)[1] )
  #setting the data
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  #getting the data
  get <- function(){ x } 
  #getting solved matrix
  getsolve <- function(){ m }
  
  #setting solution of the matrix
  setsolve <- function(){ if( det(x) != 0 ) m <<- solve(x)  
                          else {
                            message("This matrix do not have inverse!")
                            m<<-NA
                          }
                        }

  #object to return
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Getting the result
  m <- x$getsolve()
  
  #if result already exist - returning it
  if(!all(is.na(m))) {
    message("getting cached data")
    return(m)
  }
  #Solving the matrix and returning the result
  x$setsolve()
  message("getting calculated data")
  x$getsolve()
}



#Checks 
# The first check
        m1<-matrix(1:16,nrow = 4)
        mc1<-makeCacheMatrix( m1 )
        cacheSolve(mc1)
        #This matrix do not have inverse!
        #[1] NA
        ## This is correct because matrix has zero discriminant( do not have inverse )
        
# The Second Check
        m2 <- matrix(rnorm(16,4,2),ncol = 4)
        mc2<-makeCacheMatrix( m2 )
        cacheSolve(mc2)
        # [1] inverse matrix returned

        cacheSolve(mc2)
        # getting cached data
        # [1] inverse matrix returned

# The Third Example( very big martix ) - to check productivity
        m3  <- matrix(rnorm(16000000,67,23), nrow = 4000, byrow = TRUE)
        ms3 <- makeCacheMatrix(m3)
        cacheSolve(ms3)
        #takes some time to compute
        
        cacheSolve(ms3)
        #computes almost immediatly
