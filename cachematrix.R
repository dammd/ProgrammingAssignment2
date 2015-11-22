##Coursera-Data Science Specialization-R Programming
##Programming Assignment 2
##Completed 11/22/2015 @ 
##David Myers

#Function descriptions
##Overall- This is a 2 command process for 
    #1) setting up a cached matrix with commands to access update and access it
    #2) taking a given matrix and then assessing if the cache has an inverse
        #if not, then calculating and storing that inverse
        #if cache exists then checking to see if it is correct inverse for 
            #current matrix.

##makeCacheMatrix-
    ## makes empty(NULL) matrix x.inv for use as cached matrix
    ## sets up commands to store and update cached matrix to be used elsewhere
    ## NOTE: one must use the object result of this function (ie x<- f(x))


makeCacheMatrix <- function(x = matrix()) {
      x.inv <- NULL #sets up an empty object for cached inverse matrix of x
      
      set <- function(y) {
          ##sets values of x and x.inv from parent environment if called 
          ##ie makeCacheMatrix,the main function space, back to a base matrix y 
          
          x <<- y        #sets x from parent env rather than  global env
          x.inv <<- NULL #sets x.inv from parent env rather than global env
      }
      
      #creating functions for accessing/updating cached inverse
      get <- function() x                    #calls the matrix x 
      setInv <- function(inv) x.inv <<- inv  #sets cached inverse matrix of x
      getInv <- function() x.inv             #calls cached inverse matrix of x
      list(set = set, get = get, setInv = setInv, getInv = getInv)
            #creates a list of names to be used in cacheSolve function to 
            #call the needed function 
}


##cacheSolve- 
    ##Takes the object created by makeCacheMatrix (CRITICAL)
    ##Checks inverse matrix cache for value and if not present
    ##calculates the inverse matrix and stores the result in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInv()                      #calls inverse matrix of x if in cache
    
    if(!is.null(m)) {                    
        message("getting cached data")   #comment for output when cache used
        return(m)                        #and returns the cached inverse matrix
                                         #thus ending the cacheSolve function
    }
    
    data <- x$get()                      #pulls matrix x into var for processing
    m <- solve(data, ...)                #if cache was empty, solves for inverse
    x$setInv(m)                          #sets cached inverse of matrix x
    m                                    #returns inverse matrix of x ending fx
}


#Testing function of commmands for correct behavior
test.matrix<-matrix(runif(9,1,100),3,3)
test.matrix

x<-makeCacheMatrix(test.matrix)              #sets up cached matrix for test
      ##critical note, one must pass the result of makeCacheMatrix forward
      ##not orginal matrix

cacheSolve(x)                                #this time it should solve inverse
cacheSolve(x)                                #this time it should use cache

#change input matrix
test.matrix2<-matrix(runif(9,1,100),3,3)
test.matrix2

x<-makeCacheMatrix(test.matrix2)              #sets up cached matrix for test2x
cacheSolve(x)                                 #should solve and return new inverse
cacheSolve(x)                                 #should return new cached inverse
    
