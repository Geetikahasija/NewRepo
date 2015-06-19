## Here, we have two functions
## 1. makeCacheMatrix, which creates a special "matrix" object that can 
## cache its inverse
## 2. cacheSolve, a function which calculates the inverse of the special "matrix"
## returned by makeCacheMatrix


## makeCacheMatrix sourcecreates a list containing a function to 
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse
## 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      
      ##i)Substitutes the vector x with y (the new matrix)
      ##ii)Restores the value NULL to inv
      set <- function(y){
        x<<-y
        inv<<-NULL
      }
      
      ##If the matrix already exists, get the value from the cache using get function
      get<-function()x
      
      ##Stores the inverse value using setinverse
      setinverse<-function(inverse)inv<<-inverse
      
      ##Retrieve the inverse value,using getinverse
      getinverse<-function()inv
      
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## casheSolve takes a matrix as input and if the matrix is the same, it returns
## the inverse of the matrix from the cache.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## Get the inverse value from makeCacheMatrix function
        inv<-x$getinverse()
        
        ##If the inverse value is not NULL, return the value from the cache
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        
        ##If the inverse is found to be NULL, recompute the inverse of the matrix x
       
        data<-x$get()
        
        ##Use solve function to compute the inverse of the matrix and set the value
        ##of inverse
        
        inv<-solve(data,...)
        x$setinverse(inv)
        
        ##return the Inverse Value of the matrix
        inv
        
}