## The first function will set a number of functions that can set a matrx, get its value,
## inverse it and get the inverse value.

## 

makeCacheMatrix <- function(x = matrix()) {

      Inv<-NULL
      x<-NULL
      
      setMatrix<-function(y){
            x<<-y
      }
      
      getMatrix <- function() x
            
      cacheInverse<- function() Inv<<-solve(x) 
      
      getInverse<- function() Inv

      
      list(setMatrix = setMatrix,
           getMatrix=getMatrix, 
           cacheInverse = cacheInverse,
           getInverse = getInverse)
}


## In this one, you can insert the object you generated with the function above, as well as an user
## inputed matrix in "..."
## I assumed that "..." would be an user inputed matrix.

## In this sense, the function checks if ... exists and, if true, it compares it with the matrix 
## from MakeCacheMatrix to see if tey're the same. If tey aren't, ... will always take priority
## to calculate de inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' or ... ( if different, ... takes priority)
      
      
      if(is.null(x$getInverse())){ 
          
                  if(is.null(x$getMatrix())) {
                        ifelse(missing(...),"Input manual matrix",solve(...))
                  } else { 
                        if(missing(...)) { solve(x$getMatrix())  }
                        else{ ifelse(x$getMatrix()==...,solve(x$getMatrix()),solve(...)) }
                  }
      }      
                        
      else { if(missing(...)){x$getInverse() }
                  else{ ifelse(x$getMatrix()==..., x$getInverse(), solve(...)) }
      }
}      

