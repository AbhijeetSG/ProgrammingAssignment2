#####
# Programming Assignment 2
# Abhijeet Gulati 
# version 1.0
# July 2014
# Function name: makeCacheMatrix
# Function Arguments: accepts a square matrix
# Details: Function to set/get/setinverse and getinverse 
# of a cached square matrix
# 1. Set the value of the square matrix
# 2. get the value of the square matrix
# 3. set the value of the square matrix
# 4. get the value of the sqaure matrix

makeCacheMatrix <- function(mat = matrix()) {

     # Verify if the matrix is strict matrix and not any other object
     if (!is.matrix(mat)){
          print("Not a invertible matrix!")
          return
          }
     else     
     {
          inverse <- NULL
          set <- function(y) {
               mat <<- y
               inverse <<- NULL
          }
          get <- function() mat
          set_inv <- function(mat_inverse) inverse <<- mat_inverse
          get_inv <- function() inverse
          list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
     }
     
}

#####
# Programming Assignment 2
# Abhijeet Gulati 
# version 1.0
# July 2014
# Function name: cacheSolve
# Function Arguments: strict square matrix
# Details: Function checks for cached inverse matrix and if 
# no cache exists, creates it

cacheSolve<-function(m, ...){
     
     mi<-m$get_inv()
     if(!is.null(mi)) {
          message("getting cached inverse matrix")
          return(mi)
     }
     data<-m$get()
     
     mi<-solve(data) %*% data
     
     m$set_inv(mi)
     mi
}