## Before running makeCacheMatrix I create a random matrix (see comments beloss)
## To check whether a code works with other matrices, you should redefine
##a variable ''matrass'' and change ''n'' in ''idenmat'' respectively.
## My makeCacheMatrix_function uses a randomly generated matrix and creates 
##variables getglobal, setinvglobal, getinvglobal in the cache.

n<-sample(2:5,1) #Random choice of columns-rows number as integers from 2 to 5
print(n)
matrass<-matrix(rnorm(n*n),n,n) #Random matrix with number of columns-rows of n 
print(matrass)
idenmat <- matrix(0, n, n) #Zero matrix of size n
diag(idenmat) <- 1 #Assigning diagonal elements of the zero matrix value of 1 
print(idenmat)
makeCacheMatrix <- function(x = matrass) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        getglobal<<-get()
        setinverse<-function() m<<-solve(x)
        setinvglobal<<-setinverse()
        getinverse<-function() m
        getinvglobal<<-getinverse()
}
## My cacheSolve_function at first assigns an inverse matrix (getinversglobal) 
## and an initial martix (getglobal) to ''m'' and ''data'', respectively.  
## If non_zero m is an inverse matrix to data than m is retrieved from cache.
## Alternatively, cacheSolve calculates an inverse matrix
cacheSolve <- function(x, ...) {
        m <- getinvglobal
        data <- getglobal
        check<-data*m # A variable needed to check whether m is an
        #inverse matrix to the specific matrix 
        if(!is.null(m) && check==idenmat) {#If m, i.e. inverse matrix, is not 
                #zero and multiplication of ''matrass'' and ''m'' equals to 
                #identity matrix than data are chosen from cache
                message("getting cached data")
                return(m)
        }
        m <- solve(data)
        m
}
##To check whether the code works, just run it. Firstly, a code will print 
##number of columns and rows. Secondly, it will print an initial matrix and an
##identity matrix with relevant number of rows-columns.
## Then,input makeCacheMatrix()
## Finally, input cacheSolve() to see an inverse matrix.





