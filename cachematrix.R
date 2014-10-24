
## This function creates a special matrix that can cache it's inverse
## The inverse is never itself calculated in makeCacheMatrix, but the variable ( Ix )
## is set to NULL to accomodate the user modifying the free variable in the global enviroment

## an example of the proper way to call this function is:
## 
##  free_variable <- makeCacheMatrix(matrix_g) 
## 
## where matrix_g is a user defined matrix
## the matrix may then be accesed by :
##
## free_variable$get() 
##
## this will return the original matrix
## once cacheSolve is called by;
##
## cacheSolve(free_variable)
##
## then the inverse is calculated and can be accessed by using:
##
## free_variable$get_inverse()
##
##



makeCacheMatrix <- function(x = matrix()) {
        ## first we will set the inverse matrix variable, defined as Ix, to NULL 
        Ix <- NULL
        
        ## Now we will declare a funtion called set that will assign the input matrix to the value x, the
        ## input of the parent ( makeCahceMatrix ) function. The value of the inverse matrix will 
        ## be set to NULL, since this would be a case where whatever the original value was would no 
        ## longer be valid
        
        set<- function(y) {
                x <<- y         ## assigning x the value of y in the parent enviroment
                Ix <<- NULL     ## setting the value of the inverse matrix to NULL    
        }
        
        ## declaring a function that the user will call  to return the original matrix input
        ## the format for the call will [free variable]$get()  where [free variable] is some
        ## variable assigned in the global enviroment
        
        get <- function() {x} ## braces show it's a function, not some obscure syntax not yet covered in class
        
        
        ## decarling a function (which will be called internally by the cacheSolve function below)
        ## that sets the Ix in the parent (makeCacheMatrix) environment.  This function should not be called
        ## by the user directly, unless they are also solving for the inverse
        
        set_inverse <- function(x_inverse) {Ix <<- x_inverse}  ## always use braces, it's confusing otherwise
        
        ## declaring a function which the user will call via a [free variable]$get_inverse() call 
        ## that will print the inverse of the function
        
        get_inverse <- function() {Ix} ## again, neophyte programmers expect braces with functions!
        
        ## finally, the function generates and returns a list of the functions that will be accessed by 
        ## the usre, primarily set, get and get_inverse.  A skilled programmer would have a way of printing
        ## a warning to users about using set_inverse directly, but that was not part of the programming spec
        ## and it is getting late
        
        ## these functions will be 
        
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Once the user has initialized a free varaible with the free_variable <- makeCacheMatrix(matrix_g)
## function, the user can then pass the free variable to the cacheSolve function by entering:
##
## cacheSolve(free_variable)
## 
## this will use the solve function to generate an inverse matrix, or output the current inverse
## if one has already been created for free_variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        ## First we are assigning the variable Ix with whatever is in Ix called in the 
        ## makeCacheMatrix 
        ## This will be a NULL value for a free variable being called the first time
        ## or if a new value has been assinged using the $set() function
        ## if the value of the inverse has been set using $set_inverse in another contex 
        ## such as the command line, that will cause an error if the inverse is not correct
        ## so don't do that
        
        Ix <- x$get_inverse() 
        
        
        ##Next a check is done to see if the value is not NULL
        ## if so, the current value is returned  and the message is passed that cache data
        ## is being used
        ## the function then exits using a return fuction and outputting the current value of Ix
        
        if(!is.null(Ix)) {
                message("getting cached data")
                return(Ix)
        }
        
        ## if not, then the current value for the matrix is obtined with the $get
        
        data <- x$get()
        
        
        ## the solve() function is used to solve for the inverse
        ## hopefully one exists!
        
        Ix <- solve(data)
        
        
        #then the free_variable$set_inverse function is called, assigning a value to Ix that may 
        ## be accessed by calling free_variable$get_inverse() function or cachematrix(free_variable)
        x$set_inverse(Ix)
        
        ## and the current inverse matrix is also returned
        
        Ix
        
}
