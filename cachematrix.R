## The first function (makeCacheMatrix) uses an invertible matrix as its input and results in a list of 4 functions (
#set, get, setsolve and getsolve). This list is then used as an argument for the second function (cacheSolve), which then 
#returns the inverse of the original matrix used as the input of the first function, either calculating it directly, if 
#it hasn't done that before and there's no matrix cached, or simply by retrieving the cached matrix. In this case, 
#before returning the inverse matrix, it also returns a message saying that it's using a cached matrix. 



#1) For the makeCacheMatrix, it uses a matrix as its argument and starts by defining an object "m" as null(which will be 
#defined later in the second function). Afterwards, it defines four funtctions: 
#1) set, which sets values for its argument(y) and m, setting y to be the matrix in the parent environment and m as null, 
#clearing any possible prior cached matrices; 
#2) get, which retrieves the matrix x
#3) setsolve which is the function "Solve", that calculates de inverse of a matrix, applied to m, and named solve. 
#4) getsolve, which retrieves the inverse matrix calculated.     


makeCacheMatrix <- function(x = matrix()) {
	
	 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
	

}


#2) ## The cacheSolve function then uses the list created by makeCacheMatrix to solve for the inverse of the original matrix 
#used as input in the first function. It's result is m, which is defined using the if clause. If m is null (which means that 
#this is the first time calculating this inverse), then the function solves for the inverse of x by first getting x and 
#calling it data and, after calculating the inverse, naming that m. It then stores the result.  If m is not null, that means 
#that it has already been calculated before and it uses the previous stored result, printing a message that it's using a 
#cached matrix before returning m. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getsolve() 
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
