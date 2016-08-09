-## Put comments here that give an overall description of what your
-## functions do
-#makeCacheMatrix(): creates “matrix” object that can cache its inverse.
#cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ##         this list is used as the input to cacheSolve()
        
        m = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinv = function(inverse) m <<- inverse 
        getinv = function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        m = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(m)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(m)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        m = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(m)
        
        return(m)
}
#To test out these functions. I have created test(), 
#which takes in any invertible matrix, calculates its inverse twice using the above functions, 
#and prints out the times it takes for both runs. 

test = function(mat){
        ## @mat: an invertible matrix
        
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}
#we can try matrix of 1000 rows and 1000 columns.

set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
## Time difference of 1.946601 secs
## getting cached data
## Time difference of 0.0005111694 secs
