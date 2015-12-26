
## makeCacheMatrix is a function that stores matrices in memory usesing scoping rules. 

makeCacheMatrix <- function(X = matrix()) {
        inverse <- NULL
        set <- function(Y){
                X <<- Y
                inverse <<- NULL
        }
        get <- function() X
        setinverse <- function(Inverse) inverse <<- Inverse
        getinverse <- function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## catcheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache. The function uses the corpcor package to avoid determinants and use orthogonal 
## descomposition. Note that if the corpcor package is not already installed, the function will
## try to install and load the package.


cacheSolve <- function(X, ...) 
{
        if(require("corpcor")){
                print("corpcor is loaded correctly")
        } else {
                print("trying to install corpcor")
                install.packages("corpcor")
                if(require(corpcor)){
                        print("corpcor installed and loaded")
                } else {
                        stop("could not install corpcor")
                }
        }
        inverse <- X$getinverse()
        if(!is.null(inverse)){
                message("matrix is in memory")
                return(inverse)
        }
        message("inverse is not in memory so the inverse (if exist) is gonna be computed")
        data <- X$get()
        ##The standard definition for the inverse of a matrix fails if the matrix is not square or singular.
        ##However, we can generalize the inverse using singular value decomposition.We can achieve
        ##that using pseudoinverse function from the corpcore package. More info: ?pseudoinverse or
        ##from online documentation here https://cran.r-project.org/web/packages/corpcor/corpcor.pdf
        inverse <- pseudoinverse(data, ...)
        X$setinverse(inverse)
        inverse
}

### To test the functions, uncomment the following test cases.
        ##square matrix
#         X <- matrix(rpois(25,3), nrow = 5)
#         cX <- makeCacheMatrix(X)
#         cX$get()
#         cacheSolve(cX)
#         cacheSolve(cX)
#         invX <- cacheSolve(cX)
#         
#         ##rectangular matrix rows > cols
#         Y <- matrix(rpois(20,2), nrow = 5, ncol = 4)
#         cY <- makeCacheMatrix(Y)
#         cY$get()
#         cacheSolve(cY)
#         cacheSolve(cY)
#         invY <- cacheSolve(cY)
#         
#         ##rectangular matrix rows < cols
#         Z <- matrix(rpois(20,1), nrow = 4, ncol = 5)
#         cZ <- makeCacheMatrix(Z)
#         cZ$get()
#         cacheSolve(cZ)
#         cacheSolve(cZ)
#         invZ <- cacheSolve(cZ)
