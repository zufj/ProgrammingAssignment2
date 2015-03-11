# http://adv-r.had.co.nz/Performance.html
# http://kbroman.org/hipsteR/

# Dear fellow Courseran,
#
# At first, I did not really understand what was expected of us in this assignement.
# Apparently, apart from mechanically adapting the example script, we would have to explain
# how it all works.
# I tried to comment the code as I went through it to understand it. I believe for someone who already understands
# the issue, it might be helpful.
#
# Here is how I would simply explain what this caching algorithm does in simple terms:
#
# 1) Creates an extended type for the original variable that contains some flags
#
# 2) The flags are initialized within the specific cache function to register if this computation
# was done already or not
#
# 3) The cache function uses the flag to determine if:
#       a) The computation was already done for that variable and get it from the cache
#       b) The computation was not done and hence, does it while setting up a flag to keep the memory of the operation
#
# I don't really see any real-world application for me (easier to structure function so that calculation are not wastefully)
# unless I start developing packages or if I develop large sets of functions that depends on each other within some 
# kind of wrapper "main" function. I guess it is still interesting from a programming perspective :)
#
# If you have any experience with caching in R please do write about it in the evaluation!
#
# Cheers,

makeCacheMatrix <- function(x = numeric()) {
        
        ## Define m in local env.
        m <- NULL
        
        ## Define function "set" that, when called, looks for variables x,m to assign them the value y and NULL respectively
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Define "get" function that returns the matrix x
        get <- function() x
        
        ## Define "set" function that assign the solve parameter to m if found in parent environnements
        setsolve <- function(solve) m <<- solve
        
        ## Define "get" function that ouput the variable m
        getsolve <- function() m
        
        ## Final output: List containing
        ## 1) sets parent env. x
        ## 2) output local x
        ## 3) sets parent env. m
        ## 4) ouput local m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        
        ## initialize m in local env. with getsolve attribute of x
        m <- x$getsolve()
        
        ## KEY LINE FOR UNDERSTANDING
        # Test if m is not empy (meaning that if it is not empty, then the result wished is already available in cache)
        if(!is.null(m)) {
                
                message("getting cached data")
                
                ## Return result (cache)
                return(m)
        }
        
        ## get x value from parent env.
        data <- x$get()
        
        ## use data from parent env. for inversion calculation
        m <- solve(data, ...)
        
        ## KEY LINE FOR UNDERSTANDING
        ## send result of calculation to notify that it has been computed
        x$setsolve(m)
        
        ## output result of computation
        m
        
}

