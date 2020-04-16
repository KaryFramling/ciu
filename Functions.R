

# Root mean squared error between two sets of numbers.
root.mean.squared.error <- function(actual, estimated) {
  return(sqrt(mean.squared.error(actual, estimated)))
}
 
# Mean squared error between two sets of numbers.
mean.squared.error <- function(actual, estimated) {
  if ( is.null(dim(actual)) )
    n <- length(actual)
  else
    n <- prod(dim(actual))
  return(sum.squared.error(actual, estimated)/n)
}

# Sum of squared errors between two sets of numbers.
sum.squared.error <- function(actual, estimated) {
  return(sum((actual-estimated)^2))
}

# Return index of maximum value. If there are more than one, then choose
# one randomly. 
random.of.max <- function(values) {
  m <- values==max(values)
  inds <- 1:length(values)
  w <- inds[m][floor(runif(1)*sum(m)) + 1]
  w
}

# Normalize values in matrix so that their sum is one (row-wise).
normalize.to.one <- function(vector) {
  return(vector/rowSums(vector))
}

# epsilon-greedy policy
e.greedy.new <- function(eps) {
  epsilon <- eps

  p <- list (
             get.epsilon = function() { epsilon },
             set.epsilon = function(eps) { epsilon <<- eps },
             get.action = function(qvalues) {
               if ( runif(1) < epsilon )
                 floor(runif(1)*length(qvalues)) + 1 # Random index [1,nacts]
               else
                 random.of.max(qvalues) # Get greedy
             }
             )
  class(p) <- c("EpsilonGreedyPolicy")
  return(p)
}

# "Boltzmann" softmax policy
boltzmann.new <- function(temperature=1) {
  t <- temperature

  exprob <- function(a,t) { exp(a/t) }
  
  get.action <- function(qvalues) {
    # Make vector of probabilities
    v <- as.array(qvalues)
    ev <- apply(v,1,exprob,t)
    s <- sum(ev)
    probs <- ev/s
    # Choose according to probabilities.
    r <- runif(1)
    ps <- probs[1]
    for ( i in 1:length(probs) ) {
      if ( !is.nan(ps) && r < ps )
        return(i)
      ps <- ps + probs[i + 1]
    }
    return(length(probs)) # Just in case
  }
  
  p <- list (
             get.temperature = function() { t },
             set.temperature = function(temperature) { t <<- temperature },
             get.action = function(qvalues) { get.action(qvalues) }
             )
  class(p) <- c("BoltzmannPolicy")
  return(p)
}  

# Generic discretizer "class". Returns interval indices
# from 1 to "nintervals". "impose.limits" indicates if min/max values
# should be used for limiting first/last interval. In that case, the
# returned class is zero if the value doesn't fit any interval. 
discretizer.new <- function(minval, maxval, nintervals, impose.limits=FALSE) {
  min <- minval
  max <- maxval
  nints <- nintervals
  range <- (max - min)/nints;
  imp.limits <- impose.limits
  list (
        get.nbr.classes = function() { nints },
        get.class = function(value) {
          if ( imp.limits ) {
            if ( value < min || value > max )
              return(0)
          }
          min(floor(max((value - min)/range, 0)) + 1, nints)
        }
        )
}

# Generic discretizer "class". Returns interval indices
# from 1 to "nintervals"
uneven.discretizer.new <- function(limits) {
  lim <- limits
  list (
        get.nbr.classes = function() { length(lim) + 1 },
        get.class = function(value) {
          i <- 1
          while ( i <= length(lim) && value > lim[i] )
            i <- i + 1
          return(i)
        }
        )
}

test <- function() {
  d <- discretizer.new(-1, 2, 5)
  for ( i in seq(-1.5, 2.5, 0.1) )
    print(d$get.class(i))
}

test.uneven <- function() {
  d <- uneven.discretizer.new(c(-6,-1,0,1,6))
  for ( i in seq(-13, 13, 1) )
    print(paste(i, d$get.class(i)))
}

#test()
#test.uneven()

classifying.function.approximator.new <- function(function.approximator, classifier=NULL) {
  c <- classifier
  fa <- function.approximator
  inputs <- NULL
  outputs <- NULL
  state <- NULL

  eval <- function(invals) {
    inputs <<- invals
    if ( !is.null(c) )
      state <<- c$get.vector(inputs)
    outputs <<- fa$eval(state)
  }
  
  m <- list (
             get.inputs = function() { inputs },
             get.outputs = function() { outputs },
             eval = function(invals) { eval(invals) }
             )
  class(m) <- c("FunctionApproximator")
  return(m)

}

# Weighted sum "objectified". Weights must be given as one-column matrix. 
# The number of columns in input matrix must be same 
# as number of rows of w. 
# Example: ws<-weighted.sum.new(matrix(c(0.2,0.8),ncol=1)); y<-ws$eval(matrix(c(0.5,0.5,1,1), ncol=2, byrow=TRUE)); y
weighted.sum.new <- function(weights) {
  w <- weights
  
  eval <- function(x) {
    y <- x%*%w
  }
  
  m <- list (
    get.weights = function() { w },
    eval = function(x) { eval(x) }
  )
  class(m) <- c("FunctionApproximator")
  return(m)
}

# Nonlinear "decision support" model for two inputs. 
# Example: 
nonlineardssmodel.two.inputs.new <- function() {

  eval <- function(x) {
    y <- x[,1]^0.5 + x[,2]^2
    max <- 2 # max(y)
    y <- y/max
  }
  
  m <- list (
    eval = function(x) { eval(x) }
  )
  class(m) <- c("FunctionApproximator")
  return(m)
}

# This function takes a list of vectors as parameter. Every vector
# contains a list of values. The function then creates a matrix that
# has as many rows as necessary to contain all the permutations of the
# values in the vectors. Returns the resulting matrix, which then
# has as many columns as there are vectors in the
# "list.of.value.vectors" list. 
# Example: l<-list(c(1,2,3),c(4,5,6)); create.permutation.matrix(l)
create.permutation.matrix <- function(list.of.value.vectors) {
  # Calculate number of rows needed
  ncol <- length(list.of.value.vectors)
  nrow <- 1
  for ( i in 1:ncol )
    nrow <- nrow*length(list.of.value.vectors[[i]])
  w <- matrix(nrow=nrow, ncol=ncol)
  rep.cnt <- 1
  for ( i in ncol:1 ) {
    values <- c()
    for ( val in 1:length(list.of.value.vectors[[i]]) )
      values <- c(values, rep(list.of.value.vectors[[i]][val], rep.cnt))
    w[,i] <- values
    rep.cnt <- rep.cnt*length(list.of.value.vectors[[i]])
  }
  return(w)
}

# Create an "input matrix" for e.g. a neural network or similar that
# contains all combinations of values from "mins" to "maxs" with
# steps "steps". "indices" indicates the column indices in the resulting
# matrix where the values should be inserted. "default.inputs" gives
# the values for all other inputs, which thus have constant values.
# The number of columns of the returned matrix is the bigger of the maximum
# value of "indices" or the length of "default.inputs". The number of
# rows depends on the number of possible input value combinations. 
# Example: create.input.matrix(c(2,4),c(0,10),c(1,20),c(0.2,2), 0)
create.input.matrix <- function(indices, mins, maxs, steps, default.inputs=0) {
  # Get number of columns desired
  ncol <- max(indices, length(default.inputs))

  # Create list of value vectors and get matrix with all permutations of
  # them.
  l <- list()
  for ( i in 1:length(indices) ) {
    l[[i]] <- seq(mins[i], maxs[i], steps[i])
  }
  pm <- create.permutation.matrix(l)

  # Create matrix, initialize with default inputs and then insert values.
  m <- matrix(data=default.inputs, nrow=nrow(pm), ncol=ncol, byrow=TRUE)
  for ( i in 1:length(indices) ) {
    m[,indices[i]] <- pm[,i]
  }
  return(m)
}

# Create z matrix for 3D plot, e.g. with "persp". 
# mins: Vector with minimum (x,y) values, e.g. c(0,0)
# maxs: Vector with maximum (x,y) values, e.g. c(1,1)
# steps: Vector with spacing between (x,y) values, e.g. c(0.1,0.1)
# f: a function that takes an [n,2] dimension input matrix as input and gives a [n,1] output 
#    (or [1,n], doesn't matter). 
# Returns: z matrix with number of rows equal to length of x vector, number of columns length of y vector 
# Example: x <- seq(0, 1, 0.1); y <- seq(0, 1, 0.2); z <- create.z.for.persp(x, y, rowSums); persp(x,y,z)
# Example with weighted.sum object:
# ws<-weighted.sum.new(matrix(c(0.2,0.8),ncol=1)) 
# x <- seq(0, 1, 0.1); y <- seq(0, 1, 0.1); z <- create.z.for.persp(x, y, ws$eval); persp(x,y,z)
# Example with nonlineardssmodel.two.inputs.new object:
# nl<-nonlineardssmodel.two.inputs.new()
# x <- seq(0, 1, 0.1); y <- seq(0, 1, 0.1); z <- create.z.for.persp(x, y, nl$eval); persp(x,y,z)
create.z.for.persp <- function(x, y, f) {
  l <- list(y,x)
  m <- create.permutation.matrix(l)
  zvals <- f(m)
  z <- matrix(zvals, nrow=length(x), ncol=length(y))
  return(z)
}

# Create affine transformation "object" with transformation matrices
# "A" and "b". If "A" or "b" are NULL, then they need to be initialised
# using some other method.
affine.transformation.new <- function(A=NULL, b=NULL) {
  A.matrix <- A
  b.matrix <- b

  m <- list(
            get.A = function() { A },
            get.b = function() { b },
            set.A = function(value) { A <<- value },
            set.b = function(value) { b <<- value },
            eval = function(x) { return(A%*%x + b) }
          )
  class(m) <- c("AffineTranformation", class(m))
  return(m)
}

# Return affine transformation "object" that scales and translates the
# coordinates from one set of ranges to another range. 
scale.translate.ranges <- function(minvals, maxvals, newmins, newmaxs) {
  old.ranges <- maxvals - minvals
  new.ranges <- newmaxs - newmins
  ratios <- new.ranges/old.ranges
  A <- diag(length(minvals))*ratios
  b <- (newmins - minvals)*ratios
  return(affine.transformation.new(A=A, b=b))
}


