# "R" implementation of Contextual Importance and Utility.
#
# Kary Främling, created in 2019
#

source("Interfaces.R")
source("Functions.R")

# Create CIU object
# Calculate Contextual Importance (CI) and Contextual Utility (CU) for the "black-box" bb.
# Arguments: 
# bb: "black-box" object. Must be an object that inherits "FunctioApproximator" (implements "eval" method) 
#   or some other supported model. At least "rf" model of caret-library and "lda" model 
#  from MASS are supported. It might be that just about all caret models work directly 
#  also, as well as all models that have a similar "predict" method as lda.
#  Otherwise, the prediction function to be used can be gives as value of "predict.function" 
#  argument. 
# in.min.max.limits: a matrix with one row per output and two columns, where the first column indicates 
#   the minimal value and the second column the maximal value for that output. 
# mx2 matrix of min-max values of outputs
# predict.function: can be supplied if a model that is not supported by ciu should be used.
#   As an example, this is the function for lda: 
#   o.predict.function <- function(model, inputs) { 
#     pred <- predict(model,inputs) 
#     return(pred$posterior)
#   }
# "montecarlo.samples" is the number of random values to use for estimating CI and CU.
# Returns: mx2 matrix with CI, CU for all outputs
# Might be useful also to return the estimated minimal and maximal values found. Would then be mx2 matrix again
ciu.new <- function(bb, in.min.max.limits=NULL, abs.min.max=NULL, 
                    input.names=NULL, output.names=NULL, predict.function=NULL, 
                    train.inputs=NULL, train.targets=NULL) {
  o.model <- bb
  t.in <- train.inputs
  t.target <- train.targets
  o.absminmax <- abs.min.max
  o.input.names <- input.names
  o.outputnames <- output.names
  n.mc.samples <- NULL 
  o.in.minmax <- in.min.max.limits # Would be better to name all instance variable "o." or similar. Next time again!

  # Set prediction function according to parameter or to model type. 
  o.predict.function <- predict.function
  if ( is.null(o.predict.function) ) {
    if ( inherits(o.model, "FunctionApproximator") ) {
      o.predict.function <- function(model, inputs) { model$eval(inputs) }
    }
    else if ( inherits(o.model, "train") ) { # caret
      o.predict.function <- function(model, inputs) { predict(model, inputs, type="prob") }
    }
    else {
      # This works at least with "lda" model, don't know with which other ones.
      o.predict.function <- function(model, inputs) { 
        pred <- predict(model,inputs) 
        return(pred$posterior)
      }
    }
  }
  
  ciu <- NULL
  
  # If no absmin/max matrix is given, then get it from train.targets, if provided.
  if ( is.null(o.absminmax)  && !is.null(t.target)) {
    target.mins <- apply(t.target, 2, min)
    target.maxs <- apply(t.target, 2, max)
    o.absminmax <- matrix(c(target.mins, target.maxs), ncol=2)
  }
  
  # If no min-max limits given as parameter, then we get them from train.inputs parameter, if available. 
  if ( is.null(o.in.minmax) && !is.null(train.inputs) ) {
    in.mins <- apply(t.in, 2, min)
    in.maxs <- apply(t.in, 2, max)
    o.in.minmax <- matrix(c(in.mins, in.maxs), ncol=2)
  }

  # Calculate Contextual Importance (CI) and Contextual Utility (CU) for the "black-box" model.
  # Arguments:
  # inputs: the current input values for the instance to explain.
  # ind.inputs.to.explain: vector of indices for the inputs to be included in the plot. 
  #   If NULL then all inputs will be included.
  # in.min.max.limits: a matrix with one row per output and two columns, where the first column indicates 
  #   the minimal value and the second column the maximal value for that output. ONLY NEEDED HERE IF 
  #   not given as parameter to "new" or if the limits are different for this specific instance than 
  #   the default. 
  # montecarlo.samples: how many random instances to use for estimating CI and CU.
  # Returns: mx2 matrix with CI, CU for all outputs. 
  #
  # Might be useful also to return the estimated minimal and maximal values found. 
  # Would then be mx2 matrix again, or adding two more columns. Then again, they could be stored with the object and 
  # retrieved separately with other method. 
  explain <- function(inputs, ind.inputs.to.explain, in.min.max.limits=NULL, montecarlo.samples=100) {
    last.inputs <<- inputs
    last.explained.inp.inds <<- ind.inputs.to.explain
    if ( is.null(in.min.max.limits) ) 
       in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")
    n.mc.samples <<- montecarlo.samples
    
    # Create matrix of inputs using the provided values, replacing the indicated columns with random values.
    nbr.mc.cols <- length(ind.inputs.to.explain)
    
    # Create random values for the desired columns.
    # Special treatment for [0,1] values, makes it more efficient. 
    if ( is.null(in.min.max.limits) ) {
      rvals <- matrix(runif(n.mc.samples*nbr.mc.cols), nrow=n.mc.samples)
    }
    else {
      # Different treatment required if various min-max ranges for inputs (not [0,1]). 
      mins <- in.min.max.limits[ind.inputs.to.explain,1]
      diffs <- in.min.max.limits[ind.inputs.to.explain,2] - mins
      rvals <- matrix(mins, nrow=n.mc.samples, ncol=nbr.mc.cols, byrow=T) + 
        matrix(runif(n.mc.samples*nbr.mc.cols), nrow=n.mc.samples)*matrix(diffs, nrow=n.mc.samples, ncol=nbr.mc.cols, byrow=T)
    }
    
    # Many/most Machine Learning packages seem to use/require data.frame rather than matrix. 
    if ( is.data.frame(inputs)) { 
      mcm <- inputs[1,] # Initialize as data frame
      mcm[1:n.mc.samples,] <- inputs[1,]
    }
    else { # We try to go with ordinary matrix
      mcm <- matrix(inputs,ncol=length(inputs),nrow=n.mc.samples,byrow=TRUE)
    }
    mcm[,ind.inputs.to.explain] <- rvals
    
    # Evaluate output for all random values, as well as current output. 
    mcout <- as.matrix(o.predict.function(o.model, mcm)) # as.matrix for dealing with case of only one output.
    cu.val <- o.predict.function(o.model, inputs)
    minvals <- apply(mcout,2,min)
    range <- apply(mcout,2,max) - minvals
    output_ranges <- matrix(o.absminmax[,2] - o.absminmax[,1], ncol=1)
    CI <- range/output_ranges
    
    # Calculate CU.
    CU <- (cu.val - minvals)/range
    CU[is.na(CU)] <- 0 # If absmax-absmin was zero
    CU[is.infinite(CU)] <- 0 # If range was zero
    
    # Finalize the return matrix (maybe data.frame in future?)
    #ciu <- data.frame(CI=CI, CU=CU)
    ciu <- matrix(c(CI,CU), ncol=2)
    colnames(ciu) <- c("CI", "CU")
    if ( !is.null(o.outputnames) )
      rownames(ciu) <- o.outputnames
    return(ciu)
  }
  
  # Function for plotting out the effect of changing values of one input on one output
  # Arguments:
  # etc:
  plot.CI.CU <- function(inputs, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab="x", ylab="y", ylim=NULL, ...) {
    # Check and set up minimum/maximum limits for inputs
    if ( is.null(in.min.max.limits) ) 
      in.min.max.limits <- o.in.minmax[ind.input,]
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'plot.CI.CU'")
    in.min <- in.min.max.limits[1]
    in.max <- in.min.max.limits[2]
    interv <- (in.max - in.min)/n.points
    xp <- seq(in.min,in.max,interv)
    if ( is.null(dim(inputs)) )
      n.col <- length(inputs)
    else
      n.col <- ncol(inputs)
    if ( is.data.frame(inputs)) { 
      m <- inputs[1,] # Initialize as data frame
      m[1:length(xp),] <- inputs[1,]
    }
    else {
      m <- matrix(inputs, ncol=n.col, nrow=length(xp), byrow=T)
    }
    m[,ind.input] <- xp
    yp <- o.predict.function(o.model, m)
    cu.val <- o.predict.function(o.model, inputs)

    # Set up plot parameters
    if ( is.null(ylim) ) ylim <- o.absminmax[ind.output,]
    inp.names <- o.input.names
    if ( is.null(inp.names) ) 
      inp.names <- colnames(inputs)
    if ( !is.null(inp.names) )
      in.name <- inp.names[ind.input]
    else
      in.name <- paste("Input value", ind.input)
    if ( !is.null(o.outputnames) ) 
      outname <- o.outputnames[ind.output]
    else
      outname <- "Z"
    if ( is.null(xlab) ) {
      xlab <- in.name
    }
    if ( is.null(ylab) ) {
      ylab <- "Output value"
    }
    if ( is.null(main) ) {
      main <- outname
    }
    
    # Create plot, show current value
    plot(xp, yp[,ind.output], type='l', ylim=ylim, main=main, xlab=xlab, ylab=ylab, ...)
    points(inputs[ind.input], cu.val[ind.output], col = "red", pch = 16, cex = 2)
  }
  
  # Function for 3D plotting the effect of changing values of two inputs on one output
  # Arguments:
  # ...
  # n.points: How many x/y values for the plot between in.mins and in.maxs.
  # etc:
  plot.CI.CU.3D <- function(inputs, ind.inputs, ind.output, in.min.max.limits=NULL, n.points=40, 
                            main=NULL, xlab=NULL, ylab=NULL, zlab=NULL, zlim=NULL, ...) {
    if ( is.null(in.min.max.limits) ) 
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")
    in.mins <- in.min.max.limits[,1]
    in.maxs <- in.min.max.limits[,2]
    interv <- (in.maxs[ind.inputs] - in.mins[ind.inputs])/n.points
    xp <- seq(in.mins[ind.inputs[1]], in.maxs[ind.inputs[1]], by=interv[1])
    yp <- seq(in.mins[ind.inputs[2]], in.maxs[ind.inputs[2]], by=interv[2])
    l <- list(xp,yp)
    pm <- create.permutation.matrix(l)
    if ( is.null(dim(inputs)) )
      n.col <- length(inputs)
    else
      n.col <- ncol(inputs)
    if ( is.data.frame(inputs)) { 
      m <- inputs[1,] # Initialize as data frame
      m[1:nrow(pm),] <- inputs[1,]
    }
    else {
      m <- matrix(inputs, ncol=n.col, nrow=nrow(pm), byrow=T)
    }
    m[,ind.inputs[1]] <- pm[,1]
    m[,ind.inputs[2]] <- pm[,2]
    z <- o.predict.function(o.model, m)
    cu.val <- o.predict.function(o.model, inputs)
    zm <- matrix(z[,ind.output], nrow = length(xp), byrow = TRUE)

    # Set up plot labels, limits, ...
    inp.names <- o.input.names
    if ( is.null(inp.names) ) 
      inp.names <- colnames(inputs)
    if ( !is.null(inp.names) ) {
      x.name <- inp.names[ind.inputs[1]]
      y.name <- inp.names[ind.inputs[2]]
    }
    else {
      x.name <- paste("Input ", ind.inputs[1])
      y.name <- paste("Input ", ind.inputs[2])
    }
    if ( !is.null(o.outputnames) ) 
      outname <- o.outputnames[ind.output]
    else
      outname <- paste("Output ", ind.output)
    if ( is.null(main) ) main <- outname
    if ( is.null(xlab) ) xlab <- x.name
    if ( is.null(ylab) ) ylab <- y.name
    if ( is.null(zlab) ) zlab <- "Output value"
    if ( is.null(zlim) ) zlim <- o.absminmax[ind.output,]
    vt <- persp(xp, yp, zm, xlab=xlab, ylab=ylab, zlab=zlab, zlim=zlim, main=main, ticktype = "detailed", ...) # persp3D might want these: , bg="white", colvar=NULL, col="black", facets=FALSE
    
    # Show where current instance is located
    x.plot <- as.numeric(inputs[ind.inputs[1]])
    y.plot <- as.numeric(inputs[ind.inputs[2]])
    z.plot <- as.numeric(cu.val[ind.output])
    points(trans3d(x.plot, y.plot, z.plot, pmat = vt), col = "red", pch = 16, cex = 3)
  }
  
  # Create a barplot showing CI as the length of the bar and CU as more or less dark tones 
  # of green and red, for the given inputs and the given output. 
  # Arguments:
  # inputs: the current input values for the instance to explain/plot.
  # ind.inputs: vector of indices for the inputs to be included in the plot. If NULL then all inputs will be included.
  # ind.output: index of output to be explained.
  # in.min.max.limits: a matrix with one row per output and two columns, where the first column indicates 
  #   the minimal value and the second column the maximal value for that output. ONLY NEEDED
  #   if they were not provided to "new" or if they need to be different for this instance.
  # montecarlo.samples: how many random instances to use for estimating CI and CU.
  # neutral.CU: indicates when the Contextual Utility is considered 
  #   to be "negative". The default value of 0.5 seems quite logical for most cases.
  # xlim: X-axis is for CI, so it is c(0,1) by default, if xlim=NULL.
  barplot.CI.CU <- function(inputs, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, 
                            montecarlo.samples=100, neutral.CU=0.5, xlab=NULL, xlim=NULL, ...) {
    # If no input indices are given, then use all inputs
    if ( is.null(ind.inputs) ) {
      if ( is.null(dim(inputs)) ) {
        ind.inputs <- 1:length(inputs)
      }
      else {
        ind.inputs <- 1:ncol(inputs)
      }
    }

    # Use default limits if they are not given explicitly. 
    if ( is.null(in.min.max.limits) ) 
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")
    
    # We have to get CI/CU one input at a time, so have to do it as a loop. 
    n.inps.included <- length(ind.inputs)
    ci.cu <- matrix(0, nrow=n.inps.included, ncol=2) # Initiallize CI/CU matrix
    for ( inp in 1:n.inps.included ) {
      cius <- explain(inputs, ind.inputs.to.explain=c(inp), 
                      in.min.max.limits=in.min.max.limits, montecarlo.samples=montecarlo.samples)
      ci.cu[inp,] <- as.numeric(cius[ind.output,])
    }
    
    # Limit everything to [0,1]
    ci.cu[ci.cu > 1] <- 1
    ci.cu[ci.cu < 0] <- 0
    
    # We get error otherwise...
    CIs <- as.numeric(ci.cu[,1])
    CUs <- as.numeric(ci.cu[,2])
    
    # Get plotting values. Bar length corresponds to CI. 
    # Green bar for "positive CU", red for "negative CU". 
    # Darker color the higher the abs(CU) value is. Should still fine-tune this.
    plus.min.CUs <- CUs - neutral.CU
    bar.heights <- CIs
    green.h <- 0.33
    red.h <- 0
    h.vals <- rep(green.h, length(CIs)) # Green by default
    neg.CUs <- (plus.min.CUs < 0)
    h.vals[neg.CUs] <- red.h
    bar.col <- hsv(h.vals, 1, 1 - abs(plus.min.CUs))
    
    # Labels for the bars. Haven't tested what happens if this is NULL, maybe still fine. 
    inp.names <- o.input.names
    if ( is.null(inp.names) ) 
      inp.names <- colnames(inputs)
    
    # Do bar plot. Limit X axis to 1 because that's normally the maximal CI value. 
    old.mai <- par(mai=c(0.8,1.2,0.4,0.2))
    if ( is.null(xlab) ) xlab <- "Contextual Importance"
    if ( is.null(xlim) ) xlim <- c(0,1)
    barplot(bar.heights,col=bar.col,names=inp.names,horiz=T,las=1, 
            main=rownames(cius)[ind.output], xlab=xlab, xlim=xlim, ...)
    par(mai=old.mai)
  }
   
  # Return list of "public" methods
  pub <- list(
    explain = function(inputs, ind.inputs.to.explain, in.min.max.limits=NULL, montecarlo.samples=100) { 
      explain(inputs, ind.inputs.to.explain, in.min.max.limits, montecarlo.samples)
    },
    plot.CI.CU = function(inputs, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab=NULL, ylab=NULL, ylim=NULL, ...) {
      plot.CI.CU (inputs, ind.input, ind.output, in.min.max.limits, n.points, main, xlab, ylab, ylim, ...)
    }, 
    plot.CI.CU.3D = function(inputs, ind.inputs, ind.output, in.min.max.limits=NULL, n.points=40, 
                             main=NULL, xlab=NULL, ylab=NULL, zlab=NULL, zlim=NULL, ...) {
      plot.CI.CU.3D(inputs, ind.inputs, ind.output, in.min.max.limits, n.points, main, xlab, ylab, zlab, zlim, ...)
    },
    barplot.CI.CU = function(inputs, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, montecarlo.samples=100, neutral.CU=0.5, xlab=NULL, xlim=NULL, ...) {
      barplot.CI.CU(inputs, ind.inputs, ind.output, in.min.max.limits, montecarlo.samples, neutral.CU, xlab, xlim, ...)
    }
  )
  
  class(pub) <- c("CIU", class(pub))
  return(pub)
}

#=========================================================================
# After this comes development-time code, for testing etc.
#=========================================================================

# Call e.g. "adaline.three.inputs.test()".
# Or "adaline.three.inputs.test(indices=c(1,3))" for getting joint importance of inputs one and three. 
adaline.three.inputs.test <- function(inp=c(0.1,0.2,0.3), indices=c(1), n.samples=100) {
  a <- adaline.new(3, 1)
  inp <- c(0.1,0.2,0.3)
  w <- c(0.20,0.30,0.50)
  a$set.weights(matrix(w, nrow=1, byrow=T))
  ciu <- ciu.new(a, in.min.max.limits=matrix(c(0,1,0,1),nrow=2,byrow=T), abs.min.max=matrix(c(0, 1), nrow=1, byrow=T))
  CI.CU <- ciu$explain(inp, ind.inputs.to.explain=indices)
  CI.CU
}

## Two outputs
# Call e.g. "adaline.two.outputs.test()"
# Or "adaline.two.outputs.test(indices=c(1,3))" for getting joint importance of inputs one and three. 
adaline.two.outputs.test <- function(inp=c(0.1,0.2,0.3), indices=c(1), n.samples=100) {
  a <- adaline.new(3, 2)
  w <- matrix(c(0.20,0.30,0.50,0.25,0.35,0.40), nrow=2, byrow=TRUE)
  a$set.weights(w)
  #out2 <- a2$eval(inp2)
  ciu <- ciu.new(a, in.min.max.limits=matrix(c(0,1,0,1,0,1),nrow=3,byrow=T), abs.min.max=matrix(c(0,1,0,1), nrow=2, byrow=T))
  CI.CU <- ciu$explain(inp, ind.inputs.to.explain=indices)
  CI.CU
}




