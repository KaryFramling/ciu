# "R" implementation of Contextual Importance and Utility.
#
# Kary Främling, created in 2019
#

require("graphics")

source("Interfaces.R")
source("Functions.R")

# Create CIU object. 
# Returns: the created ciu object.
# Arguments: 
# bb: "black-box" object. Must be an object that inherits "FunctioApproximator" (implements "eval" method) 
#   or some other supported model. At least "rf" model of caret-library and "lda" model 
#  from MASS are supported. It might be that just about all caret models work directly 
#  also, as well as all models that have a similar "predict" method as lda.
#  Otherwise, the prediction function to be used can be gives as value of "predict.function" 
#  argument. A more powerful way is to inherit from FunctionApproximator class and 
#  implement an "eval" method.
# in.min.max.limits: a matrix with one row per output and two columns, where the first column indicates 
#   the minimal value and the second column the maximal value for that output. 
# abs.min.max: matrix of min-max values of outputs, one row per output, two columns (min, max).
# input.names: labels of inputs. 
# output.names: labels of outputs.
# predict.function: can be supplied if a model that is not supported by ciu should be used.
#   As an example, this is the function for lda: 
#   o.predict.function <- function(model, inputs) { 
#     pred <- predict(model,inputs) 
#     return(pred$posterior)
#   }
# train.inputs: if some other arguments are missing, then some information might be 
#   extracted from this, such as "in.min.max.limits" and "input.names".
# train.targets: if some other arguments are missing, then some information might be 
#   extracted from this, such as "abs.min.max" and "output.names".
# vocabulary (NOT IMPLEMENTED YET): list of labels/concepts to be used when producing explanations and what combination 
#   of inputs they correspond to. Example of two intermediate concepts and a higher-level one that combines them: 
#   list(intermediate.concept1=c(1,2,3), intermediate.concept2=c(4,5), higher.level.concept=c(1,2,3,4,5))
# TO BE IMPLEMENTED
# use.absminmax=T: if set to FALSE, then perform estimation of the [absmin,absmax] interval using the 
#   given bb model. Then in.min.max.limits has to be provided. abs.min.max may be provided but will 
#   not be used by default. 
# montecarlo.samples=1000: how many random instances to use for estimating [absmin,absmax]. 
# 
ciu.new <- function(bb, in.min.max.limits=NULL, abs.min.max=NULL, 
                    input.names=NULL, output.names=NULL, predict.function=NULL, 
                    train.inputs=NULL, train.targets=NULL, vocabulary=NULL) {
  o.model <- bb
  t.in <- train.inputs
  t.target <- train.targets
  o.absminmax <- abs.min.max
  o.input.names <- input.names
  o.outputnames <- output.names
  n.mc.samples <- NULL 
  o.in.minmax <- in.min.max.limits # Would be better to name all instance variable "o." or similar. Next time again!
  o.vocabulary <- vocabulary
  
  # Set prediction function according to parameter or to model type. 
  o.predict.function <- predict.function
  if ( is.null(o.predict.function) ) {
    if ( inherits(o.model, "FunctionApproximator") ) {
      o.predict.function <- function(model, inputs) { model$eval(inputs) }
      # We have to do extra check here for RBF since support for formula was introduced
      if ( inherits(o.model, "RBF") ) {
        if ( !is.null(o.model$get.formula()) )
          o.predict.function <- function(model, inputs) { predict.rbf(model, inputs) }
      }
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
  
  # If no min-max limits given as parameter, then we get them from train.inputs parameter, 
  # if available. 
  if ( is.null(o.in.minmax) && !is.null(train.inputs) ) {
    in.mins <- apply(t.in, 2, min)
    in.maxs <- apply(t.in, 2, max)
    o.in.minmax <- matrix(c(in.mins, in.maxs), ncol=2)
  }
  
  # If no input.names names given, then attempt to get them from train.inputs
  if ( is.null(o.input.names) && !is.null(t.in) )
    o.input.names <- names(t.in) # Shouldn't give worse result than NULL
  
  # If no output.names given, then attempt to get them from train.inputs
  if ( is.null(o.outputnames) && !is.null(t.target) )
    o.outputnames <- names(t.target) # Shouldn't give worse result than NULL
  
  # Calculate Contextual Importance (CI) and Contextual Utility (CU) for the "black-box" model.
  # Returns: mx4 matrix with CI, CU, Cmin, Cmax for all outputs. 
  # Arguments:
  # inputs: the current input values for the instance to explain.
  # ind.inputs.to.explain: vector of indices for the inputs to be included in the plot. 
  #   If NULL then all inputs will be included.
  # in.min.max.limits: a matrix with one row per output and two columns, where the first column indicates 
  #   the minimal value and the second column the maximal value for that output. ONLY NEEDED HERE IF 
  #   not given as parameter to "new" or if the limits are different for this specific instance than 
  #   the default. 
  # montecarlo.samples: how many random instances to use for estimating CI and CU.
  # target.concept: if provided, then calculate CIU of inputs ind.inputs.to.explain relative to the given 
  #   concept rather than relative to the actual output(s). "ind.inputs.to.explain" should normally be 
  #   a subset (or all) of the inputs that "target.concept" consists of - but that is not required 
  #   by the calculation. If a "target.CI.CU" is provided, then the "target.concept" doesn't have to 
  #   be included in the ciu vocabulary (at least for the moment).
  # target.CI.CU: CIU structure like the one returned by "explain" method, calculated for "target.concept". 
  #   If a "target.concept" is provided but no "target.CI.CU", then "target.CI.CU" is estimated by 
  #   a call to "explain" with the "montecarlo.samples" value given as argument. It may be useful 
  #   to provide a "target.CI.CU" value if it should be estimated using some other (typically greater)
  #   value for "montecarlo.samples" than the default one, or if it has already been calculated for 
  #   some reason. 
  #
  explain <- function(inputs, ind.inputs.to.explain, in.min.max.limits=NULL, montecarlo.samples=100, 
                      target.concept=NULL, target.CI.CU=NULL) {
    last.inputs <<- inputs
    last.explained.inp.inds <<- ind.inputs.to.explain
    if ( is.null(in.min.max.limits) ) 
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")
    n.mc.samples <<- montecarlo.samples
    
    # Check if "target.concept" has been provided and set absminmax accordingly.
    absminmax <- o.absminmax
    if ( !is.null(target.concept) ) {
      if ( is.null(target.CI.CU) ) {
        ind.inps <- o.vocabulary[target.concept][[1]]
        target.CI.CU <- explain(inputs, ind.inputs.to.explain=ind.inps, in.min.max.limits=in.min.max.limits, 
                                montecarlo.samples=montecarlo.samples) 
      }
      absminmax[,1:2] <- target.CI.CU[,3:4]
    }
    
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
    
    # Strange, in some case it's necessary to convert into vector.
    if ( ncol(rvals) == 1 )
      rvals <- as.vector(rvals)
    
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
    maxvals <- apply(mcout,2,max)
    range <- maxvals - minvals
    output_ranges <- matrix(absminmax[,2] - absminmax[,1], ncol=1)
    CI <- range/output_ranges
    
    # Calculate CU.
    CU <- (cu.val - minvals)/range
    CU[is.na(CU)] <- 0 # If absmax-absmin was zero
    CU[range==0] <- 0 # If range was zero
    
    # Finalize the return matrix (maybe data.frame in future?)
    #ciu <- data.frame(CI=CI, CU=CU)
    ciu <- matrix(c(CI, CU, minvals, maxvals), ncol=4)
    colnames(ciu) <- c("CI", "CU", "Cmin", "Cmax")
    if ( !is.null(o.outputnames) )
      rownames(ciu) <- o.outputnames
    return(ciu)
  }
  
  # explain.vocabulary
  # CI.CU for "named concepts" that can be combinations of several inputs. 
  # Returns: a list with one element per named concept. Each element contains the CI.CU 
  # result given by "explain" method for the named concept.
  # Arguments:
  # concepts.to.explain: list of strings with the names of the concepts to be explained. 
  # Other arguments are the same as for "explain" method. 
  # QUESTION: where to put the "relative.to" option, i.e. relative to higher-level concept 
  # other than the "whole output"? Or maybe better to give option to provide abs.minmax as 
  # optional argument to "explain"? Then it would probably be useful to add Cmin and Cmax to 
  # the matrix returned by "explain" so that it can be retrieved and re-used by the application. 
  explain.vocabulary <- function(inputs, concepts.to.explain, in.min.max.limits=NULL, montecarlo.samples=1000, 
                                 target.concept=NULL, target.CI.CU=NULL) {
    # Go through concepts.to.explain one by one
    res <- vector("list", length(concepts.to.explain))
    names(res) <- concepts.to.explain
    for ( i in 1:length(concepts.to.explain) ) {
      # Get ind.inputs.to.explain from o.vocabulary for concept
      ind.inputs <- o.vocabulary[concepts.to.explain[i]][[1]]
      res[[i]] <- explain(inputs=inputs, ind.inputs.to.explain=ind.inputs, in.min.max.limits=in.min.max.limits, 
                          montecarlo.samples=montecarlo.samples, target.concept=target.concept, target.CI.CU=target.CI.CU)
    }
    return(res)
  }
  
  # Function for plotting out the effect of changing values of one input on one output
  # Returns: "void", or whatever happens to be result of last instruction. 
  # Arguments:
  # inputs: the current input values for the instance to explain.
  # ind.input: index of input to plot (X-axis).
  # ind.output: index of input to plot (Y-axis).
  # in.min.max.limits: same as for "explain" method.
  # n.points: how many x,y pairs will be calculated, equally spaced over in.min.max.limits. 
  # remaining: plot parameters.
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
    yp <- as.matrix(o.predict.function(o.model, m)) # as.matrix to deal with case of only one output
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
  # Returns: "void", or whatever happens to be result of last instruction. 
  # Arguments:
  # inputs: the current input values for the instance to explain.
  # ind.inputs: indices of inputs to plot (X- and Y-axis).
  # ind.output: index of input to plot (Z-axis).
  # in.min.max.limits: same as for "explain" method.
  # n.points: How many x/y values for the plot between in.mins and in.maxs.
  # remaining: plot parameters.
  #
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
  
  # Create a barplot showing CI as the length of the bar and CU on color scale from 
  # red to green, via yellow, for the given inputs and the given output. 
  # Returns: "void", or whatever happens to be result of last instruction. 
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
  # show.input.values: include input values after input labels or not. Default is TRUE. 
  # concepts.to.explain: list of concepts to use in the plot, as defined by vocabulary provided
  #   as argument to ciu.new. if "ind.inputs=NULL", then use "concepts.to.explain" instead. If both
  #   are NULL, then use all inputs. 
  # color.ramp.below.neutral: color ramp function as returned by function colorRamp(). Default color 
  #   ramp is from red3 to yellow. 
  # color.ramp.above.neutral: color ramp function as returned by function colorRamp(). Default color 
  #   ramp is from yellow to darkgreen. 
  # sort: NULL, "CI" or "CU". No sorting by default, other options are sorting by CI or CU.
  # decreasing=FALSE: set to TRUE for decreasing sort (see arguments of sort()).
  # Other arguments are the same as for "explain" method. 
  # main, xlab, xlim etc: usual plot parameters, possible to override the default ones provided
  #   here if needed.
  barplot.CI.CU <- function(inputs, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, 
                            montecarlo.samples=100, neutral.CU=0.5, 
                            show.input.values=TRUE, concepts.to.explain=NULL, 
                            target.concept=NULL, target.CI.CU=NULL,
                            color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                            sort=NULL, decreasing=FALSE, 
                            main=NULL, xlab=NULL, xlim=NULL, ...) {
    # If no input indices are given, then use all inputs
    explain.concepts <- FALSE
    if ( is.null(ind.inputs) ) {
      if ( !is.null(concepts.to.explain) ) {
        explain.concepts <- TRUE
      }
      else {
        if ( is.null(dim(inputs)) ) {
          ind.inputs <- 1:length(inputs)
        }
        else {
          ind.inputs <- 1:ncol(inputs)
        }
      }
    }
    
    # Use default limits if they are not given explicitly. 
    if ( is.null(in.min.max.limits) ) 
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")
    
    # Set colorRamps to use.
    cols.below <- ifelse ( is.null(color.ramp.below.neutral), colorRamp(c("red3", "yellow")), color.ramp.below.neutral)
    cols.above <- ifelse ( is.null(color.ramp.above.neutral), colorRamp(c("yellow","darkgreen")), color.ramp.above.neutral)
    
    # We have to get CI/CU one input at a time, so have to do it as a loop. 
    n.inps.included <- ifelse(explain.concepts, length(concepts.to.explain), length(ind.inputs))
    ci.cu <- matrix(0, nrow=n.inps.included, ncol=2) # Initialize CI/CU matrix
    if ( explain.concepts ) {
      cius <- explain.vocabulary(inputs, concepts.to.explain=concepts.to.explain, in.min.max.limits=in.min.max.limits, 
                                 montecarlo.samples=montecarlo.samples,target.concept=target.concept, target.CI.CU=target.CI.CU)
      for ( inp in 1:length(cius) ) {
        ci.cu[inp,] <- as.numeric(cius[[inp]][ind.output,1:2])
      }
    }
    else {
      for ( inp in 1:n.inps.included ) {
        cius <- explain(inputs, ind.inputs.to.explain=c(ind.inputs[inp]), in.min.max.limits=in.min.max.limits, 
                        montecarlo.samples=montecarlo.samples,target.concept=target.concept, target.CI.CU=target.CI.CU)
        ci.cu[inp,] <- as.numeric(cius[ind.output,1:2])
      }
    }
    
    # Limit everything to [0,1]
    ci.cu[ci.cu > 1] <- 1
    ci.cu[ci.cu < 0] <- 0
    
    # We get error otherwise...
    CIs <- as.numeric(ci.cu[,1])
    CUs <- as.numeric(ci.cu[,2])
    
    # Sort results?
    if ( !is.null(sort) ) {
      if ( sort=="CI") {
        s <- sort(CIs, decreasing=decreasing, index.return=TRUE)
      }
      else if ( sort=="CU" )  {
        s <- sort(CUs, decreasing=decreasing, index.return=TRUE)
      }
      else {
        stop("Argument 'sort' must be NULL, 'CI' or 'CU'.")
      }
      CIs <- CIs[s$ix]
      CUs <- CUs[s$ix]
    }
    
    # Get plotting values. Bar length corresponds to CI. 
    # Green bar for "positive CU", red for "negative CU". 
    # Darker color the higher the abs(CU) value is. Should still fine-tune this.
    bar.heights <- CIs # Simple for bar heights. More work for CU<->colors
    below <- CUs < neutral.CU; above <- CUs >= neutral.CU
    cols1 <- rgb(cols.below((CUs[below])*(1/neutral.CU))/255)
    cols2<-rgb(cols.above((CUs[above]-neutral.CU)*(1/(1-neutral.CU)))/255)
    bar.col <- c(cols1, cols2) # Not right, just initialize. Not most elegant in the world...
    bar.col[below] <- cols1
    bar.col[above] <- cols2
    
    # Labels for the bars. Haven't tested what happens if this is NULL, maybe still fine. 
    if ( explain.concepts ) { # Explain using Intermediate concepts
      inp.names <- concepts.to.explain
    }
    else { # Explain using inputs directly
      inp.names <- o.input.names[ind.inputs]
      if ( is.null(inp.names) ) 
        inp.names <- colnames(inputs)
      # Add input values to input labels
      if ( show.input.values ) {
        for ( i in 1:length(inp.names) ) {
          inp.names[i] <- paste(inp.names[i], " (", format(inputs[ind.inputs[i]], digits=2), ")", sep="")
        }
      }
    }
    
    # Plot title
    main.title <- main
    if ( is.null(main.title) ) {
      main.title <- o.outputnames[ind.output]
      main.title <- paste(main.title, " (", format(o.model$eval(inputs)[ind.output], digits=2), ")", sep="")
      if ( !is.null(target.concept) ) {
        main.title <- paste(target.concept, "(", main.title, ")")
      }
    }
    
    # Do bar plot. Limit X axis to 1 because that's normally the maximal CI value. 
    old.mai <- par(mai=c(0.8,1.2,0.4,0.2))
    if ( is.null(xlab) ) xlab <- "Contextual Importance"
    if ( is.null(xlim) ) xlim <- c(0,1)
    barplot(bar.heights,col=bar.col,names=inp.names,horiz=T,las=1, 
            main=main.title, xlab=xlab, xlim=xlim, ...)
    par(mai=old.mai)
  }
  
  # Create a pie chart showing CI as the area of slice and CU on color scale from 
  # red to green, via yellow, for the given inputs and the given output. 
  # Returns: "void", or whatever happens to be result of last instruction. 
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
  # show.input.values: include input values after input labels or not. Default is TRUE. 
  # concepts.to.explain: list of concepts to use in the plot, as defined by vocabulary provided
  #   as argument to ciu.new. if "ind.inputs=NULL", then use "concepts.to.explain" instead. If both
  #   are NULL, then use all inputs. 
  # color.ramp.below.neutral: color ramp function as returned by function colorRamp(). Default color 
  #   ramp is from red3 to yellow. 
  # color.ramp.above.neutral: color ramp function as returned by function colorRamp(). Default color 
  #   ramp is from yellow to darkgreen. 
  # sort: NULL, "CI" or "CU". No sorting by default, other options are sorting by CI or CU.
  # decreasing=FALSE: set to TRUE for decreasing sort (see arguments of sort()).
  # Other arguments are the same as for "explain" method. 
  # main, xlab, xlim etc: usual plot parameters, possible to override the default ones provided
  #   here if needed.
  pie.CI.CU <- function(inputs, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, 
                        montecarlo.samples=100, neutral.CU=0.5, 
                        show.input.values=TRUE, concepts.to.explain=NULL, 
                        target.concept=NULL, target.CI.CU=NULL,
                        color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                        sort=NULL, decreasing=FALSE,
                        main=NULL, ...) {
    # If no input indices are given, then use all inputs
    explain.concepts <- FALSE
    if ( is.null(ind.inputs) ) {
      if ( !is.null(concepts.to.explain) ) {
        explain.concepts <- TRUE
      }
      else {
        if ( is.null(dim(inputs)) ) {
          ind.inputs <- 1:length(inputs)
        }
        else {
          ind.inputs <- 1:ncol(inputs)
        }
      }
    }
    
    # Use default limits if they are not given explicitly. 
    if ( is.null(in.min.max.limits) ) 
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")
    
    # Set colorRamps to use.
    cols.below <- ifelse ( is.null(color.ramp.below.neutral), colorRamp(c("red3", "yellow")), color.ramp.below.neutral)
    cols.above <- ifelse ( is.null(color.ramp.above.neutral), colorRamp(c("yellow","darkgreen")), color.ramp.above.neutral)
    
    # We have to get CI/CU one input at a time, so have to do it as a loop. 
    n.inps.included <- ifelse(explain.concepts, length(concepts.to.explain), length(ind.inputs))
    ci.cu <- matrix(0, nrow=n.inps.included, ncol=2) # Initialize CI/CU matrix
    if ( explain.concepts ) {
      cius <- explain.vocabulary(inputs, concepts.to.explain=concepts.to.explain, in.min.max.limits=in.min.max.limits, 
                                 montecarlo.samples=montecarlo.samples,target.concept=target.concept, target.CI.CU=target.CI.CU)
      for ( inp in 1:length(cius) ) {
        ci.cu[inp,] <- as.numeric(cius[[inp]][ind.output,1:2])
      }
    }
    else {
      for ( inp in 1:n.inps.included ) {
        cius <- explain(inputs, ind.inputs.to.explain=c(ind.inputs[inp]), in.min.max.limits=in.min.max.limits, 
                        montecarlo.samples=montecarlo.samples,target.concept=target.concept, target.CI.CU=target.CI.CU)
        ci.cu[inp,] <- as.numeric(cius[ind.output,1:2])
      }
    }
    
    # Limit everything to [0,1]
    ci.cu[ci.cu > 1] <- 1
    ci.cu[ci.cu < 0] <- 0
    
    # We get error otherwise...
    CIs <- as.numeric(ci.cu[,1])
    CUs <- as.numeric(ci.cu[,2])
    
    # Sort results?
    if ( !is.null(sort) ) {
      if ( sort=="CI") {
        s <- sort(CIs, decreasing=decreasing, index.return=TRUE)
      }
      else if ( sort=="CU" )  {
        s <- sort(CUs, decreasing=decreasing, index.return=TRUE)
      }
      else {
        stop("Argument 'sort' must be NULL, 'CI' or 'CU'.")
      }
      CIs <- CIs[s$ix]
      CUs <- CUs[s$ix]
    }
    
    # Get plotting values. Pie size corresponds to CI. 
    # Green color for "positive CU", red for "negative CU". 
    bar.heights <- CIs # Simple for bar heights. More work for CU<->colors
    below <- CUs < neutral.CU; above <- CUs >= neutral.CU
    cols1 <- rgb(cols.below((CUs[below])*(1/neutral.CU))/255)
    cols2<-rgb(cols.above((CUs[above]-neutral.CU)*(1/(1-neutral.CU)))/255)
    bar.col <- c(cols1, cols2) # Not right, just initialize. Not most elegant in the world...
    bar.col[below] <- cols1
    bar.col[above] <- cols2
    
    # Labels for the bars. Haven't tested what happens if this is NULL, maybe still fine. 
    if ( explain.concepts ) {
      inp.names <- concepts.to.explain
    }
    else {
      inp.names <- o.input.names[ind.inputs]
      if ( is.null(inp.names) ) 
        inp.names <- colnames(inputs)
      if ( show.input.values ) {
        for ( i in 1:length(inp.names) ) {
          inp.names[i] <- paste(inp.names[i], " (", inputs[ind.inputs[i]], ")", sep="")
        }
      }
    }
    
    # Plot title
    main.title <- main
    if ( is.null(main.title) ) {
      main.title <- o.outputnames[ind.output]
      main.title <- paste(main.title, " (", format(o.model$eval(inputs)[ind.output], digits=2), ")", sep="")
      if ( !is.null(target.concept) ) {
        main.title <- paste(target.concept, "(", main.title, ")")
      }
    }
    
    # Draw pie chart plot.  
    old.mai <- par(mai=c(0.8,1.2,0.4,0.2))
    pie(bar.heights,col=bar.col,labels=inp.names,
        main=main.title, ...)
    par(mai=old.mai)
  }
  
  # Return list of "public" methods
  pub <- list(
    explain = function(inputs, ind.inputs.to.explain, in.min.max.limits=NULL, montecarlo.samples=100,
                       target.concept=NULL, target.CI.CU=NULL) { 
      explain(inputs, ind.inputs.to.explain, in.min.max.limits, montecarlo.samples, target.concept, target.CI.CU)
    },
    explain.vocabulary = function(inputs, concepts.to.explain, in.min.max.limits=NULL, montecarlo.samples=1000, 
                                  target.concept=NULL, target.CI.CU=NULL) {
      explain.vocabulary(inputs, concepts.to.explain, in.min.max.limits, montecarlo.samples, 
                         target.concept, target.CI.CU)
    },
    plot.CI.CU = function(inputs, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab=NULL, ylab=NULL, ylim=NULL, ...) {
      plot.CI.CU (inputs, ind.input, ind.output, in.min.max.limits, n.points, main, xlab, ylab, ylim, ...)
    }, 
    plot.CI.CU.3D = function(inputs, ind.inputs, ind.output, in.min.max.limits=NULL, n.points=40, 
                             main=NULL, xlab=NULL, ylab=NULL, zlab=NULL, zlim=NULL, ...) {
      plot.CI.CU.3D(inputs, ind.inputs, ind.output, in.min.max.limits, n.points, main, xlab, ylab, zlab, zlim, ...)
    },
    barplot.CI.CU = function(inputs, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, montecarlo.samples=100, 
                             neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL, target.concept=NULL, target.CI.CU=NULL,
                             color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL, 
                             sort=NULL, decreasing=FALSE, 
                             main= NULL, xlab=NULL, xlim=NULL, ...) {
      barplot.CI.CU(inputs, ind.inputs, ind.output, in.min.max.limits, montecarlo.samples, neutral.CU, show.input.values,
                    concepts.to.explain, target.concept, target.CI.CU, color.ramp.below.neutral, color.ramp.above.neutral, 
                    sort, decreasing, main, xlab, xlim, ...)
    },
    pie.CI.CU = function(inputs, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, montecarlo.samples=100, 
                         neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL, target.concept=NULL, target.CI.CU=NULL,
                         color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL, 
                         sort=NULL, decreasing=FALSE,
                         main= NULL, ...) {
      pie.CI.CU(inputs, ind.inputs, ind.output, in.min.max.limits, montecarlo.samples, neutral.CU, 
                show.input.values, concepts.to.explain, target.concept, target.CI.CU, 
                color.ramp.below.neutral, color.ramp.above.neutral, 
                sort, decreasing, main, ...)
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

# Tests with vocabulary, intermediate concepts. 
vocabulary.test <- function() {
  a <- adaline.new(3, 2)
  w <- matrix(c(0.20,0.30,0.50,0.25,0.35,0.40), nrow=2, byrow=TRUE)
  a$set.weights(w)
  voc <- list(oneand2=c(1,2),twoand3=c(2,3),oneand3=c(1,3),one=c(1))
  ciu <- ciu.new(a, in.min.max.limits=matrix(c(0,1,0,1,0,1),nrow=3,byrow=T), abs.min.max=matrix(c(0,1,0,1), nrow=2, byrow=T), 
                 vocabulary=voc)
  inp <- c(0.1,0.2,0.3)
  CI.CU <- ciu$explain.vocabulary(inp, concepts.to.explain=c("oneand2","oneand3"), montecarlo.samples=1000)
  print(CI.CU)
}


