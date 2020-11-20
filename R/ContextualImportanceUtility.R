#"R" implementation of Contextual Importance and Utility.
#
# Pity, roxygen automatic documentation don't work for the "object methods",
# have to do that manually.
#
# Kary Främling, created in 2019
#

#' Create CIU object
#'
#' Sets up a CIU object with the given parameters. CIU objects have "public"
#' and "private" methods. A CIU object is actually a \link{list} whose elements
#' are the public functions (methods).
#'
#' @param bb Model/"black-box" object. At least all \code{caret} models, the
#' \code{lda} model from MASS, and the \code{lm} model are supported.
#' Otherwise, the prediction
#' function to be used can be gives as value of the \code{predict.function}
#' parameter.
#' A more powerful way is to inherit from FunctionApproximator class and
#' implement an "eval" method.
#' @param formula Formula that describes input versus output values. Only to
#' be used together with \code{data} parameter.
#' @param data The training data used for training the model. If this parameter
#' is provided, a \code{formula} MUST be given also.
#' \code{ciu.new} attempts to infer the other parameters from
#' \code{data} and \code{formula}. i.e. \code{in.min.max.limits},
#' \code{abs.min.max}, \code{input.names}
#' and \code{output.names}. If those parameters are provided, then they
#' override the inferred ones.
#' @param in.min.max.limits matrix with one row per output and two columns,
#' where the first column indicates the minimal value and the second column
#' the maximal value for that input.
#' @param abs.min.max \link{data.frame} or \link{matrix} of min-max values of
#' outputs, one row per output, two columns (min, max).
#' @param input.names labels of inputs.
#' @param output.names labels of outputs.
#' @param predict.function can be supplied if a model that is not supported by
#' ciu should be used. As an example, this is the function for lda:\preformatted{
#' o.predict.function <- function(model, inputs) \{
#'     pred <- predict(model,inputs)
#'         return(pred$posterior)
#' \}}
# @param train.inputs if some other parameters are missing, then some
# information might be extracted from this parameter, such as
# \code{in.min.max.limits} and \code{input.names}.
# @param train.targets if some other parameters are missing, then some
# information might be extracted from this, such as \code{abs.min.max} and
# \code{output.names}.
#' @param vocabulary list of labels/concepts to be used when producing
#' explanations and what combination of inputs they correspond to. Example of
#' two intermediate concepts and a higher-level one that combines them:
#' \code{list(intermediate.concept1=c(1,2,3), intermediate.concept2=c(4,5), higher.level.concept=c(1,2,3,4,5))}
#'
#' @return Object of class \code{CIU}.
#' @details CIU is implemented in an object-oriented manner, where a CIU
#' object is a \code{\link{list}} whose methods are made visible as
#' elements of the list. The general way for using `CIU` objects is to
#' first get a `CIU` object by calling \code{\link{ciu.new}} as e.g.
#' \code{ciu <- ciu.new(...)}, then call \code{ciu.res <- ciu$<method>(...)}.
#' The methods that can be used in `<method>` are:
#' - \code{\link{explain}}
#' - \code{\link{barplot.ciu}}
#' - \code{\link{ggplot.col.ciu}}
#' - \code{\link{pie.ciu}}
#' - \code{\link{plot.ciu}}
#' - \code{\link{plot.ciu.3D}}
#'
#' \emph{"Usage" section is here in "Details" section because Roxygen etc.
#' don't support documentation of functions within functions.}
#' @export
#' @import stats
#' @import graphics
#' @import grDevices
#' @import ggplot2
#' @references Främling, K. *Explainable AI without Interpretable Model*. 2020, <https://arxiv.org/abs/2009.13996>.
#' @references Främling, K. *Decision Theory Meets Explainable AI*. 2020, <https://doi.org/10.1007/978-3-030-51924-7_4>.
#' @references Främling, K. *Modélisation et apprentissage des préférences par réseaux de neurones pour l'aide à la décision multicritère*. 1996, <https://tel.archives-ouvertes.fr/tel-00825854/document> (title translation in English: *Learning and Explaining Preferences with Neural Networks for Multiple Criteria Decision Making*)
#' @examples
#' # Explaining the classification of an Iris instance with lda model.
#' # We use a versicolor (instance 100).
#' library(MASS)
#' test.ind <- 100
#' iris_test <- iris[test.ind, 1:4]
#' iris_train <- iris[-test.ind, 1:4]
#' iris_lab <- iris[[5]][-test.ind]
#' model <- lda(iris_train, iris_lab)
#'
#' # Create CIU object
#' ciu <- ciu.new(model, Species~., iris)
#'
#' # This can be used with explain method for getting CIU values
#' # of one or several inputs. Here we get CIU for all three outputs
#' # with input feature "Petal.Length" that happens to be the most important.
#' ciu$explain(iris_test, 1)
#'
#' # It is, however, more convenient to use one of the graphical visualisations.
#' # Here's one using ggplot.
#' ciu$ggplot.col.ciu(iris_test)
#'
#' # LDA creates very sharp class limits, which can also be seen in the CIU
#' # explanation. We can study what the underlying model looks like using
#' # plot.ciu and plot.ciu.3D methods. Here is a 3D plot for all three classes
#' # as a function of Petal Length&Width. Iris #100 (shown as the red dot)
#' # is on the ridge of the "versicolor" class, which is quite narrow for
#' # Petal Length&Width.
#' ciu$plot.ciu.3D(iris_test,c(3,4),1,main=levels(iris$Species)[1],)
#' ciu$plot.ciu.3D(iris_test,c(3,4),2,main=levels(iris$Species)[2])
#' ciu$plot.ciu.3D(iris_test,c(3,4),3,main=levels(iris$Species)[3])
#'
#' # Same thing with a regression task, the Boston Housing data set. Instance
#' # #370 has the highest valuation (50k$). Model is gbm, which performs
#' # decently here. Plotting with "standard" bar plot this time.
#' # Use something like "par(mai=c(0.8,1.2,0.4,0.2))" for seeing Y-axis labels.
#' library(caret)
#' gbm <- train(medv ~ ., Boston, method="gbm", trControl=trainControl(method="cv", number=10))
#' ciu <- ciu.new(gbm, medv~., Boston)
#' ciu$barplot.ciu(Boston[370,1:13])
#'
#' # Same but sort by CI.
#' ciu$barplot.ciu(Boston[370,1:13], sort = "CI")
#'
#' # The two other possible plots
#' ciu$ggplot.col(Boston[370,1:13])
#' ciu$pie.ciu(Boston[370,1:13])
#'
#' # Method "plot" for studying the black-box behavior and CIU one input at a time.
#' ciu$plot.ciu(Boston[370,1:13],13)
#'
#' @author Kary Främling
# # Remark: "lm" is a really bad model for Boston Housing data set! It gives
# # 32.6 as estimated price instead of 50. "gbm" or similar works much better
# # but we want to keep these examples short and not load too many libraries.
# ciu <- ciu.new(model, medv~., Boston)
# ciu$barplot.ciu(Boston[370,1:13])
ciu.new <- function(bb, formula=NULL, data=NULL, in.min.max.limits=NULL, abs.min.max=NULL,
                    input.names=NULL, output.names=NULL, predict.function=NULL,
                    vocabulary=NULL) {

  # Initialize default values and "instance variables"
  o.model <- bb
  o.formula <- formula
  o.data.inp <- NULL
  o.data.outp <- NULL
  o.absminmax <- abs.min.max
  o.inp.levels <- list()
  o.input.names <- input.names
  o.outputnames <- output.names
  o.last.n.samples <- NULL
  o.in.minmax <- in.min.max.limits
  o.vocabulary <- vocabulary
  o.last.instance <- NULL
  o.last.ciu <- NULL
  o.last.explained.inp.inds <- NULL

  # Deal with formula+data first. If one is missing, then ignore other.
  if ( !is.null(formula) && !is.null(data) ) {
    out.name <- formula[[2]] # We expect that output variable name is given and that there's only one

    # Extract output data and do needed operations on it.
    o.data.outp <- data[, names(data)==out.name, drop = FALSE]
    # If it's "character", then transform into "factor"
    if ( is.character(o.data.outp[,1]) )
      o.data.outp[,1] <- factor(o.data.outp[,1])
    # Deal with factor output
    if ( is.factor(o.data.outp[,1]) ) {
      o.outp.levels <- levels(o.data.outp[,1])
    }

    # Extract input data and do needed operations on it.
    o.data.inp <- data[, names(data)!=out.name]
    # Transform all "character" columns into "factor"
    for ( i in 1:ncol(o.data.inp) ) {
      if ( is.character(o.data.inp[,i]) )
        o.data.inp[,i] <- factor(o.data.inp[,i])
    }
    # Store factor input levels
    for ( i in 1:ncol(o.data.inp) ) {
      if ( is.factor(o.data.inp[,i]) ) {
        o.inp.levels[[i]] <- levels(o.data.inp[,i])
      }
    }
  }

  # Set prediction function according to parameter or to model type.
  o.predict.function <- predict.function
  if ( is.null(o.predict.function) ) {
    if ( inherits(o.model, "CIU.BlackBox") || inherits(o.model, "FunctionApproximator") ) {
      o.predict.function <- function(model, inputs) { model$eval(inputs) }
      # We have to do extra check here for RBF since support for formula was introduced
      # Commented away for the moment, doesn't pass package check without
      # "predict.rbf" visible and that would require the inka package.
      # if ( inherits(o.model, "RBF") ) {
      #   if ( !is.null(o.model$get.formula()) )
      #     o.predict.function <- function(model, inputs) { predict.rbf(model, inputs) }
      # }
    }
    else if ( inherits(o.model, "train") ) { # caret
      if ( o.model$modelType == "Regression" ) # Have to use different version of predict here.
        o.predict.function <- function(model, inputs) { predict(model, inputs) }
      else
        o.predict.function <- function(model, inputs) { predict(model, inputs, type="prob") }
    }
    else if ( inherits(o.model, "lm") ) { # lm
        o.predict.function <- function(model, inputs) { predict(model, inputs) }
    }
    else {
      # This works at least with "lda" model, don't know with which other ones.
      o.predict.function <- function(model, inputs) {
        pred <- predict(model,inputs)
        return(pred$posterior)
      }
    }
  }

  # If no absmin/max matrix is given, then get it from data, if provided.
  if ( is.null(o.absminmax)  && !is.null(o.data.outp)) {
    if ( is.factor(o.data.outp[,1]) ) {
      o.absminmax <- matrix(c(0,1), nrow=length(o.outp.levels), ncol=2, byrow=T)
    }
    else {
      mins <- apply(o.data.outp, 2, min)
      maxs <- apply(o.data.outp, 2, max)
      o.absminmax <- matrix(c(mins, maxs), ncol=2)
    }
  }

  # If no min-max limits given as parameter, then we get them from train.inputs parameter,
  # if available. Have to convert everything to numeric first to also deal with
  # factor inputs, as well as avoiding minmax values to become "character" type.
  if ( is.null(o.in.minmax) && !is.null(o.data.inp) ) {
    d <- sapply(o.data.inp, as.numeric)
    in.mins <- apply(d, 2, min)
    in.maxs <- apply(d, 2, max)
    o.in.minmax <- matrix(c(in.mins, in.maxs), ncol=2)
  }

  # If no input.names names given, then attempt to get them from train.inputs
  if ( is.null(o.input.names) && !is.null(o.data.inp) )
    o.input.names <- names(o.data.inp) # Shouldn't give worse result than NULL

  # If no output.names given, then attempt to get them from train.inputs
  if ( is.null(o.outputnames) && !is.null(o.data.outp) ) {
    if ( is.factor(o.data.outp[,1]) )
      o.outputnames <- levels(o.data.outp[,1])
    else
      o.outputnames <- names(o.data.outp) # Shouldn't give worse result than NULL
  }

  # Calculate CIU for specific instance
  #
  # Calculate Contextual Importance (CI) and Contextual Utility (CU) for a
  # instance (Context) using the given "black-box" model.
  #
  # @param instance the current input values for the instance to explain. Should
  # be a \link{data.frame} even though a \link{vector} or \link{matrix} might
  # work too if input names and other needed metadata can be deduced from the
  # dataset or other parameters given to \link{ciu::ciu.new}.
  # @param ind.inputs.to.explain \link{vector} of indices for the inputs to be
  # explained, i.e. for which CIU should be calculated. If NULL, then all
  # inputs will be included.
  # @param in.min.max.limits data.frame or matrix with one row per output and
  # two columns, where the first column indicates the minimal value and the
  # second column the maximal value for that output. ONLY NEEDED HERE IF not
  # given as parameter to \link{ciu::ciu.new} or if the limits are different
  # for this specific instance than the default ones.
  # @param n.samples how many instances to generate for estimating CI and CU.
  # For inputs of type \link{factor}, all possible combinations of input values
  # is generated, so this parameter only influences how many instances are
  # (at least) generated for continuous-valued inputs.
  # @param target.concept if provided, then calculate CIU of inputs
  # \code{ind.inputs.to.explain} relative to the given concept rather than
  # relative to the actual output(s). \code{ind.inputs.to.explain} should
  # normally be a subset (or all) of the inputs that \code{target.concept}
  # consists of, even though that not required by the CIU calculation.
  # If a "target.ciu" is provided, then the "target.concept" doesn't have to
  # be included in the \code{vocabulary} gives as parameter to
  # \link{ciu::ciu.new} (at least for the moment).
  # @param target.ciu \code{ciu.result} object previously calculated for
  # \code{target.concept}. Default value is NULL. If a \code{target.concept}
  # is provided but \code{target.ciu=NULL}, then \code{target.ciu} is
  # estimated by a call to "explain" with the \code{n.samples} value given as
  # a parameter to this call. It may be useful to provide \code{target.ciu}
  # if it should be estimated using some other (typically greater) value for
  # \code{n.samples} than the default one, or if it has already been calculated
  # for some reason.
  #
  # @return A \code{ciu.result} object (see \link{ciu.result.new})
  explain <- function(instance, ind.inputs.to.explain, in.min.max.limits=NULL, n.samples=100,
                      target.concept=NULL, target.ciu=NULL) {
    o.last.instance <<- instance
    o.last.explained.inp.inds <<- ind.inputs.to.explain
    o.last.n.samples <<- n.samples

    # absminmax has to be set. Stop here if they haven't been defined or
    # retrieved from data set or somehow otherwise been deduced.
    if ( is.null(o.absminmax) )
      stop("abs.min.max values have not been provided, nor retrieved from data set.")

    # Check if "target.concept" has been provided and set absminmax accordingly.
    absminmax <- o.absminmax
    if ( !is.null(target.concept) ) {
      if ( is.null(target.ciu) ) {
        ind.inps <- o.vocabulary[target.concept][[1]]
        target.ciu <- explain(instance, ind.inputs.to.explain=ind.inps, in.min.max.limits=in.min.max.limits,
                              n.samples=n.samples)
      }
      absminmax[,1] <- target.ciu$cmin
      absminmax[,2] <- target.ciu$cmax
    }

    # Here comes the generation of the Set of Representative Input Vectors
    ciu.eval.set <- create.ciu.input.set(instance, ind.inputs.to.explain,
                                         in.min.max.limits=in.min.max.limits, discrete.levels=NULL,
                                         n.samples=n.samples)

    # Evaluate output for all random values, as well as current output.
    mcout <- as.matrix(o.predict.function(o.model, ciu.eval.set)) # as.matrix for dealing with case of only one output.
    cu.val <- o.predict.function(o.model, instance)
    minvals <- apply(mcout,2,min)
    maxvals <- apply(mcout,2,max)
    range <- maxvals - minvals
    output_ranges <- matrix(absminmax[,2] - absminmax[,1], ncol=1)
    CI <- range/output_ranges

    # Calculate CU.
    CU <- (cu.val - minvals)/range
    CU[is.na(CU)] <- 0 # If absmax-absmin was zero
    CU[range==0] <- 0 # If range was zero

    # Finalize the return CIU object
    o.last.ciu <<- ciu.result.new(CI, CU, minvals, maxvals, as.numeric(cu.val))
    # if ( !is.null(o.outputnames) )
    #   rownames(o.last.ciu) <- o.outputnames
    return(o.last.ciu)
  }

  # Function for creating the "Set of Representative Input Vectors" for
  # estimating cmin and cmax.
  # Returns a data.frame with the requested number of rows (instances)
  create.ciu.input.set <- function(instance, ind.inputs.to.explain,
                                   in.min.max.limits=NULL, discrete.levels=NULL,
                                   n.samples=100) {

    # Initialize
    discrete.ciu.input.set <- NULL
    continuous.ciu.input.set <- NULL

    # Get indices of discrete inputs to explain.
    i.discrete <- sapply(instance, is.character) | sapply(instance, is.factor)
    i.inputs <- rep(0,length(instance)); i.inputs[ind.inputs.to.explain] <- TRUE
    ind.discrete.to.explain <- which(i.inputs & i.discrete)
    ind.continuous.to.explain <- which(i.inputs & !i.discrete)
    if ( length(ind.discrete.to.explain) > 0 ) {
      discrete.ciu.input.set <-
        create.discrete.ciu.input.set(instance, ind.discrete.to.explain)
      # Actual number of samples should be at least the size of the discrete
      # input sample set.
      n.samples <- max(n.samples, nrow(discrete.ciu.input.set))
    }

    # Deal with continuous-valued inputs.
    if ( length(ind.continuous.to.explain) > 0 ) {
      continuous.ciu.input.set <-
        create.continuous.ciu.input.set(instance, ind.continuous.to.explain,
                                        in.min.max.limits, n.samples)
    }

    # Combine if needed
    if ( is.null(discrete.ciu.input.set) )
      return(continuous.ciu.input.set)
    if ( is.null(continuous.ciu.input.set) )
      return(discrete.ciu.input.set)
    res <- continuous.ciu.input.set # This is the bigger (or same size) set so has to be done in this order.
    res[,ind.discrete.to.explain] <- discrete.ciu.input.set[,ind.discrete.to.explain]
    return(res)
  }

  # Create "Set of Representative Input Vectors" for the given continuous
  # inputs. This is currently done using Monte-Carlo simulation, after first
  # creating samples with the given min and max values of all the inputs.
  create.continuous.ciu.input.set <- function(instance, ind.inputs.to.explain,
                                              in.min.max.limits, n.samples) {

    # Necessary verifications
    if ( is.null(in.min.max.limits) )
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")

    # One of the samples should be current instance. Combine at the end,
    # for the moment just reduce n.samples by one.
    n.sampl.rem <- n.samples - 1

    # Then two samples per input to explain: min and max for that input.
    # With loop now, probably not so optimized. Furthermore, this could/should
    # be generalised for more than one input min/max combinations.
    n.minmax <- 2*length(ind.inputs.to.explain)
    min.max.samples <- instance[1,]
    min.max.samples[1:n.minmax,] <- instance[1,]
    r <- 1
    for ( i in 1:length(ind.inputs.to.explain) ) {
      min.max.samples[r,ind.inputs.to.explain[i]] <- in.min.max.limits[ind.inputs.to.explain[i],1]; r <- r+1
      min.max.samples[r,ind.inputs.to.explain[i]] <- in.min.max.limits[ind.inputs.to.explain[i],2]; r <- r+1
    }
    n.sampl.rem <- n.sampl.rem - n.minmax

    # Then four samples per every two inputs

    # Etc, until all the combinations of min-max input values of ind.inputs.to.explain

    # Create first samples using min-max input values
    #    minmax.rows <- expand.grid(in.min.max.limits[ind.inputs.to.explain,1], in.min.max.limits[ind.inputs.to.explain,2])
    #    n.samples <- n.samples - nrow(minmax.rows) # length(ind.inputs.to.explain)^2

    # Create matrix of inputs using the provided values, replacing the indicated columns with random values.
    nbr.cols <- length(ind.inputs.to.explain)

    # Create random values for the desired columns.
    # Special treatment for [0,1] values, makes it more efficient.
    if ( is.null(in.min.max.limits) ) {
      rvals <- matrix(runif(n.sampl.rem*nbr.cols), nrow=n.sampl.rem)
    }
    else {
      # Different treatment required if various min-max ranges for inputs (not [0,1]).
      mins <- in.min.max.limits[ind.inputs.to.explain,1]
      diffs <- in.min.max.limits[ind.inputs.to.explain,2] - mins
      rvals <- matrix(mins, nrow=n.sampl.rem, ncol=nbr.cols, byrow=T) +
        matrix(runif(n.sampl.rem*nbr.cols), nrow=n.sampl.rem)*
        matrix(diffs, nrow=n.sampl.rem, ncol=nbr.cols, byrow=T)
    }

    # Join together the different sets
    #    rvals <- rbind(minmax.rows, rvals)

    # Strange, in some case it's necessary to convert into vector.
    if ( ncol(rvals) == 1 )
      rvals <- as.vector(rvals)

    # Data frame here, skip alternative to use matrix..
    #if ( is.data.frame(instance)) {
    mcm <- instance[1,] # Initialize as data frame
    mcm[1:n.sampl.rem,] <- instance[1,]
    # }
    # else { # We try to go with ordinary matrix
    #   mcm <- matrix(inputs,ncol=length(inputs),nrow=o.last.n.samples,byrow=TRUE)
    # }
    mcm[,ind.inputs.to.explain] <- rvals

    # Join everything together
    res <- rbind(instance, min.max.samples, mcm)
    return(res)
  }

  # Create "Set of Representative Input Vectors" for the given discrete
  # inputs. This will generate a data.frame with all combinations of the
  # possible values for the given inputs. Rows are returned in randomized
  # order.
  # All "ind.inputs" columns of "data" parameter have to be of type
  # "character" or "factor".
  create.discrete.ciu.input.set <- function(instance, ind.inputs, data) {
    # We have to transform character type values into factors here.
    for ( i in 1:min(length(instance),length(o.inp.levels))) {
      if ( is.character(instance[,i]) ) {
        if ( is.null(o.inp.levels[[i]]) )
          instance[,i] <- factor(instance[,i])
        else
          instance[,i] <- factor(instance[,i], o.inp.levels[[i]])
      }
    }

    # Finally to the real work
    n.in <- length(ind.inputs)
    exp.cmd <- "expand.grid("
    if ( n.in > 1 )
      for ( i in 1:(n.in-1) )
        exp.cmd <- paste(exp.cmd, "levels(instance[[", ind.inputs[i], "]]),", sep="")
    exp.cmd <- paste(exp.cmd, "levels(instance[[", ind.inputs[n.in], "]]))", sep="")
    rep.ciu <- eval(str2expression(exp.cmd))
    names(rep.ciu) <- names(instance)[ind.inputs]
    reps <- rbind(instance,instance[rep(1,nrow(rep.ciu)-1),])
    reps[,ind.inputs] <- rep.ciu

    # Finally shuffle rows randomly
    rows <- sample(nrow(reps))
    reps <- reps[rows,]
    return(reps)
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
  plot.ciu <- function(instance, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab="x", ylab="y", ylim=NULL, ...) {
    # Check and set up minimum/maximum limits for inputs
    if ( is.null(in.min.max.limits) )
      in.min.max.limits <- o.in.minmax[ind.input,]
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'plot.ciu'")
    in.min <- in.min.max.limits[1]
    in.max <- in.min.max.limits[2]
    interv <- (in.max - in.min)/n.points
    xp <- seq(in.min,in.max,interv)
    if ( is.null(dim(instance)) )
      n.col <- length(instance)
    else
      n.col <- ncol(instance)
    if ( is.data.frame(instance)) {
      m <- instance[1,] # Initialize as data frame
      m[1:length(xp),] <- instance[1,]
    }
    else {
      m <- matrix(instance, ncol=n.col, nrow=length(xp), byrow=T)
    }
    m[,ind.input] <- xp
    yp <- as.matrix(o.predict.function(o.model, m)) # as.matrix to deal with case of only one output
    cu.val <- o.predict.function(o.model, instance)

    # Set up plot parameters
    if ( is.null(ylim) ) ylim <- o.absminmax[ind.output,]
    inp.names <- o.input.names
    if ( is.null(inp.names) )
      inp.names <- colnames(instance)
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
      main <- paste(main, " (", format(o.predict.function(o.model, instance)[ind.output], digits=2), ")", sep="")
    }

    # Create plot, show current value
    plot(xp, yp[,ind.output], type='l', ylim=ylim, main=main, xlab=xlab, ylab=ylab, ...)
    points(instance[ind.input], cu.val[ind.output], col = "red", pch = 16, cex = 2)
  }

  # Function for 3D plotting the effect of changing values of two inputs on one output
  # Returns: "void", or whatever happens to be result of last instruction.
  # Arguments:
  # instance: the current input values for the instance to explain.
  # ind.inputs: indices of inputs to plot (X- and Y-axis).
  # ind.output: index of input to plot (Z-axis).
  # in.min.max.limits: same as for "explain" method.
  # n.points: How many x/y values for the plot between in.mins and in.maxs.
  # remaining: plot parameters.
  #
  plot.ciu.3D <- function(instance, ind.inputs, ind.output=1, in.min.max.limits=NULL, n.points=40,
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
    pm <- expand.grid(xp, yp)
    if ( is.null(dim(instance)) )
      n.col <- length(instance)
    else
      n.col <- ncol(instance)
    if ( is.data.frame(instance)) {
      m <- instance[1,] # Initialize as data frame
      m[1:nrow(pm),] <- instance[1,]
    }
    else {
      m <- matrix(instance, ncol=n.col, nrow=nrow(pm), byrow=T)
    }
    m[,ind.inputs[1]] <- pm[,1]
    m[,ind.inputs[2]] <- pm[,2]
    z <- as.matrix(o.predict.function(o.model, m)) # as.matrix to deal with case of only one output
    cu.val <- o.predict.function(o.model, instance)
    zm <- matrix(z[,ind.output], nrow = length(xp), byrow = TRUE)

    # Set up plot labels, limits, ...
    inp.names <- o.input.names
    if ( is.null(inp.names) )
      inp.names <- colnames(instance)
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
    if ( is.null(main) ) {
      main <- outname
      main <- paste(main, " (", format(o.predict.function(o.model, instance)[ind.output], digits=2), ")", sep="")
    }
    if ( is.null(xlab) ) xlab <- x.name
    if ( is.null(ylab) ) ylab <- y.name
    if ( is.null(zlab) ) zlab <- "Output value"
    if ( is.null(zlim) ) zlim <- o.absminmax[ind.output,]
    vt <- persp(xp, yp, zm, xlab=xlab, ylab=ylab, zlab=zlab, zlim=zlim, main=main, ticktype = "detailed", ...) # persp3D might want these: , bg="white", colvar=NULL, col="black", facets=FALSE

    # Show where current instance is located
    x.plot <- as.numeric(instance[ind.inputs[1]])
    y.plot <- as.numeric(instance[ind.inputs[2]])
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
  # n.samples: how many random instances to use for estimating CI and CU.
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
  barplot.ciu <- function(instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL,
                          n.samples=100, neutral.CU=0.5,
                          show.input.values=TRUE, concepts.to.explain=NULL,
                          target.concept=NULL, target.ciu=NULL,
                          color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                          sort=NULL, decreasing=FALSE,
                          main=NULL, xlab=NULL, xlim=NULL, ...) {

    # Check if concepts are to be explained or pure inputs.
    # If no input indices are given, then use all inputs
    explain.concepts <- FALSE
    if ( !is.null(concepts.to.explain) ) {
      explain.concepts <- TRUE
      inp.names <- concepts.to.explain
    }
    else {
      if ( is.null(ind.inputs) ) {
        if ( is.null(dim(instance)) )
          ind.inputs <- 1:length(instance)
        else
          ind.inputs <- 1:ncol(instance)
      }
      inp.names <- o.input.names[ind.inputs]
      if ( is.null(inp.names) )
        inp.names <- names(instance)[ind.inputs]
    }
    n.inps <- length(inp.names)

    # Again, "instance" has to be a data.frame so this can't be NULL.
    inst.name <- rownames(instance)

    # Use default limits if they are not given explicitly.
    if ( is.null(in.min.max.limits) )
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")

    # Set colorRamps to use.
    cols.below <- ifelse ( is.null(color.ramp.below.neutral), colorRamp(c("red3", "yellow")), color.ramp.below.neutral)
    cols.above <- ifelse ( is.null(color.ramp.above.neutral), colorRamp(c("yellow","darkgreen")), color.ramp.above.neutral)

    # We have to get CI/CU one input at a time, so have to do it as a loop.
    ci.cu <- matrix(0, nrow=n.inps, ncol=2) # Initialize CI/CU matrix
    for ( i in 1:n.inps ) {
      f.label <- inp.names[i]
      if ( explain.concepts )
        expl.inps <- o.vocabulary[concepts.to.explain[i]][[1]]
      else
        expl.inps <- c(ind.inputs[i])
      ciu <- explain(instance, ind.inputs.to.explain=expl.inps,
                     in.min.max.limits=in.min.max.limits,
                     n.samples=n.samples,target.concept=target.concept, target.ciu=target.ciu)
      ci.cu[i,] <- as.numeric(ciu[ind.output,1:2])
    }

    # Limit everything to [0,1]
    ci.cu[ci.cu > 1] <- 1
    ci.cu[ci.cu < 0] <- 0

    # We get error otherwise...
    CIs <- as.numeric(ci.cu[,1])
    CUs <- as.numeric(ci.cu[,2])

    # Labels for the bars. Haven't tested what happens if this is NULL, maybe still fine.
    if ( !explain.concepts ) {
      # Add input values to input labels
      if ( show.input.values ) {
        for ( i in 1:length(inp.names) ) {
          value <- instance[ind.inputs[i]]
          if ( is.data.frame(value) )
            value <- value[[1]] # Crazy checks...
          if ( is.numeric(value) )
            value <- format(value, digits=2)
          inp.names[i] <- paste(inp.names[i], " (", value, ")", sep="")
        }
      }
    }

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
      inp.names <- inp.names[s$ix]
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

    # Plot title
    main.title <- main
    if ( is.null(main.title) ) {
      main.title <- o.outputnames[ind.output]
      main.title <- paste(main.title, " (", format(o.predict.function(o.model, instance)[ind.output], digits=2), ")", sep="")
      if ( !is.null(target.concept) ) {
        main.title <- paste(target.concept, "(", main.title, ")")
      }
    }

    # Do bar plot. Limit X axis to 1 because that's normally the maximal CI value.
    if ( is.null(xlab) ) xlab <- "Contextual Importance"
    if ( is.null(xlim) ) xlim <- c(0,1)
    barplot(bar.heights,col=bar.col,names=inp.names,horiz=T,las=1,
            main=main.title, xlab=xlab, xlim=xlim, ...)
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
  # n.samples: how many random instances to use for estimating CI and CU.
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
  pie.ciu <- function(instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL,
                      n.samples=100, neutral.CU=0.5,
                      show.input.values=TRUE, concepts.to.explain=NULL,
                      target.concept=NULL, target.ciu=NULL,
                      color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                      sort=NULL, decreasing=FALSE,
                      main=NULL, ...) {
    # Check if concepts are to be explained or pure inputs.
    # If no input indices are given, then use all inputs
    explain.concepts <- FALSE
    if ( !is.null(concepts.to.explain) ) {
      explain.concepts <- TRUE
      inp.names <- concepts.to.explain
    }
    else {
      if ( is.null(ind.inputs) ) {
        if ( is.null(dim(instance)) )
          ind.inputs <- 1:length(instance)
        else
          ind.inputs <- 1:ncol(instance)
      }
      inp.names <- o.input.names[ind.inputs]
      if ( is.null(inp.names) )
        inp.names <- names(instance)[ind.inputs]
    }
    n.inps <- length(inp.names)

    # Again, "instance" has to be a data.frame so this can't be NULL.
    inst.name <- rownames(instance)

    # Use default limits if they are not given explicitly.
    if ( is.null(in.min.max.limits) )
      in.min.max.limits <- o.in.minmax
    if ( is.null(in.min.max.limits) )
      stop("No minimum/maximum limits provided to 'new' nor 'explain'")

    # Set colorRamps to use.
    cols.below <- ifelse ( is.null(color.ramp.below.neutral), colorRamp(c("red3", "yellow")), color.ramp.below.neutral)
    cols.above <- ifelse ( is.null(color.ramp.above.neutral), colorRamp(c("yellow","darkgreen")), color.ramp.above.neutral)

    # We have to get CI/CU one input at a time, so have to do it as a loop.
    ci.cu <- matrix(0, nrow=n.inps, ncol=2) # Initialize CI/CU matrix
    for ( i in 1:n.inps ) {
      f.label <- inp.names[i]
      if ( explain.concepts )
        expl.inps <- o.vocabulary[concepts.to.explain[i]][[1]]
      else
        expl.inps <- c(ind.inputs[i])
      ciu <- explain(instance, ind.inputs.to.explain=expl.inps,
                     in.min.max.limits=in.min.max.limits,
                     n.samples=n.samples,target.concept=target.concept, target.ciu=target.ciu)
      ci.cu[i,] <- as.numeric(ciu[ind.output,1:2])
    }

    # Limit everything to [0,1]
    ci.cu[ci.cu > 1] <- 1
    ci.cu[ci.cu < 0] <- 0

    # We get error otherwise...
    CIs <- as.numeric(ci.cu[,1])
    CUs <- as.numeric(ci.cu[,2])

    # Labels for the bars. Haven't tested what happens if this is NULL, maybe still fine.
    if ( !explain.concepts ) {
      # Add input values to input labels
      if ( show.input.values ) {
        for ( i in 1:length(inp.names) ) {
          value <- instance[ind.inputs[i]]
          if ( is.data.frame(value) )
            value <- value[[1]] # Crazy checks...
          if ( is.numeric(value) )
            value <- format(value, digits=2)
          inp.names[i] <- paste(inp.names[i], " (", value, ")", sep="")
        }
      }
    }

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
      inp.names <- inp.names[s$ix]
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

    # Plot title
    main.title <- main
    if ( is.null(main.title) ) {
      main.title <- o.outputnames[ind.output]
      main.title <- paste(main.title, " (", format(o.predict.function(o.model, instance)[ind.output], digits=2), ")", sep="")
      if ( !is.null(target.concept) ) {
        main.title <- paste(target.concept, "(", main.title, ")")
      }
    }

    # Draw pie chart plot.
    pie(bar.heights,col=bar.col,labels=inp.names,
        main=main.title, ...)
  }

  # CIU feature importance/utility plot using ggplot. All parameters are not
  # implemented yet in this ggplot version.
  # Also, this method only works with the discrete input case for the moment.
  # - output.names: vector with names of outputs to include. If NULL (default),
  #   then include all.
  # - discrete.inputs: temporary parameter for indicating if all inputs are
  #   discrete. Should not be needed once "explain" has been updated to deal
  #   with both discrete and continuous inputs.
  # Also, not found out yet how to sort features according to CI (or CU)
  # for all facets, now they are all sorted according to mean value of all CI
  # (which might actually be a good choice).
  ggplot.col.ciu <- function(instance, ind.inputs=NULL, output.names=NULL,
                             in.min.max.limits=NULL,
                             n.samples=100, neutral.CU=0.5,
                             show.input.values=TRUE, concepts.to.explain=NULL,
                             target.concept=NULL, target.ciu=NULL,
                             low.color="red", mid.color="yellow",
                             high.color="darkgreen",
                             sort=NULL, decreasing=FALSE, # These are not used yet.
                             main=NULL) {

    # Check if concepts are to be explained or pure inputs.
    # If no input indices are given, then use all inputs
    explain.concepts <- FALSE
    if ( !is.null(concepts.to.explain) ) {
      explain.concepts <- TRUE
      inp.names <- concepts.to.explain
    }
    else {
      if ( is.null(ind.inputs) ) {
        if ( is.null(dim(instance)) )
          ind.inputs <- 1:length(instance)
        else
          ind.inputs <- 1:ncol(instance)
      }
      inp.names <- names(instance)[ind.inputs]
    }
    n.inps <- length(inp.names)

    # Again, "instance" has to be a data.frame so this can't be NULL.
    inst.name <- rownames(instance)

    # Create data frame for ggplot plotting
    ci.cu <- data.frame()
    for ( i in 1:n.inps ) {
      f.label <- inp.names[i]
      if ( explain.concepts )
        expl.inps <- o.vocabulary[concepts.to.explain[i]][[1]]
      else
        expl.inps <- c(ind.inputs[i])
      ciu <- explain(instance, ind.inputs.to.explain=expl.inps, in.min.max.limits=in.min.max.limits,
                     n.samples=n.samples,target.concept=target.concept, target.ciu=target.ciu)
      if ( show.input.values ) {
        # Don't really understand why [1,1] is necessary but otherwise factor values display numerically (not as strings).
        f.label <- paste0(f.label, " (", instance[ind.inputs[i]][1,1], ")")
      }

      # Some special treatment here for getting output names correct if result
      # variable is a factor, which leads to as many output classes as levels.
      if ( is.factor(o.data.outp[,1]) && !is.null(o.outputnames) ) {
        if ( length(levels(o.data.outp[,1])) == length(o.outputnames))
          rownames(ciu) <- o.outputnames
      }

      # Only include the outputs that are indicated to be included,
      # otherwise include all
      if ( !is.null(output.names) ) {
        ciu <- ciu[row.names(ciu) %in% output.names,]
      }
      ci.cu <- rbind(ci.cu, data.frame(Label=rownames(ciu), Output.Value=ciu$outval,
                                       in.names=inp.names[i], CI=ciu$CI, CU=ciu$CU,
                                       Output=paste0(rownames(ciu), " (",
                                                     format(ciu$outval, digits=3), ")"),
                                       feature.labels=f.label)
      )
    }

    # Sort facets according to output value. Sorting factor levels correctly does the job.
    ci.cu$Output <- factor(ci.cu$Output, unique(ci.cu$Output[order(ci.cu$Output.Value, decreasing = TRUE)]))

    # Check if main plot title has been given as parameter, otherwise use default one
    if ( is.null(main) ) {
      main <- paste("Studied instance (context):", inst.name)
      if  ( !is.null(target.concept) )
        main <- paste0(main, "\nTarget concept is \"", target.concept, "\"")
    }

    # Create the plot. Have to some tricks here for avoiding warnings either
    # by devtools.check or during execution. Apparently devtools.check
    # doesn't understand attach() explicitly nor done by ggplot
    fl <- ci.cu$feature.labels; ci <- ci.cu$CI; cu <- ci.cu$CU
    ggplot(ci.cu) +
      geom_col(aes(reorder(fl, ci), ci, fill=cu)) +
      coord_flip() +
      facet_wrap(~Output, labeller=label_both) + # Use scales="free_y" is different ordering for every facet
      ggtitle(main) +
      xlab("Feature") +
      ylim(0,1) +
      scale_fill_gradient2(low=low.color, mid=mid.color, high=high.color, midpoint=neutral.CU)
  }

  # Return list of "public" methods
  pub <- list(
    explain = function(instance, ind.inputs.to.explain, in.min.max.limits=NULL, n.samples=100,
                       target.concept=NULL, target.ciu=NULL) {
      explain(instance, ind.inputs.to.explain, in.min.max.limits, n.samples, target.concept, target.ciu)
    },
    # explain.discrete.inputs = function(data, instance, ind.inputs, target.concept=NULL, target.ciu=NULL) {
    #   explain.discrete.inputs(data, instance, ind.inputs, target.concept, target.ciu)
    # },
    # explain.vocabulary = function(instance, concepts.to.explain, in.min.max.limits=NULL, n.samples=1000,
    #                               target.concept=NULL, target.ciu=NULL) {
    #   explain.vocabulary(instance, concepts.to.explain, in.min.max.limits, n.samples,
    #                      target.concept, target.ciu)
    # },
    plot.ciu = function(instance, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab=NULL, ylab=NULL, ylim=NULL, ...) {
      plot.ciu (instance, ind.input, ind.output, in.min.max.limits, n.points, main, xlab, ylab, ylim, ...)
    },
    plot.ciu.3D = function(instance, ind.inputs, ind.output, in.min.max.limits=NULL, n.points=40,
                           main=NULL, xlab=NULL, ylab=NULL, zlab=NULL, zlim=NULL, ...) {
      plot.ciu.3D(instance, ind.inputs, ind.output, in.min.max.limits, n.points, main, xlab, ylab, zlab, zlim, ...)
    },
    barplot.ciu = function(instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, n.samples=100,
                           neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL, target.concept=NULL, target.ciu=NULL,
                           color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                           sort=NULL, decreasing=FALSE,
                           main= NULL, xlab=NULL, xlim=NULL, ...) {
      barplot.ciu(instance, ind.inputs, ind.output, in.min.max.limits, n.samples, neutral.CU, show.input.values,
                  concepts.to.explain, target.concept, target.ciu, color.ramp.below.neutral, color.ramp.above.neutral,
                  sort, decreasing, main, xlab, xlim, ...)
    },
    pie.ciu = function(instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, n.samples=100,
                       neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL, target.concept=NULL, target.ciu=NULL,
                       color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                       sort=NULL, decreasing=FALSE,
                       main= NULL, ...) {
      pie.ciu(instance, ind.inputs, ind.output, in.min.max.limits, n.samples, neutral.CU,
              show.input.values, concepts.to.explain, target.concept, target.ciu,
              color.ramp.below.neutral, color.ramp.above.neutral,
              sort, decreasing, main, ...)
    },
    ggplot.col.ciu = function(instance, ind.inputs=NULL, output.names=NULL,
                              in.min.max.limits=NULL,
                              n.samples=100, neutral.CU=0.5,
                              show.input.values=TRUE, concepts.to.explain=NULL,
                              target.concept=NULL, target.ciu=NULL,
                              low.color="red", mid.color="yellow",
                              high.color="darkgreen",
                              sort=NULL, decreasing=FALSE, # These are not used yet.
                              main=NULL) {
      ggplot.col.ciu(instance, ind.inputs, output.names, in.min.max.limits,
                     n.samples, neutral.CU,
                     show.input.values, concepts.to.explain,
                     target.concept, target.ciu,
                     low.color, mid.color, high.color,
                     sort, decreasing, main)
    }
  )

  class(pub) <- c("CIU", class(pub))
  return(pub)
}


#=========================================================================
# After this comes development-time code, for testing etc.
#=========================================================================

# # Call e.g. "adaline.three.inputs.test()".
# # Or "adaline.three.inputs.test(indices=c(1,3))" for getting joint importance of inputs one and three.
# adaline.three.inputs.test <- function(inp=c(0.1,0.2,0.3), indices=c(1), n.samples=100) {
#   a <- adaline.new(3, 1)
#   inp <- c(0.1,0.2,0.3)
#   w <- c(0.20,0.30,0.50)
#   a$set.weights(matrix(w, nrow=1, byrow=T))
#   ciu <- ciu.new(a, in.min.max.limits=matrix(c(0,1,0,1),nrow=2,byrow=T), abs.min.max=matrix(c(0, 1), nrow=1, byrow=T))
#   CI.CU <- ciu$explain(inp, ind.inputs.to.explain=indices)
#   CI.CU
# }
#
# ## Two outputs
# # Call e.g. "adaline.two.outputs.test()"
# # Or "adaline.two.outputs.test(indices=c(1,3))" for getting joint importance of inputs one and three.
# adaline.two.outputs.test <- function(inp=c(0.1,0.2,0.3), indices=c(1), n.samples=100) {
#   a <- adaline.new(3, 2)
#   w <- matrix(c(0.20,0.30,0.50,0.25,0.35,0.40), nrow=2, byrow=TRUE)
#   a$set.weights(w)
#   #out2 <- a2$eval(inp2)
#   ciu <- ciu.new(a, in.min.max.limits=matrix(c(0,1,0,1,0,1),nrow=3,byrow=T), abs.min.max=matrix(c(0,1,0,1), nrow=2, byrow=T))
#   CI.CU <- ciu$explain(inp, ind.inputs.to.explain=indices)
#   CI.CU
# }
#
# # Tests with vocabulary, intermediate concepts.
# vocabulary.test <- function() {
#   a <- adaline.new(3, 2)
#   w <- matrix(c(0.20,0.30,0.50,0.25,0.35,0.40), nrow=2, byrow=TRUE)
#   a$set.weights(w)
#   voc <- list(oneand2=c(1,2),twoand3=c(2,3),oneand3=c(1,3),one=c(1))
#   ciu <- ciu.new(a, in.min.max.limits=matrix(c(0,1,0,1,0,1),nrow=3,byrow=T), abs.min.max=matrix(c(0,1,0,1), nrow=2, byrow=T),
#                  vocabulary=voc)
#   inp <- c(0.1,0.2,0.3)
#   CI.CU <- ciu$explain.vocabulary(inp, concepts.to.explain=c("oneand2","oneand3"), n.samples=1000)
#   print(CI.CU)
# }
#
#
