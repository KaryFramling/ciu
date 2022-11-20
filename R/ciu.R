#' Create `ciu` object.
#'
#' Sets up a `ciu` object with the given parameters. This is not the same as
#' a `CIU` object as returned by the function [ciu.new]! a `ciu` object is a
#' [list] with all the parameter values needed for Contextual Importance and
#' Utility calculations, whereas a `CIU` object only exposes a set of methods
#' that can be called using the `$` operator. `CIU` provides the method
#' `$as.ciu` for retrieving a `ciu` object from a `CIU` object.
#'
#' @param model Model/"black-box" object (same parameter as `bb` for function
#' [ciu.new]).
#' @inheritParams ciu.new
#'
#' @return `ciu` object.
#' @seealso [ciu.new]
#' @export
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
#' ciu <- ciu(model, Species~., iris)
#'
#' # This can be used with explain method for getting CIU values
#' # of one or several inputs. Here we get CIU for all three outputs
#' # with input feature "Petal.Length" that happens to be the most important.
#' ciu.explain(ciu, iris_test, 1)
#'
#' # It is, however, more convenient to use one of the graphical visualizations.
#' # Here's one using ggplot.
#' ciu.ggplot.col(ciu, iris_test)
#'
#' @author Kary Främling
ciu <- function(model, formula=NULL, data=NULL, in.min.max.limits=NULL, abs.min.max=NULL,
                input.names=NULL, output.names=NULL, predict.function=NULL,
                vocabulary=NULL) {
  ciu <- ciu.new(model, formula, data, in.min.max.limits, abs.min.max,
                 input.names, output.names, predict.function,
                 vocabulary)
  this <- ciu$as.ciu()
  this$CIU <- ciu
  class(this) <- c("ciu", class(this))
  return(this)
}


#' Create `CIU` object from `ciu` object.
#'
#' A `CIU` object is an "object-oriented programming" object, i.e. it has its
#' own environment, private variables and methods etc. A `CIU` object is created
#' using [ciu.new] like `ciu_obj <- ciu.new(...)` and the object's methods
#' are then called as `ciu_obj$method(...)`. This approach has numerous advantages
#' but CIU objects consume much more memory than "ordinary" R data structures.
#'
#' A `ciu` object is simply a [list] that contains all the "object variables" of
#' a `CIU` object, which is the reason why CIU <-> ciu conversions can be done at
#' any time. CIU -> ciu conversion doesn't have any overhead but ciu -> CIU does
#' require overhead due to the environment setup etc. Therefore, it is advisable
#' to avoid unnecessary CIU -> ciu conversions.
#'
#' `ciu` objects are very memory-efficient because they are ordinary [list] objects
#' (however, make sure that ciu$CIU element's value is NULL). `ciu` objects also
#' give direct access to all the object variables that are private in a `CIU` object.
#'
#' However, using `ciu` objects means that they have to be passed as a parameter
#' to all functions that use them. The advantages of object oriented programming
#' are of course lost too.
#'
#' @param ciu `ciu` object.
#'
#' @return CIU object
#' @export
ciu.to.CIU <- function(ciu) {
  CIU <- ciu.new(ciu$model, ciu$formula, ciu$data, ciu$in.min.max.limits,
                 ciu$abs.min.max, ciu$input.names, ciu$output.names,
                 ciu$predict.function, ciu$vocabulary)
  return(CIU)
}

#' Calculate CIU for specific instance
#'
#' Calculate Contextual Importance (CI) and Contextual Utility (CU) for an
#' instance (Context) using the given "black-box" model.
#'
#' @param ciu `ciu` object as created with [ciu] function (not to be confused
#' with `CIU` object as created by [ciu.new]).
#' @param instance Input values for the instance to explain. Should be a
#' [data.frame] even though a `vector` or `matrix` might work too if input
#' names and other needed metadata can be deduced from the dataset or other
#' parameters given to \code{\link{ciu.new}}.
#' @param ind.inputs.to.explain [vector] of indices for the inputs to be
#' explained, i.e. for which CIU should be calculated. If `NULL`, then all
#' inputs will be included.
#' @param in.min.max.limits [data.frame] or [matrix] with one row per output
#' and two columns, where the first column indicates the minimal value and the
#' second column the maximal value for that output. ONLY NEEDED HERE IF not
#' given as parameter to [ciu.new] or if the limits are different for this
#' specific instance than the default ones.
#' @param n.samples How many instances to generate for estimating CI and CU.
#' For inputs of type [factor], all possible combinations of input values
#' are generated, so this parameter only influences how many instances are
#' (at least) generated for continuous-valued inputs.
#' @param target.concept If provided, then calculate CIU of inputs
#' `ind.inputs.to.explain` relative to the given concept rather than
#' relative to the actual output(s). `ind.inputs.to.explain` should
#' normally be a subset (or all) of the inputs that `target.concept`
#' consists of, even though that not required by the CIU calculation.
#' If a `target.ciu` is provided, then the `target.concept` doesn't have to
#' be included in the `vocabulary` gives as parameter to [ciu.new]
#' (at least for the moment).
#' @param target.ciu `ciu.result` object previously calculated for
#' `target.concept`. If a `target.concept` is provided but `target.ciu=NULL`,
#' then `target.ciu` is estimated by a call to [ciu.explain] with the
#' `n.samples` value given as a parameter to this call. It may be useful
#' to provide `target.ciu` if it should be estimated using some other
#' (typically greater) value for `n.samples` than the default one, or if it
#' has already been calculated for some reason.
#'
#' @return `ciu.result` object.
#' @export
#' @author Kary Främling
ciu.explain <- function(ciu, instance, ind.inputs.to.explain, in.min.max.limits=NULL, n.samples=100,
                        target.concept=NULL, target.ciu=NULL) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$explain(instance, ind.inputs.to.explain, in.min.max.limits, n.samples, target.concept, target.ciu)
}

#' ciu.plot
#'
#' Function for plotting out the effect of changing values of one input on one output
#'
#' @inheritParams ciu.explain
#' @param ind.input Index of input feature to plot.
#' @param ind.output Index of output to plot.
#' @param n.points How many x,y pairs will be calculated, equally spaced over in.min.max.limits.
#' @param main Text to use as main title.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param ylim Minimal and maximal values for y-axis.
#' @param ... See [base::plot].
#'
#' @return "void", or whatever happens to be result of last instruction.
#' @export
#' @seealso [base::plot] for "..." parameters.
#' @author Kary Främling
ciu.plot <- function(ciu, instance, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab=NULL, ylab=NULL, ylim=NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$plot.ciu (instance, ind.input, ind.output, in.min.max.limits, n.points, main, xlab, ylab, ylim, ...)
}

#' ciu.ggplot
#'
#' Function for plotting out the effect of changing values of one input on one output.
#'
#' @inheritParams ciu.plot
#' @param illustrate.CIU Include illustration of CIU Cmin, Cmax, neutral.CU. Default is FALSE
#' @param neutral.CU Value of neutral.CU. Default is 0.5.
#' @param CIU.illustration.colours Colours to use for illustrating CIU.
#' Default is red, orange, green.
#'
#' @return ggplot object.
#' @export
#' @author Kary Främling
ciu.ggplot <- function(ciu, instance, ind.input=1, ind.output=1, in.min.max.limits=NULL, n.points=40, main=NULL, xlab=NULL, ylab=NULL,
                       ylim=NULL, illustrate.CIU=FALSE, neutral.CU=0.5, CIU.illustration.colours=c("red", "orange", "green", "blue")) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$ggplot.ciu(instance, ind.input, ind.output, in.min.max.limits, n.points, main, xlab, ylab,
                 ylim, illustrate.CIU, neutral.CU, CIU.illustration.colours)
}

#' ciu.plot.3D
#'
#' Function for 3D plotting the effect of changing values of two inputs on one output.
#'
#' @inheritParams ciu.explain
#' @inheritParams graphics::plot
#' @inheritParams graphics::persp
#' @param ind.inputs Indices of input features to plot.
#' @param ind.output Index of output to plot.
#' @param n.points Number of x/y-axis points to use.
#' @param zlab Label to use for Z-axis. Default: NULL.
#' @param zlim Limits to use for Z-axis. Default: NULL.
#' @return "void", or whatever happens to be result of last instruction.
#' @export
#' @author Kary Främling
ciu.plot.3D <- function(ciu, instance, ind.inputs, ind.output, in.min.max.limits=NULL, n.points=40,
                        main=NULL, xlab=NULL, ylab=NULL, zlab=NULL, zlim=NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$plot.ciu.3D(instance, ind.inputs, ind.output, in.min.max.limits, n.points, main, xlab, ylab, zlab, zlim, ...)
}

#' ciu.barplot
#'
#' Create a barplot showing CI as the length of the bar and CU on color scale from
#' red to green, via yellow, for the given inputs and the given output.
#'
#' @inheritParams ciu.meta.explain
#' @param ind.inputs \code{\link{vector}} of indices for the inputs to be
#' included in the plot. If NULL then all inputs will be included.
#' @param ind.output Index of output to be explained.
#' @param neutral.CU Indicates when the Contextual Utility is considered
#' to be "negative". The default value of 0.5 seems quite logical for most cases.
#' @param show.input.values Include input values after input labels or not.
#' Default is TRUE.
#' @param concepts.to.explain List of concepts to use in the plot, as defined
#' by vocabulary provided as argument to [ciu.new]. If `ind.inputs=NULL`,
#' then use `concepts.to.explain` instead. If both are `NULL`, then use all inputs.
#' @param ciu.meta If given, then use existing `ciu.meta.result` rather
#' than calling [ciu.meta.explain].
#' @param color.ramp.below.neutral Color ramp function as returned by function
#' `colorRamp()`. Default color ramp is from red3 to yellow.
#' @param color.ramp.above.neutral Color ramp function as returned by function
#' `colorRamp()`. Default colorramp is from yellow to darkgreen.
#' @param use.influence Plot using "influence" rather than CIU, i.e. a
#' LIME-like barplot. Default is FALSE.
#' @param sort NULL, "CI" or "CU".
#' @param decreasing Set to TRUE for decreasing sort.
#' @param main Text to use as main title.
#' @param xlab Label for x-axis.
#' @param xlim Minimal and maximal values for x-axis.
#' @param ... See [base::plot].
#'
#' @return "void", i.e. whatever happens to be result of last instruction.
#' @export
#' @author Kary Främling
#' @seealso [ggplot.col.ciu]
#' @seealso [pie.ciu]
#' @seealso [ciu.new]
#' @seealso [ciu.explain]
ciu.barplot <- function(ciu, instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, n.samples=100,
                        neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL, target.concept=NULL, target.ciu=NULL,
                        ciu.meta = NULL,
                        color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                        use.influence=FALSE,
                        sort=NULL, decreasing=FALSE,
                        main= NULL, xlab=NULL, xlim=NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$barplot.ciu(instance, ind.inputs, ind.output, in.min.max.limits, n.samples, neutral.CU, show.input.values,
                  concepts.to.explain, target.concept, target.ciu, ciu.meta, color.ramp.below.neutral, color.ramp.above.neutral,
                  use.influence, sort, decreasing, main, xlab, xlim, ...)
}

#' ciu.pie
#'
#' Create a pie chart showing CI as the area of slice and CU on color scale from
#' red to green, via yellow, for the given inputs and the given output.
#' @inheritParams ciu.barplot
#'
#' @return "void", i.e. whatever happens to be result of last instruction.
#' @export
#' @author Kary Främling
ciu.pie <- function(ciu, instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, n.samples=100,
                    neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL,
                    target.concept=NULL, target.ciu=NULL, ciu.meta = NULL,
                    color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                    sort=NULL, decreasing=FALSE,
                    main= NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$pie.ciu(instance, ind.inputs, ind.output, in.min.max.limits, n.samples, neutral.CU,
              show.input.values, concepts.to.explain, target.concept, target.ciu, ciu.meta,
              color.ramp.below.neutral, color.ramp.above.neutral,
              sort, decreasing, main, ...)
}
