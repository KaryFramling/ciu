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

#' Explain CIU
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
#' @param target.ciu [ciu.result] object previously calculated for
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
#' @param ciu `ciu` object.
#' @param instance blabla
#' @param ind.input blabla
#' @param ind.output blabla
#' @param in.min.max.limits blabla
#' @param n.points blabla
#' @param main blabla
#' @param xlab blabla
#' @param ylab blabla
#' @param ylim blabla
#' @param ... blabla
#'
#' @return blabla
#' @export
#' @author Kary Främling
ciu.plot <- function(ciu, instance, ind.input, ind.output, in.min.max.limits=NULL, n.points=40, main=NULL, xlab=NULL, ylab=NULL, ylim=NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$plot.ciu (instance, ind.input, ind.output, in.min.max.limits, n.points, main, xlab, ylab, ylim, ...)
}

#' ciu.plot.3D
#'
#' @param ciu `ciu` object.
#' @param instance blabla
#' @param ind.inputs blabla
#' @param ind.output blabla
#' @param in.min.max.limits blabla
#' @param n.points blabla
#' @param main blabla
#' @param xlab blabla
#' @param ylab blabla
#' @param zlab blabla
#' @param zlim blabla
#' @param ... blabla
#'
#' @return blabla
#' @export
#' @author Kary Främling
ciu.plot.3D <- function(ciu, instance, ind.inputs, ind.output, in.min.max.limits=NULL, n.points=40,
                        main=NULL, xlab=NULL, ylab=NULL, zlab=NULL, zlim=NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$plot.ciu.3D(instance, ind.inputs, ind.output, in.min.max.limits, n.points, main, xlab, ylab, zlab, zlim, ...)
}

#' ciu.barplot.ciu
#'
#' @param ciu `ciu` object.
#' @param instance Instance to explain. See \code{\link{ciu.explain}}.
#' @param ind.inputs \code{\link{vector}} of indices for the inputs to be
#' included in the plot. If NULL then all inputs will be included.
#' @param ind.output Index of output to be explained.
#' @param in.min.max.limits See \code{\link{ciu.explain}}.
#' @param n.samples See \code{\link{ciu.explain}}.
#' @param neutral.CU Indicates when the Contextual Utility is considered
#' to be "negative". The default value of 0.5 seems quite logical for most cases.
#' @param show.input.values Include input values after input labels or not.
#' Default is TRUE.
#' @param concepts.to.explain List of concepts to use in the plot, as defined
#' by vocabulary provided as argument to [ciu.new]. If `ind.inputs=NULL`,
#' then use `concepts.to.explain` instead. If both are `NULL`, then use all inputs.
#' @param target.concept See [ciu.explain].
#' @param target.ciu See [ciu.explain].
#' @param color.ramp.below.neutral Color ramp function as returned by function
#' `colorRamp()`. Default color ramp is from red3 to yellow.
#' @param color.ramp.above.neutral Color ramp function as returned by function
#' `colorRamp()`. Default colorramp is from yellow to darkgreen.
#' @param use.influence Plot using "influence" rather than CIU, i.e. a
#' LIME-like barplot. Default is FALSE.
#' @param sort NULL, "CI" or "CU". No sorting by default, other options are
#' sorting by CI or CU.
#' @param decreasing Set to TRUE for decreasing sort.
#' @param main Usual plot parameter, possible to override default one if needed.
#' @param xlab Usual plot parameter, possible to override default one if needed.
#' @param xlim Usual plot parameter, possible to override default one if needed.
#' @param ... Other graphical parameters to pass to [base::plot]
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
                        color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL, use.influence=FALSE,
                        sort=NULL, decreasing=FALSE,
                        main= NULL, xlab=NULL, xlim=NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$barplot.ciu(instance, ind.inputs, ind.output, in.min.max.limits, n.samples, neutral.CU, show.input.values,
                  concepts.to.explain, target.concept, target.ciu, color.ramp.below.neutral, color.ramp.above.neutral,
                  use.influence, sort, decreasing, main, xlab, xlim, ...)
}

#' ciu.pie
#'
#' @param ciu blabla
#' @param instance blabla
#' @param ind.inputs blabla
#' @param ind.output blabla
#' @param in.min.max.limits blabla
#' @param n.samples blabla
#' @param neutral.CU blabla
#' @param show.input.values blabla
#' @param concepts.to.explain blabla
#' @param target.concept blabla
#' @param target.ciu blabla
#' @param color.ramp.below.neutral blabla
#' @param color.ramp.above.neutral blabla
#' @param sort blabla
#' @param decreasing blabla
#' @param main blabla
#' @param ... blabla
#'
#' @return blabla
#' @export
#' @author Kary Främling
ciu.pie <- function(ciu, instance, ind.inputs=NULL, ind.output=1, in.min.max.limits=NULL, n.samples=100,
                    neutral.CU=0.5, show.input.values=TRUE, concepts.to.explain=NULL, target.concept=NULL, target.ciu=NULL,
                    color.ramp.below.neutral=NULL, color.ramp.above.neutral=NULL,
                    sort=NULL, decreasing=FALSE,
                    main= NULL, ...) {
  if ( inherits(ciu, "ciu") )
    ciu <- ciu.to.CIU(ciu)
  ciu$pie.ciu(instance, ind.inputs, ind.output, in.min.max.limits, n.samples, neutral.CU,
              show.input.values, concepts.to.explain, target.concept, target.ciu,
              color.ramp.below.neutral, color.ramp.above.neutral,
              sort, decreasing, main, ...)
}
