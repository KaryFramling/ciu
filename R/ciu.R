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
#' @param ciu blabla
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
#' @param ciu blabla
#' @param instance blabla
#' @param ind.inputs.to.explain blabla
#' @param in.min.max.limits blabla
#' @param n.samples blabla
#' @param target.concept blabla
#' @param target.ciu blabla
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
#' @param ciu blabla
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
#' @param ciu blabla
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
#' @param use.influence blabla
#' @param sort blabla
#' @param decreasing blabla
#' @param main blabla
#' @param xlab blabla
#' @param xlim blabla
#' @param ... blabla
#'
#' @return blabla
#' @export
#' @author Kary Främling
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
