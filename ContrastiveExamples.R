# Examples of how to create contrastive explanations for different datasets.
#
# Author: Kary främling

library(ggplot2)
library(MASS)
library(caret)
library(ciu)

#' Run demo of contrastive CIU explanations with Boston Housing data set.
#'
#' @return [ggplot] object.
#' @export
#'
#' @examples
#' # Use defaults
#' ciu.contrastive.demo.boston()
#' # Defaults but given explicitly
#' ciu.contrastive.demo.boston(instance1 = 370, instance2 = 100, question = "Why?")
#' # For "Why not?" explanation, the first instance should be the non-preferred
#' # one.
#' ciu.contrastive.demo.boston(instance1 = 100, instance2 = 370, question = "Why not?")
ciu.contrastive.demo.boston <- function(instance1 = 370, instance2 = 100, question="Why?") {
  library(caret)
  kfoldcv <- trainControl(method="cv", number=10)
  gbm <- caret::train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
  inst1 <- Boston[instance1, 1:13]
  inst2 <- Boston[instance2, 1:13]
  ciu <- ciu.new(gbm, medv~., Boston)
  meta1 <- ciu$meta.explain(inst1)
  meta2 <- ciu$meta.explain(inst2)
  ciuvals.inst1 <- ciu.list.to.frame(meta1$ciuvals)
  ciuvals.inst2 <- ciu.list.to.frame(meta2$ciuvals)
  contrastive <- ciu.contrastive(ciuvals.inst1, ciuvals.inst2)
  p <- ciu.ggplot.contrastive(meta1, contrastive,
                              c(paste("Instance", instance1), paste("Instance", instance2)),
                              question = question)
  print(p)
  # p <- ciu$ggplot.col.ciu(inst1); print(p)
  # p <- ciu$ggplot.col.ciu(inst1, plot.mode = "overlap"); print(p)
  # p <- ciu$ggplot.col.ciu(inst1, cu.colours=NULL, plot.mode = "overlap"); print(p)
  # p <- ciu$ggplot.col.ciu(inst2); print(p)
  # p <- ciu$ggplot.col.ciu(inst2, plot.mode = "overlap"); print(p)
  # p <- ciu$ggplot.col.ciu(inst2, cu.colours=NULL, plot.mode = "overlap"); print(p)
  return(p)
}

#' Run demo of contrastive CIU explanations with Ames data set.
#'
#' Default instances are one of the most expensive ones and one of the cheapest ones.
#'
#' @return [ggplot] object.
#' @export
#'
#' @examples
#' ciu.contrastive.demo.ames()
ciu.contrastive.demo.ames <- function(instance1 = 16, instance2 = 182, question="Why?") {
  library(AmesHousing)
  ames <- data.frame(make_ames())
  kfoldcv <- trainControl(method="cv", number=10)
  gbm <- caret::train(Sale_Price ~ ., ames, method="gbm", trControl=kfoldcv)
  ind <- which(names(ames)=="Sale_Price")
  inst1 <- ames[instance1,-ind]
  inst2 <- ames[instance2,-ind]
  ciu <- ciu.new(gbm, Sale_Price~., ames)
  meta1 <- ciu$meta.explain(inst1)
  meta2 <- ciu$meta.explain(inst2)
  ciuvals.inst1 <- ciu.list.to.frame(meta1$ciuvals)
  ciuvals.inst2 <- ciu.list.to.frame(meta2$ciuvals)
  contrastive <- ciu.contrastive(ciuvals.inst1, ciuvals.inst2)
  p <- ciu.ggplot.contrastive(meta1, contrastive,
                              c(paste("Instance", instance1), paste("Instance", instance2)),
                              question = question)
  print(p)
  # Vocabularies
  Ames.voc <- list(
    "Garage"=c(58,59,60,61,62,63),
    "Basement"=c(30,31,33,34,35,36,37,38,47,48),
    "Lot"=c(3,4,7,8,9,10,11),
    "Access"=c(13,14),
    "House type"=c(1,15,16,21),
    "House aesthetics"=c(22,23,24,25,26),
    "House condition"=c(17,18,19,20,27,28),
    "First floor surface"=c(43),
    "Above ground living area"=which(names(data)=="Gr_Liv_Area"))
  Ames.voc_ciu <- ciu.new(gbm, Sale_Price~., ames, vocabulary = Ames.voc)
  Ames.voc_ciu.meta <- Ames.voc_ciu$meta.explain(inst1)
  meta.top1 <- Ames.voc_ciu$meta.explain(inst1, concepts.to.explain=names(Ames.voc), n.samples = 1000)
  meta.top2 <- Ames.voc_ciu$meta.explain(inst2, concepts.to.explain=names(Ames.voc), n.samples = 1000)
  ciuvals.inst1 <- ciu.list.to.frame(meta.top1$ciuvals)
  ciuvals.inst2 <- ciu.list.to.frame(meta.top2$ciuvals)
  contrastive <- ciu.contrastive(ciuvals.inst1, ciuvals.inst2)
  p <- ciu.ggplot.contrastive(meta.top1, contrastive,
                              c(paste("Instance", instance1), paste("Instance", instance2)),
                              question = question)
  print(p)

  # Facet plot with contrastive and individual explanations
  p1 <- p + theme(legend.position = "none")
  p2 <- Ames.voc_ciu$ggplot.col.ciu(inst1, concepts.to.explain=names(Ames.voc),
                                   plot.mode = "overlap"); #print(p)
  p3 <- Ames.voc_ciu$ggplot.col.ciu(inst2, concepts.to.explain=names(Ames.voc),
                                   plot.mode = "overlap"); #print(p)
  grid.arrange(p1, p2, p3, nrow = 1)

  return(p)
}

