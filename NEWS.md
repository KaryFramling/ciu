# ciu (development version)

* `ciu.contrastive` is now implemented as a call to `ciu.contextual.influence`
  with a per-feature reference CU, making contrastive explanations the general
  case and `neutral.CU = 0.5` the special case of one and the same
  factorisation, rather than two separate mechanisms. Both functions now reject
  a reference vector whose length is neither 1 nor the number of features,
  instead of silently recycling it.
* `ciu.ggplot.col` (and the `ggplot.col.ciu` method): `sort` and `decreasing`
  are now implemented rather than merely declared, and accept "CI", "CU", "phi"
  and "absphi". The new `row.order` parameter takes an explicit vector of
  feature labels, which allows two panels (e.g. CI/CU and influence) to be shown
  with rows aligned so that a feature can be tracked across them.
* `ciu.ggplot.col` now accepts a per-feature `neutral.CU` vector. It previously
  computed influence inside a per-input loop, where a vector recycled over
  outputs rather than features.
* `ggplot.col.ciu` gained a default of `NULL` for `instance`, which was
  previously required even when passing `ciu.meta`.
* `ciu.explain.long.data.frame` now returns an `Instance` column.
* Added `ciu.kg.feature.of.edges`, `ciu.kg.orphan.nodes` and
  `ciu.kg.lookup.by.name`, generic knowledge-graph vocabulary helpers that were
  previously duplicated in the data-set-specific test scripts. The two test
  scripts now look labels and descriptions up by column name (German Credit) or
  check them against the data columns (Ames), so a change in column order can no
  longer silently attach labels to the wrong features.
* Removed dead code from `TestGermanCreditKnowledgeGraph.R`: the
  `categorical_values` mappings and their helpers (keyed on column names that do
  not exist, and using UCI codes that `rchallenge::german` has already decoded)
  and three hierarchy definitions referencing non-existent columns.

* Implemented a function `ciu.igraph.additive_attribution_to_ICs` that makes it 
  possible to also use Shapley values for Intermediate Concepts and therefore 
  also with graph visualisations. 
* Moved the Knowledge Graph code from the EXTRAAMAS'25 paper into the `ciu` 
  package, which required some serious rewriting for making it generic to 
  any data set/model, instead of being specific for Ames Housing. This includes 
  the interactive Shiny app too, with support for "partner models" etc.
  Source files are included in the top directory for using the Ames Housing and 
  German Credit data sets with Knowledge Graphs and Intermediate Concepts. This
  is a major update. 

# ciu 0.8

* Changed the default colours if influence/contrastive plots to the brick red
  / blue used by LIME in R version.
* Corrected the function "ciu.contrastive" that was implemented in a very strange
  (and presumably incorrect) way.
* Added possibility to get line segment IO-plots for categorical features,
  rather than the earlier histogram visualisation. Added parameter to
  ggplot.ciu() "categorical_style="segment"", where the default is now to
  use line segments, even though that breaks backwards compatibility a little.
  However, for some reason setting "ylim" breaks the old "geom_col" plot so the bar
  plots are now un-scaled. But, strange enough, "+ ylim" applied to the returned
  ggplot works so it's possible to adjust afterwards. Something happened somewhere,
  no change in the CIU code and it worked in the past.
* Added new function ciu.contrastive that uses CU values of one 
  class/instance/whatever as the baseline "normal.CU" values of the other. 
* Added new function ciu.contextual.influence that calculates contextual influence 
  value(s) from CI and CU value(s) and "baseline".
* Added new file ContextualInfluence.R for functions related to contextual influence. 
* Fixed x-axis label error for influence plot in ``ciu.ggplot.col``.

# ciu 0.6.0

* Extensive tests on Intermediate Concepts, which are also integrated into the 
  new README.Rmd file (and README) as well as in test scripts TestCases.R and 
  TestCases_NoObject.R on Github. 
* Fixed inverted axis bug in plot in ``plot.ciu.3D``.
* Integrated support for mlr3. Only tested with "classif.rpart" model though 
  because other tested models (xgboost, nnet, ...) don't like R types and no
  need/time to dive into mlr3 for the moment. To fix in the future in case bugs 
  are discovered.
* Added parameter "plot.mode" to "ciu.ggplot.col" function, which allows for value 
  "overlap" that plots CI as a bar and then CU as another bar over it, scaled 
  into CI bar. Colour of CU bar can either be fixed (dark green by default) or set to NULL, 
  which then uses the green-yellow-red color scale (unless those colours are 
  changed by the function parameters).
* Removed parameter for min/max values of contextual influence because the 
  range of contextual influence is (and should be) always one (1) based on 
  the mathematical constructs. 
* Added new method "ggplot.ciu" to ciu.new for plotting input/output graphs with 
  ggplot. Identical to old plot.ciu, except that 1) ggplot offers some 
  advantages, notably what comes to figure scaling, 2) added possibility to 
  include CIU visualisation (cmin, cmax, neutral) by setting "illustrate.CIU=TRUE". 
* Added new method "influence" to ciu.new for getting numerical contextual 
  influence (rather than just seeing them in plots).
  TO-DO: add corresponding function to "ciu.R".
* Corrected axis in ``plot.ciu.3D``.

# ciu 0.5.0

* Textual explanations have been implemented with function "ciu.textual". 
* Implemented "meta.explain" method, which returns a self-contained 
  "ciu.meta.result" object with CIU results for a given instance. 
  This mainly makes it possible to visualize exactly the same CIU result 
  in different ways. 
  Before this (and still if no "ciu.meta" parameter is given), every 
  visualization method ran "explain" again, so CIU results could differ 
  somewhat between different runs.
* Added parameters "use.influence" and "influence.minmax" to "ggplot.col" 
  and "barplot.ciu" functions/methods, which produces a LIME/SHAP/etc-like plot 
  where "influence = (influence.max - influence.min)*ci(cu-neutral.CU)", bars 
  go either right or left of zero and there are only two colours. 
* Added support for factor-type inputs to method "$plot.ciu". 
* Created new "class" called "ciu", which is just a list object with the 
  "instance variables" of a CIU object. This makes it possible to create 
  plot functions that are not methods of CIU but rather take a "ciu" object 
  as their first parameter. The main reason for doing this modification is that 
  CIU objects seem to use much more memory than a simple data-based "ciu" object. 
  The documentation and package tools in R are also not too aware about 
  "inner functions" (methods), which is an inconvenient. Finally, the plan was 
  indeed to go for this approach in any case because increasing the code length
  of CIU was not desirable in the long run. The way in which it is implemented 
  now allows functions and methods to be used interchangeably in any case. 

# ciu 0.1.0

* First version of ciu published at CRAN
