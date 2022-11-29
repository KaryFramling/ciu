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
