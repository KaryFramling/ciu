# Tests to perform on KnowledgeGraph functionality of ciu package with Ames
# housing data set.

library(AmesHousing)
library(igraph)
library(caret)
library(visNetwork)
library(htmltools)
library(shiny)
library(shinyBS)
library(ciu)

# Launch the Shiny interactive "Social XAI" application with the Ames data,
# models etc.
# You can just do "source" and then call this function.
launch.Shiny.Ames <- function() {
  ciu.launch.social_xai.shiny(create.CIU_Ames_for_Shiny())
}

# Create visNetwork for Ames with CIU visualisation.
# instance <- data.frame(make_ames())[16,]
# get.Ames.visNetwork(instance)
get.Ames.visNetwork <- function(instance) {
  kg_ciu <- create.CIU_Ames_for_Shiny()
  voc_graph <- kg_ciu$partner_models[[1]]$graph
  ciu_g <- ciu.igraph.explain(instance, kg_ciu$ciu, voc_graph, "SalesPrice_def_voc")
  ciu_g <- ciu.igraph.explain(instance, kg_ciu$ciu, ciu_g, "Miscellaneous")
  return(ciu.plots.visNetwork_CIU(ciu_g))
}

# Create Ames Knowledge Graphs, partner models, CIU etc. and return them
# in a list that works for the Shiny app.
create.CIU_Ames_for_Shiny <- function(model_index = 1, caret_model="gbm") {
  ames_data <- data.frame(make_ames())
  target_variable <- "Sale_Price"
  ames_formula <- as.formula(paste(target_variable, "~."))
  pmodels <- create.Ames.KGs(ames_data)
  #Ames_voc <- ciu.voc.from.graph(pmodels[[model_index]]$graph, colnames(ames_data)[colnames(ames_data) != "Sale_Price"])
  # Train and show some performance indicators.
  kfoldcv <- trainControl(method="cv", number=10)
  model <- train(ames_formula, ames_data, method=caret_model, trControl=kfoldcv)
  ames_ciu <- ciu.new(model, ames_formula, ames_data, knowledge.graph = pmodels[[model_index]]$graph)

  # Return a list with all the required information for launching the Shiny app.
  return(list(model=model, data=ames_data, target_variable=target_variable,
              ciu=ames_ciu, partner_models=pmodels))
}

# Create "partner models" for explaining Ames Housing results.
# Returns a list of "partner models", where each partner model has a name,
# a list of root node names and an igraph object.
create.Ames.KGs <- function(ames_data, target_variable = "Sale_Price") {
  # Create the graph with only vertices
  g <- make_empty_graph()
  data_names <- colnames(ames_data)
  feature_names <- c(
    "Type of dwelling", "Zoning classification", "Lot frontage", "Lot area",
    "Street type", "Alley type", "Lot shape", "Land contour",
    "Utilities available", "Lot configuration", "Land slope", "Neighborhood",
    "Proximity to road or railroad", "Proximity to road or railroad (2nd)",
    "Building type", "House style",
    "Overall quality", "Overall condition", "Year built", "Year of remodeling",
    "Roof style", "Roof material", "Exterior 1st material", "Exterior 2nd material",
    "Masonry veneer type", "Masonry Veneer Area", "Exterior quality", "Exterior condition",
    "Foundation type", "Basement Height", "Basement condition", "Basement exposure",
    "Presence of Type 1 finished basement surface", "Area of finished basement Type 1",
    "Presence of Type 2 finished basement surface", "Area of finished basement Type 2",
    "Surface of unfinished basement", "Total basement area", "Heating type", "Heating quality",
    "Central air", "Electrical system", "First floor area", "Second floor area",
    "Low Quality Finished Area", "Above ground living area", "Full bathroom in basement", "Half bathroom in basement",
    "Full baths", "Half baths", "Bedrooms above ground", "Kitchens above ground",
    "Kitchen quality", "Total rooms above ground", "Functionality", "Fireplaces",
    "Fireplace quality", "Garage type", "Garage finish", "Garage capacity", "Garage area",
    "Garage quality", "Garage condition", "Paved driveway",
    "Wood deck area", "Open porch area", "Enclosed porch area", "3-season porch area",
    "Screen porch area", "Pool area", "Pool quality", "Fence quality",
    "Miscellaneous feature", "Miscellaneous value", "Month sold", "Year sold",
    "Sale type", "Sale condition", "Sale price", "Longitude",
    "Latitude"
  )
  feature_descriptions <- c(
    "Identifies the type of dwelling involved in the sale, such as one-story, two-story, or split-level.",
    "Identifies the general zoning classification of the sale property, such as residential, commercial, etc.",
    "The linear feet of street connected to the property.",
    "The size of the lot in square feet.",
    "Type of road access to the property (gravel or paved).",
    "Type of alley access to the property (if applicable).",
    "The general shape of the lot, such as regular, irregular, or other shapes.",
    "The flatness or slope of the land (e.g., flat, hilly).",
    "Indicates the availability of various utilities like water, gas, and electricity.",
    "Configuration of the lot, such as whether it is a corner lot, inside lot, or cul-de-sac.",
    "The slope of the property, ranging from gentle to steep.",
    "The general neighborhood where the property is located, indicating its desirability and amenities.",
    "Proximity of the house to various road types or railroad tracks, indicating noise or traffic considerations.",
    "Proximity to secondary roads or railroads, describing the location of the property in relation to these features.",
    "Describes the type of building structure, such as single-family home, townhouse, or duplex.",
    "The style of the house, e.g., ranch, colonial, or modern.",
    "A general rating of the quality of the house based on construction and aesthetic features.",
    "The overall condition of the house, including aspects like maintenance and age.",
    "The year when the house was originally built.",
    "The year the house was last remodeled or updated.",
    "Describes the type of roof, such as gable, hip, or flat.",
    "Type of roofing material, e.g., asphalt, wood, or tile.",
    "The material used for the exterior covering of the house.",
    "The material used for a second layer of exterior covering (if applicable).",
    "Type of masonry veneer used on the house, such as stone, brick, or none.",
    "The area (in square feet) of the masonry veneer.",
    "Quality of the exterior material and finish.",
    "Condition of the exterior material and finish.",
    "Type of foundation used for the house, such as concrete, stone, or wood.",
    "Height of the basement, whether it's above ground or below, and the quality of the basement area.",
    "Condition of the basement area, such as whether it's finished or unfinished.",
    "Whether or not the basement has exposure to natural light, such as via windows or walkouts.",
    "Type of finishing in the basement, such as finished or partially finished.",
    "The square footage of finished area in the basement.",
    "The type of finishing for the second part of the basement (if applicable).",
    "The square footage of the second finished area in the basement.",
    "Square footage of the basement that is unfinished.",
    "Total square footage of the basement, combining both finished and unfinished areas.",
    "Type of heating system in the home, e.g., forced air, radiant, or geothermal.",
    "Quality of the heating system.",
    "Whether the home has central air conditioning.",
    "Type of electrical system in the home.",
    "First floor area",
    "Second floor area",
    "Square footage of low-quality finished area, often unfinished or poorly finished rooms.",
    "Square footage of living area above ground, excluding the basement.",
    "Number of full-baths in the basement.",
    "Number of half-baths in the basement.",
    "Number of full bathrooms in the house, with a tub and shower.",
    "Number of half-baths in the house.",
    "Number of bedrooms located above ground level.",
    "Number of kitchens located above ground level.",
    "Quality of the kitchen, ranging from low to excellent.",
    "Total number of rooms above ground, including bedrooms and living rooms.",
    "Describes how functional the property is in terms of layout, room placements, and flow.",
    "Number of fireplaces in the home.",
    "Quality of the fireplace, if one exists.",
    "Type of garage, such as attached, detached, or no garage.",
    "Finish quality of the garage, whether it's unfinished or finished.",
    "Capacity of the garage in terms of the number of cars it can hold.",
    "Total square footage of the garage.",
    "Quality of the garage, based on materials and construction.",
    "Condition of the garage, including any wear or damage.",
    "Indicates whether the driveway is paved or not.",
    "Square footage of the wood deck attached to the house.",
    "Square footage of the open porch area.",
    "Square footage of any enclosed porch area.",
    "Square footage of a three-season porch, typically one that is not heated.",
    "Square footage of any screened-in porch area.",
    "Square footage of the pool area.",
    "Quality of the pool area, if applicable.",
    "Quality of the fence surrounding the property.",
    "Any miscellaneous feature of the property, such as a shed or a satellite dish.",
    "Value of any miscellaneous feature present on the property.",
    "The month when the sale took place, represented as a numeric value.",
    "The year when the sale took place.",
    "Type of sale, such as a standard sale or foreclosure.",
    "Condition of the sale, including aspects like normal sale, foreclosure, or short sale.",
    "The final sale price of the house, the target variable in the dataset.",
    "Longitude position of the house.",
    "Latitude position of the house."
  )

  # Define Intermediate Concept names and labels
  def_voc_root_concept_name <- "SalesPrice_def_voc"
  def_voc_root_concept_label <- "House Sales Price"
  IC_names <- c(def_voc_root_concept_name, "House condition", "Basement", "Garage", "Lot", "Access",
                "House type", "House aesthetics", "Porch", "Miscellaneous")
  IC_labels <- c(def_voc_root_concept_label, "House condition", "Basement", "Garage", "Lot", "Access",
                 "House type", "House aesthetics", "Porch", "Miscellaneous")

  # Add Intermediate Concept vertices first, better for UI
  g <- add_vertices(g, length(IC_names), name = IC_names, label = IC_labels, type = "Intermediate concept")
  g <- add_vertices(g, length(data_names), label = feature_names,
                    name = data_names, description = feature_descriptions,
                    type = "Feature")

  # Add "feature_of" edges
  house_features <- c("Garage","Basement", "Lot", "Access", "House type",
                      "House aesthetics", "House condition", "Porch", "First_Flr_SF",
                      "Gr_Liv_Area") #, "Miscellaneous")
  house_feature_edges <- interleave.value.in.array(def_voc_root_concept_name, house_features)
  garage_features <- c("Garage_Type", "Garage_Finish", "Garage_Cars",
                       "Garage_Area", "Garage_Qual", "Garage_Cond")
  garage_feature_edges <- interleave.value.in.array("Garage", garage_features)
  house_condition_features <- c("Overall_Qual", "Year_Built", "Overall_Cond",
                                "Year_Remod_Add", "Exter_Qual",
                                "Exter_Cond")
  house_condition_edges <- interleave.value.in.array("House condition", house_condition_features)
  basement_features <-
    c("Total_Bsmt_SF", "Bsmt_Unf_SF",
      "BsmtFin_SF_1", "BsmtFin_SF_2",
      "BsmtFin_Type_1", "BsmtFin_Type_2",
      "Bsmt_Qual", "Bsmt_Cond", "Bsmt_Full_Bath",
      "Bsmt_Half_Bath", "Bsmt_Exposure")
  basement_edges <- interleave.value.in.array("Basement", basement_features)
  lot_features <-
    c("Lot_Frontage",
      "Lot_Area",
      "Lot_Shape",
      "Land_Contour",
      "Utilities",
      "Lot_Config",
      "Land_Slope")
  lot_edges <- interleave.value.in.array("Lot", lot_features)
  access_features <-
    c("Condition_1",
      "Condition_2")
  access_edges <- interleave.value.in.array("Access", access_features)
  house_type_features <-
    c("MS_SubClass",
      "Bldg_Type",
      "House_Style",
      "Roof_Style")
  house_type_edges <- interleave.value.in.array("House type", house_type_features)
  house_aesthetics_features <-
    c("Roof_Matl", "Exterior_1st", "Exterior_2nd", "Mas_Vnr_Type", "Mas_Vnr_Area")
  house_aesthetics_edges <- interleave.value.in.array("House aesthetics",
                                                      house_aesthetics_features)
  porch_features <- c("Open_Porch_SF", "Enclosed_Porch", "Three_season_porch",
                      "Screen_Porch")
  porch_edges <- interleave.value.in.array("Porch", porch_features)

  # Create all "feature-of" edges
  g <- add_edges(g, c(house_feature_edges, house_condition_edges, basement_edges,
                      garage_feature_edges, lot_edges, access_edges,
                      house_type_edges, house_aesthetics_edges, porch_edges),
                 relation="feature-of")

  # Add all remaining features to "Miscellaneous"
  miscellaneous_features <- get_orphan_nodes(g)
  miscellaneous_features <- miscellaneous_features[miscellaneous_features != target_variable]
  miscellaneous_edges <- interleave.value.in.array("Miscellaneous", miscellaneous_features)
  g <- add_edges(g, miscellaneous_edges, relation="feature-of")

  # Remove potential orphans. Normally there's only the target_variable node left.
  g <- delete_vertices(g, V(g)[degree(g) == 0])

  # Default vocabulary
  pmodel <- list(
    name = "Default",
    root_concepts = def_voc_root_concept_name,
    graph = g
  )
  partner.models <- list()
  partner.models[[pmodel$name]] <- pmodel

  # Default vocabulary + Miscellaneous feature.
  pmodel <- list(
    name = "Def.+Misc.",
    root_concepts = c(def_voc_root_concept_name, "Miscellaneous"),
    graph = g
  )
  partner.models[[pmodel$name]] <- pmodel

  # We still add the "Bob" model
  pmodel <- create.Ames.Bob.pmodel(g)
  partner.models[[pmodel$name]] <- pmodel

  return(partner.models)
}

# Create the "Bob" partner model, which is a very simple model of someone
# who ony cares about surface sizes.
create.Ames.Bob.pmodel <- function(g) {
  bob_voc_root_name <- "SalesPrice_Bob"

  # Define Intermediate Concept names and labels
  bob_IC_names <- c(bob_voc_root_name, "Surfaces")
  bob_IC_labels <- c("House Sale Price", "Surfaces")

  # Add Intermediate Concept vertices first, better for UI
  g <- add_vertices(g, length(bob_IC_names), name = bob_IC_names, label = bob_IC_labels,
                    type = "Intermediate concept")

  # We don't need to add basic feature vertices, they should be in the "g"
  # graph already.

  # Add "feature_of" edges
  bob_house_features <- c("Surfaces")
  bob_house_feature_edges <- interleave.value.in.array(bob_voc_root_name, bob_house_features)
  bob_surfaces_features <- c("Garage_Area", "Total_Bsmt_SF", "Lot_Area")
  bob_surfaces_feature_edges <- interleave.value.in.array("Surfaces", bob_surfaces_features)

  # Create all "feature-of" edges
  g <- add_edges(g, c(bob_house_feature_edges, bob_surfaces_feature_edges),
                 relation="feature-of")

  # Remove orphans
  orphan_vertices <- V(g)[degree(g) == 0]
  g <- delete_vertices(g, orphan_vertices)

  # Add this vocabulary to the XAI_vocabularies global variable
  pmodel <- list(
    name = "Bob",
    root_concepts = bob_voc_root_name,
    graph = g
  )
  return(pmodel)
}

# Create a new array with `value` interleaved at every second position.
interleave.value.in.array <- function(value, array, value_first = TRUE) {
  if ( value_first )
    inds <- c(2,1)
  else
    inds <- c(1,2)
  new_array <- rep(NA, length(array) * 2)  # Allocate space
  new_array[seq(inds[1], length(new_array), by = 2)] <- array  # Fill original values
  new_array[seq(inds[2], length(new_array), by = 2)] <- value  # Fill inserted values
  return(new_array)
}

# Get names of nodes/vertices of a certain type that are orphans
get_orphan_nodes <- function(g, type_value = "Feature") {
  feature_vertices <- which(V(g)$type == type_value)
  orphan_vertices <- which(degree(g) == 0)
  nodes <- intersect(feature_vertices, orphan_vertices)
  return(V(g)$name[nodes])
}

