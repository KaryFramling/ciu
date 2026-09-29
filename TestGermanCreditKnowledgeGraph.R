# Tests to perform on KnowledgeGraph functionality of ciu package with
# German Credit data set.
#
# The hierarchies are proposed by Claude LLM, as well as the targeted end-user
# roles:
# For loan officers: Hierarchy 1 or 2 (familiar banking terms)
# For regulators: Hierarchy 2 (focuses on risk)
# For customers: Hierarchy 5 (simpler, more understandable)
# For sociologists/researchers: Hierarchy 3 (socio-economic factors)
#

# How to run this:
# Source the whole script. Then you can do e.g. the following:
# 1. Create partner models and visualizing their KG graph by calling:
#  pms <- create.GermanCredit.partner_models(german)
#  ciu.plots.visNetwork_CIU(pms[[1]]$graph)
# 2. Create a CIU explainer with RF model and partner models. Call:
#  ciu_german <- create.GermanCredit.CIU(create.GermanCredit.RF(), pms)
#  # Explanation without vocabulary:
#  print(ciu_german$ggplot.col.ciu(instance, sort="CI", output.names = "good", plot.mode="overlap"))
#  # Explanation with vocabulary:
#  p <- ciu_german$ggplot.col.ciu(instance, sort="CI", output.names = "good", concepts.to.explain=ciu.kg.get.child.features(pms[[1]]$graph, pms[[1]]$root_concepts[1]), plot.mode = "overlap"); print(p)
#  # Explain Intermediate Concept
#  p <- ciu_german$ggplot.col.ciu(instance, sort="CI", output.names = "good", target.concept = "Financial Capacity", concepts.to.explain=ciu.kg.get.child.features(pms[[1]]$graph, "Financial Capacity"), plot.mode = "overlap"); print(p)
# 3. Launch Shiny app from scratch:
#

library(caret)
library(rchallenge) # We use the German Credit data from here, not from caret.
library(igraph)
library(caret)
library(visNetwork)
library(htmltools)
library(shiny)
library(shinyBS)
library(ciu)

# Load German Credit data, included in "caret" package
data("german", package = "rchallenge")
target <- "credit_risk"
instance <- subset(german[2,], select=-credit_risk) # We choose an instance with good credit so that we know what we explain

# Create German Credit, partner models, CIU etc. and return them
# in a list that works for the Shiny app.
# Run for instance by "ciu.launch.social_xai.shiny(create.CIU_german_credit_for_Shiny())"
create.CIU_german_credit_for_Shiny <- function(model_index = 1, caret_model="rf") {
  pmodels <- create.GermanCredit.partner_models(german)
  ciu <- create.GermanCredit.CIU(create.GermanCredit.RF(), pmodels)
  ciuobj <- ciu$as.ciu()
  # Return a list with all the required information for launching the Shiny app.
  return(list(model=ciuobj$model, data=ciuobj$data, target_variable=target,
              ciu=ciu, partner_models=pmodels, output_name="good"))
}

# Get CIU object for trained ML model and the given partner models.
create.GermanCredit.CIU <- function(model, partner_models) {
  voc <- list()
  for ( pms in partner_models ) {
    voc <- c(voc, ciu.voc.from.graph(pms$graph, colnames(german)[colnames(german) != target]))
  }
  ciu.GermanCredit <- ciu.new(model, credit_risk~., german, vocabulary = voc)
  return(ciu.GermanCredit)
}

# Train Random Forest model. We don't care about training/test set here
# because that's not the main thing. And we do k-fold cross validation anyways.
create.GermanCredit.RF <- function() {
  kfoldcv <- trainControl(method="cv", number=10)
  GermanCredit.rf.caret <- train(credit_risk~., german, method="rf", trControl=kfoldcv)
}

# Set of knowledge graphs, initial partner models etc.
# Test:
# pms <- create.GermanCredit.partner_models(german)
# plot(ciu.get.sub.graph(pms$`Traditional Banking`$graph, pms$`Traditional Banking`$root_concepts))
create.GermanCredit.partner_models <- function(data, target_variable = "credit_risk") {
  # Feature names (short, readable versions)
  feature_names <- c(
    status = "Checking Account Status",
    duration = "Credit Duration",
    credit_history = "Credit History",
    purpose = "Credit Purpose",
    amount = "Credit Amount",
    savings = "Savings Account",
    employment_duration = "Employment Duration",
    installment_rate = "Installment Rate",
    personal_status_sex = "Marital Status and Gender",
    other_debtors = "Co-applicant/Guarantor",
    present_residence = "Residence Duration",
    property = "Property Ownership",
    age = "Age",
    other_installment_plans = "Other Installments",
    housing = "Housing Type",
    number_credits = "Existing Credits",
    job = "Job Type",
    people_liable = "Dependents",
    telephone = "Has Telephone",
    foreign_worker = "Foreign Worker",
    credit_risk = "Credit Risk"
  )

  # Feature descriptions (more detailed explanations)
  feature_descriptions <- c(
    status = "Status of existing checking account (balance level)",
    duration = "Duration of the credit in months",
    credit_history = "Credit payment history and behavior at this bank",
    purpose = "Purpose for which the credit is requested",
    amount = "Amount of credit requested in Deutsche Mark",
    savings = "Average balance in savings account or bonds",
    employment_duration = "Length of current employment relationship",
    installment_rate = "Installment rate as percentage of disposable income",
    personal_status_sex = "Personal status (marital status) and gender",
    other_debtors = "Presence of other debtors or guarantors for this credit",
    present_residence = "Number of years at present residence",
    property = "Type of property or assets owned by the applicant",
    age = "Age of the applicant in years",
    other_installment_plans = "Other installment plans at other banks or stores",
    housing = "Type of housing (rent, own, or free)",
    number_credits = "Number of existing credits at this bank",
    job = "Type of employment or job category",
    people_liable = "Number of people for whom applicant is financially liable",
    telephone = "Whether applicant has registered telephone under their name",
    foreign_worker = "Whether applicant is a foreign worker",
    credit_risk = "The target variable: 'bad' or 'good'"
  )

  # Define the partner models
  partner.models <- list()

  # Create the graph with only vertices
  g <- make_empty_graph()
  data_names <- colnames(data)

  # Add basic features to the graph
  g <- add_vertices(g, length(data_names), label = feature_names,
                    name = data_names, description = feature_descriptions,
                    type = "Feature")

  # Model 1: Traditional Banking Perspective

  # hierarchy_banking <- list(
  #   "Financial Capacity" = c("employment", "duration_in_current_job"),
  #   "Existing Obligations" = c("other_installment_plans", "number_credits",
  #                              "installment_rate", "credit_amount"),
  #   "Collateral & Assets" = c("property", "savings", "other_debtors"),
  #   "Credit History" = c("credit_history", "duration", "credit_amount", "purpose"),
  #   "Personal Stability" = c("age", "present_residence_since", "housing",
  #                            "telephone", "foreign_worker")
  # )
  root_concept_name <- "Credit Worthiness"
  root_concept_label <- "Credit Worthiness (i.e. for it being 'good')"
  IC_names <- c(root_concept_name, "Financial Capacity", "Existing Obligations",
                "Collateral & Assets", "Credit History", "Personal Stability")
  IC_labels <- c(root_concept_label, "Financial Capacity", "Existing Obligations",
                 "Collateral & Assets", "Credit History", "Personal Stability")

  # Add Intermediate Concept vertices
  g <- add_vertices(g, length(IC_names), name = IC_names, label = IC_labels, type = "Intermediate concept")

  # Add "feature_of" edges
  credit_worth_features <- c("Financial Capacity", "Existing Obligations",
                             "Collateral & Assets", "Credit History",
                             "Personal Stability")
  credit_worth_feature_edges <- interleave.value.in.array(root_concept_name, credit_worth_features)
  financial_features <- c("job", "employment_duration")
  financial_feature_edges <- interleave.value.in.array("Financial Capacity", financial_features)
  obligations_features <- c("other_installment_plans", "number_credits",
                            "installment_rate", "amount")
  obligations_feature_edges <- interleave.value.in.array("Existing Obligations", obligations_features)
  assets_features <- c("property", "savings", "other_debtors")
  assets_feature_edges <- interleave.value.in.array("Collateral & Assets", assets_features)
  history_features <- c("credit_history", "duration", "purpose")
  history_feature_edges <- interleave.value.in.array("Credit History", history_features)
  stability_features <- c("age", "present_residence", "housing",
                          "telephone", "foreign_worker")
  stability_feature_edges <- interleave.value.in.array("Personal Stability", stability_features)

  # Create all "feature-of" edges
  g <- add_edges(g, c(credit_worth_feature_edges, financial_feature_edges,
                      obligations_feature_edges, assets_feature_edges,
                      history_feature_edges, stability_feature_edges),
                 relation="feature-of")

  # Remove potential orphans. Normally there's only the target_variable node left.
  g <- delete_vertices(g, V(g)[degree(g) == 0])

  pmodel <- list(
    name = "Traditional Banking",
    root_concepts = root_concept_name,
    graph = g
  )
  partner.models[[pmodel$name]] <- pmodel

  # # Hierarchy 2: Risk Assessment View. NOTE: THE FEATURE NAMES GENERATED BY LLM ARE NOT CORRECT!
  # hierarchy_risk <- list(
  #   "Repayment Ability" = c("employment", "installment_rate", "duration_in_current_job"),
  #   "Repayment Willingness" = c("credit_history", "checking_account",
  #                               "other_installment_plans"),
  #   "Financial Buffer" = c("savings", "property", "checking_account"),
  #   "Financial Stress" = c("number_people_liable", "number_credits",
  #                          "installment_rate", "other_installment_plans"),
  #   "Stability Indicators" = c("duration_in_current_job", "present_residence_since", "age")
  # )

  # Create the graph with only vertices
  g <- make_empty_graph()
  data_names <- colnames(data)

  # Add basic features to the graph
  g <- add_vertices(g, length(data_names), label = feature_names,
                    name = data_names, description = feature_descriptions,
                    type = "Feature")

  # Start building the hierarchy
  root_concept_name <- "Credit Risk"
  root_concept_label <- "Credit Risk (i.e. for it being 'low')"
  IC_names <- c(root_concept_name, "Repayment Ability", "Repayment Willingness",
                "Financial Buffer", "Financial Stress", "Stability Indicators")
  IC_labels <- c(root_concept_label, "Repayment Ability", "Repayment Willingness",
                 "Financial Buffer", "Financial Stress", "Stability Indicators")

  # Add Intermediate Concept vertices
  g <- add_vertices(g, length(IC_names), name = IC_names, label = IC_labels, type = "Intermediate concept")

  # Add "feature_of" edges
  credit_risk_features <- c("Repayment Ability", "Repayment Willingness",
                            "Financial Buffer", "Financial Stress", "Stability Indicators")
  credit_risk_feature_edges <- interleave.value.in.array(root_concept_name, credit_risk_features)
  repayment_ability_features <- c("employment_duration", "installment_rate")
  repayment_ability_feature_edges <- interleave.value.in.array("Repayment Ability", repayment_ability_features)
  repayment_willingness_features <- c("credit_history", "status",
                                      "other_installment_plans")
  repayment_willingness_feature_edges <- interleave.value.in.array("Repayment Willingness", repayment_willingness_features)
  financial_buffer_features <- c("savings", "property")
  financial_buffer_feature_edges <- interleave.value.in.array("Financial Buffer", financial_buffer_features)
  financial_stress_features <- c("people_liable", "number_credits")
  financial_stress_feature_edges <- interleave.value.in.array("Financial Stress", financial_stress_features)
  stability_features <- c("present_residence", "age")
  stability_feature_edges <- interleave.value.in.array("Stability Indicators", stability_features)

  # Create all "feature-of" edges
  g <- add_edges(g, c(credit_risk_feature_edges, repayment_ability_feature_edges,
                      repayment_willingness_feature_edges, financial_buffer_feature_edges,
                      financial_stress_feature_edges, stability_feature_edges),
                 relation="feature-of")

  # Remove potential orphans. Normally there's only the target_variable node left.
  g <- delete_vertices(g, V(g)[degree(g) == 0])

  pmodel <- list(
    name = "Risk Assessment",
    root_concepts = root_concept_name,
    graph = g
  )
  partner.models[[pmodel$name]] <- pmodel

  return(partner.models)
}

# Create visNetwork for German Credit with CIU visualisation.
# pms <- create.GermanCredit.partner_models(german)
# get.partner_model.visNetwork(pms[[1]])
get.partner_model.visNetwork <- function(partner_model) {
  return(ciu.plots.visnetwork(partner_model$graph))
}

# Categorical value mappings for German Credit dataset
categorical_values <- list(

  checking_account = c(
    "A11" = "< 0 DM",
    "A12" = "0 <= ... < 200 DM",
    "A13" = ">= 200 DM / salary for at least 1 year",
    "A14" = "no checking account"
  ),

  credit_history = c(
    "A30" = "no credits taken / all credits paid back duly",
    "A31" = "all credits at this bank paid back duly",
    "A32" = "existing credits paid back duly till now",
    "A33" = "delay in paying off in the past",
    "A34" = "critical account / other credits existing (not at this bank)"
  ),

  purpose = c(
    "A40" = "car (new)",
    "A41" = "car (used)",
    "A42" = "furniture/equipment",
    "A43" = "radio/television",
    "A44" = "domestic appliances",
    "A45" = "repairs",
    "A46" = "education",
    "A47" = "vacation",
    "A48" = "retraining",
    "A49" = "business",
    "A410" = "others"
  ),

  savings = c(
    "A61" = "< 100 DM",
    "A62" = "100 <= ... < 500 DM",
    "A63" = "500 <= ... < 1000 DM",
    "A64" = ">= 1000 DM",
    "A65" = "unknown / no savings account"
  ),

  employment = c(
    "A71" = "unemployed",
    "A72" = "< 1 year",
    "A73" = "1 <= ... < 4 years",
    "A74" = "4 <= ... < 7 years",
    "A75" = ">= 7 years"
  ),

  personal_status_sex = c(
    "A91" = "male : divorced/separated",
    "A92" = "female : divorced/separated/married",
    "A93" = "male : single",
    "A94" = "male : married/widowed",
    "A95" = "female : single"
  ),

  other_debtors = c(
    "A101" = "none",
    "A102" = "co-applicant",
    "A103" = "guarantor"
  ),

  property = c(
    "A121" = "real estate",
    "A122" = "building society savings agreement / life insurance",
    "A123" = "car or other",
    "A124" = "unknown / no property"
  ),

  other_installment_plans = c(
    "A141" = "bank",
    "A142" = "stores",
    "A143" = "none"
  ),

  housing = c(
    "A151" = "rent",
    "A152" = "own",
    "A153" = "for free"
  ),

  job = c(
    "A171" = "unemployed / unskilled - non-resident",
    "A172" = "unskilled - resident",
    "A173" = "skilled employee / official",
    "A174" = "management / self-employed / highly qualified employee / officer"
  ),

  telephone = c(
    "A191" = "none",
    "A192" = "yes, registered under customer name"
  ),

  foreign_worker = c(
    "A201" = "yes",
    "A202" = "no"
  )
)

# Helper functions for Shiny app

# Get readable value for a categorical feature
get_categorical_value <- function(feature, code) {
  if (feature %in% names(categorical_values)) {
    mapping <- categorical_values[[feature]]
    return(ifelse(code %in% names(mapping), mapping[code], code))
  }
  return(code)
}

# Get all possible values for a feature (useful for dropdowns)
get_feature_levels <- function(feature) {
  if (feature %in% names(categorical_values)) {
    return(categorical_values[[feature]])
  }
  return(NULL)
}

# Check if feature is categorical
is_categorical <- function(feature) {
  return(feature %in% names(categorical_values))
}

# Get complete feature info including categorical mappings
get_complete_feature_info <- function(feature) {
  info <- list(
    feature_id = feature,
    name = feature_names[feature],
    description = feature_descriptions[feature],
    is_categorical = is_categorical(feature)
  )

  if (info$is_categorical) {
    info$levels <- categorical_values[[feature]]
    info$level_codes <- names(categorical_values[[feature]])
  } else {
    info$levels <- NULL
  }

  return(info)
}

# Format instance values with readable labels
format_instance_values <- function(instance_data) {
  formatted <- instance_data

  for (feature in names(instance_data)) {
    if (is_categorical(feature)) {
      formatted[[feature]] <- get_categorical_value(feature, instance_data[[feature]])
    }
  }

  return(formatted)
}


# Hierarchy 3: Socio-Economic Perspective
hierarchy_socioeconomic <- list(
  "Wealth" = c("savings", "property", "checking_account", "other_debtors"),
  "Employment Situation" = c("employment", "job", "duration_in_current_job"),
  "Family Situation" = c("personal_status_sex", "number_people_liable"),
  "Social Integration" = c("housing", "telephone", "present_residence_since",
                           "foreign_worker"),
  "Credit Behavior" = c("credit_history", "purpose", "credit_amount", "duration")
)

# Hierarchy 4: Customer Life Stage View
hierarchy_lifestage <- list(
  "Life Stability" = c("age", "personal_status_sex", "number_people_liable",
                       "present_residence_since"),
  "Career Status" = c("employment", "duration_in_current_job", "job"),
  "Financial Maturity" = c("credit_history", "savings", "property", "checking_account"),
  "Current Needs" = c("purpose", "credit_amount", "duration")
)

# Hierarchy 5: Simple Three-Category Model
hierarchy_simple <- list(
  "Income & Employment" = c("employment", "job", "duration_in_current_job",
                            "installment_rate"),
  "Financial Health" = c("savings", "property", "checking_account", "credit_history",
                         "other_installment_plans", "number_credits", "other_debtors"),
  "Personal Background" = c("age", "personal_status_sex", "number_people_liable",
                            "housing", "present_residence_since", "telephone",
                            "foreign_worker")
)

# Example usage with CIU
# ciu_result <- ciu.explain(instance, model,
#                           in.min.max.limits = input_ranges,
#                           concepts = hierarchy_banking)

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

