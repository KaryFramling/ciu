# Shiny apps.
#
# Kary Främling, created in 2025
#

# This is to get the CRAN check to go through. Thanks ChatGPT.
utils::globalVariables(c("color_palette", "g", "name", ".to", ".from", "relation",
                         "renderUI", "req", "tags", "uiOutput", "visNetworkOutput"))

#' Launch the Social XAI Shiny app
#'
#' @param parameters [list] that has to have at least the following elements:
#' - model: The model to explain.
#' - data: [data.frame] with the actual data.
#' - target_variable: Name of target variable column.
#' - ciu: CIU explainer.
#' - partner_models: [list] of "partner models", where each model has at least
#   a "name" and a "root_concepts" that is the name (or array of names) of the
#  root concept(s) in the main graph for the vocabulary.
#' - NO: graph: Default knowledge graph that defines vocabulary etc.
#' @param ... Passed directly to shinyApp().
#'
#' @import magrittr
#'
#' @returns Void
#' @export
ciu.launch.social_xai.shiny <- function(parameters, ...) {
  # Check that the necessary packages are installed.
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' needed for this function. Please install it.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' needed for this function. Please install it.")
  }
  if (!requireNamespace("shinyBS", quietly = TRUE)) {
    stop("Package 'shinyBS' needed for this function. Please install it.")
  }

  # Check that we have all the required variables before launching.
  required_vars <- c("model", "data", "target_variable", "ciu", "partner_models")
  missing <- setdiff(required_vars, names(parameters))
  if (length(missing) == 0) {
    # All names present
  } else {
    stop("Missing required elements: ", paste(missing, collapse = ", "))
  }

  # OK, we extract the variables
  model <- parameters$model
  data <- parameters$data
  target_variable <- parameters$target_variable
  ciu <- parameters$ciu
  partner_models <- parameters$partner_models

  # We need to determine what output to explain if there are more than one
  ciu_obj <- ciu$as.ciu()
  if ( length(ciu_obj$output.names) > 1 ) {
    if ( is.null(parameters$output_name) )
      output_name <- ciu_obj$output.names[1]
    else
      output_name <- parameters$output_name
  }
  else
    output_name <- ciu_obj$output.names
  output_value_index <- which(ciu_obj$output.names == output_name)

  # Check that we have at least one valid vocabulary
  if (length(partner_models) < 1) {
    stop("Error: There has to be at least one partner_model.")
  }
  graph <- ciu.get.sub.graph(partner_models[[1]]$graph, partner_models[[1]]$root_concepts)

  # # Force the app to open in an external browser
  # options(shiny.launch.browser = TRUE)

  # Initialize information for navigating through instances.
  max_inst_ind <- nrow(data)

  # Extract names of input columns
  in_col_names <- setdiff(names(data), target_variable)

  # Make visNetwork tooltips
  igraph::V(graph)$title <- igraph::V(graph)$label
  igraph::E(graph)$title <- igraph::E(graph)$relation

  # Set initial value of "graph".
  viz_graph <- graph
  min_CI_radius <- 1

  # Set colors and options for coloring graph nodes.
  node_border_color <- "black"
  default_node_color <- "lightblue"
  default_node_border_color <- "#2B7CE9"

  # Memorize what kind of plot we had selected for IC plot.
  comparative_plot_type <- "PI-plot"

  # # App-specific functions. This function has to be here, rather than calling
  # # "ciu$meta.explain" directly, otherwise there's an error (don't remember
  # # what was the reason for that thouhg - but at least it's solved.)
  # meta.explain <- function(instance, target.concept, concepts.to.explain, n.samples = 1000) {
  #   return(ciu$meta.explain(instance,
  #                           target.concept = target.concept,
  #
  #                           concepts.to.explain=concepts.to.explain,
  #                           n.samples = n.samples))
  # }

  # Define UI
  ui <- shiny::fluidPage(
    # tags$head(
    #   tags$script(HTML("
    #     $(document).on('shiny:connected', function() {
    #       window.resizeTo(800, 600); // Set the desired width and height
    #     });
    #   "))
    # ),
    shiny::titlePanel("Interactive XAI with CIU"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("selected_partner_model", "Partner model:",
                           choices = sapply(partner_models, function(x) x$name)),
        shinyBS::bsTooltip("selected_partner_model",
                           "A partner model has its own vocabulary and interaction preferences that can be adjusted for each explainee.",
                           placement = "right", trigger = "hover"),
        shiny::selectInput("selected_node_list", "Select feature/concept:", choices = igraph::V(viz_graph)$name),
        shinyBS::bsTooltip("selected_node_list",
                           "The explained feature or concept can be selected from this list or from the graph.",
                           placement = "right", trigger = "hover"),
        shiny::HTML("<b>Description of feature/concept</b>"),
        shiny::htmlOutput("node_definition"),
        shiny::hr(),
        shiny::h3("Graph options"),
        shiny::radioButtons("graph_visualisation_type", "Adjust size and color by:",
                     c("None" = "none",
                       "CI&CU" = "ciu",
                       "Influence" = "influence"),
                     selected = "none",
                     inline = TRUE),
        shinyBS::bsTooltip("graph_visualisation_type",
                           paste("Adjust node size and color by CIU results.",
                                 "This can take long if there are many Intermediate Concepts (IC).",
                                 "If so, then it can be made faster by reducing the number of",
                                 "samples to use for IC estimation."),
                           placement = "right", trigger = "hover"),
        shiny::numericInput("nsamples_IC", "# samples for IC est.", min = 100, step = 100, value = 1000),
        shinyBS::bsTooltip("nsamples_IC", "The number of samples to use for estimating CIU of Intermediate Concepts",
                           placement = "right", trigger = "hover"),
        shiny::checkboxInput("graph_hierarchical", "Graph as tree", value = FALSE),
        width = 3
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(
            8,
            # Wrap visNetwork in a div with custom gradient legend
            tags$div(
              style = "position: relative; width: 100%; height: 600px;",
              visNetworkOutput("network", height = "100%", width = "100%"),
              uiOutput("color_legend")
            )            # # Wrap visNetwork in a div with custom gradient legend
            # tags$div(
            #   style = "height: 600px;",
            #   shiny::uiOutput("network", height = "600px")
            # )
            #visNetwork::visNetworkOutput("network", height = "600px")
          ),
          shiny::column(
            4,
            shiny::numericInput("instance", "Instance #:", min = 1, value = 1, max = max_inst_ind),
            shiny::htmlOutput("instance_information"),
            shiny::uiOutput("xai_plot"),
            shiny::tags$style(shiny::HTML("
            #text_explanation {
              width: 100%;
              height: 200px;
              overflow-y: scroll;
              border: 1px solid #ccc;
              padding: 10px;
              background-color: #f9f9f9;
            }
          ")),
            shiny::htmlOutput("text_explanation")
          )
        ),
        width = 9
      )
    )
  )

  # Define server
  server <- function(input, output, session) {

    # We want changes to these values to cause dependent Shiny elements to update.
    reactives <- shiny::reactiveValues(
      partner = partner_models[[1]],
      instance = NULL,
      viz_graph = viz_graph,
      meta_ciu = NULL,
      visnet = NULL,
      ciu_colored_vis = NULL,
      contrastive_instance_2 = min(2, max_inst_ind)
    )

    # Update ciu result if instance or concept has changed.
    shiny::observe({
      # We have to check that both graph and selected_node_list have been updated
      # if ever there has been a change in partner model.
      shiny::req(input$selected_node_list %in% igraph::V(reactives$viz_graph)$name)

      # Everything should be OK and we can go ahead.
      reactives$instance <- data[input$instance, in_col_names]
      target_concept <- concept <- input$selected_node_list
      g <- reactives$viz_graph

      # We don't want to have a target concept for top-most nodes.
      if ( length(igraph::E(g)[.to(target_concept) & relation=="feature-of"]) == 0 )
        target_concept <- NULL

      # Different plots for ICs and basic features.
      if ( igraph::V(g)[concept]$type == "Intermediate concept") {
        child_features <- ciu.kg.get.child.features(g, concept)
        reactives$meta_ciu <- ciu$meta.explain(reactives$instance, target.concept=target_concept, concepts.to.explain=child_features, n.samples = input$nsamples_IC)
      }
      else
        reactives$meta_ciu <- NULL
    })

    # Render visNetwork graph
    output$network <- visNetwork::renderVisNetwork({
      if ( input$graph_visualisation_type != "none" ) {
        use_influence <- ifelse(input$graph_visualisation_type == "ciu", FALSE, TRUE)
        if ( is.null(reactives$ciu_colored_vis )) {
          ciu_g <- reactives$viz_graph
          for ( root_concept in reactives$partner$root_concepts )
            ciu_g <- ciu.igraph.explain(reactives$instance, ciu, ciu_g,
                                        root_concept, out_index = output_value_index)
          reactives$ciu_colored_vis <-
            ciu.plots.visNetwork_CIU(ciu_g, use.influence = use_influence,
                                     min_radius = min_CI_radius,
                                     hierarchical_layout = input$graph_hierarchical)
        }
        visnet <- reactives$ciu_colored_vis
      }
      else {
        vis <- visNetwork::toVisNetworkData(reactives$viz_graph)
        visnet <- visNetwork::visNetwork(vis$nodes, vis$edges) %>%
          visNetwork::visNodes(size = 30, font = list(size = 35))
      }
      visnet <- visnet %>%
        visNetwork::visOptions(highlightNearest = TRUE) %>% #, selectedBy = "id") %>%
        visNetwork::visEvents(selectNode = "function(nodes) {
        Shiny.setInputValue('selected_node_id', nodes.nodes[0]);
      }")

      # Hierarchical?
      if ( input$graph_hierarchical ) {
        visnet <- visnet %>%
          visNetwork::visHierarchicalLayout()  # Apply hierarchical (tree) layout
      }
      reactives$visnet <- visnet
      return(visnet)
    })

    # Display color legend depending on the visualisation mode of the network.
    output$color_legend <- renderUI({
      req(input$graph_visualisation_type)

      if (input$graph_visualisation_type == "ciu") {
        # Gradient legend
        tags$div(
          style = "position: absolute; right: 20px; top: 80px; width: 30px; height: 200px; z-index: 1000;",
          tags$div(
            style = paste0(
              "width: 100%; height: 100%; ",
              #"background: linear-gradient(to top, lightblue, orange, red); ",
              "background: linear-gradient(to top, red, yellow, #006400); ",
              "border: 1px solid black; border-radius: 3px;"
            )
          ),
          tags$div(style = "position: absolute; left: 35px; top: -5px; font-size: 12px;", "1.0"),
          tags$div(style = "position: absolute; left: 35px; top: 95px; font-size: 12px;", "0.5"),
          tags$div(style = "position: absolute; left: 35px; bottom: -5px; font-size: 12px;", "0.0"),
          tags$div(
            style = "position: absolute; left: 0px; top: -25px; font-weight: bold; font-size: 13px;",
            "CU"
          )
        )
      } else if (input$graph_visualisation_type == "influence") {
        # Only two colors
        tags$div(
          style = "position: absolute; right: 20px; top: 80px; z-index: 1000; background: white; padding: 10px; border: 1px solid black; border-radius: 3px;",
          tags$div(style = "font-weight: bold; font-size: 13px; margin-bottom: 8px;", "Influence"),
          tags$div(
            style = "display: flex; align-items: center; margin-bottom: 5px;",
            tags$div(style = "width: 20px; height: 20px; background: #4682B4; border: 1px solid black; margin-right: 8px; border-radius: 3px;"),
            tags$span(style = "font-size: 12px;", ">= 0")
          ),
          tags$div(
            style = "display: flex; align-items: center;",
            tags$div(style = "width: 20px; height: 20px; background: #B22222; border: 1px solid black; margin-right: 8px; border-radius: 3px;"),
            tags$span(style = "font-size: 12px;", "< 0")
          )
        )
      } else {
        # No legend
        NULL
      }
    })

    # Change the partner model. This needs to be reflected in the graph, as well
    # as in the concept menu, as well as resetting CIU results (and potentially
    # other things too).
    shiny::observeEvent(input$selected_partner_model, {
      # Initialize reactives$viz_graph either directly to partner model's igraph
      # or extract the graph as a subgraph from the "default" one.
      reactives$partner <- partner_models[[input$selected_partner_model]]
      g <- ciu.get.sub.graph(reactives$partner$graph, reactives$partner$root_concepts)

      # We have to re-create CIU with new vocabulary.
      ciu_obj <- ciu$as.ciu()
      graph <- ciu.get.sub.graph(reactives$partner$graph, reactives$partner$root_concepts)
      ciu <- ciu.new(ciu_obj$model, ciu_obj$formula, ciu_obj$data, knowledge.graph = graph)

      # Update list of concepts
      shiny::updateSelectInput(session, "selected_node_list",
                               choices = igraph::V(g)$name,
                               selected = reactives$partner$root_concepts[1])

      # Launch reactive.
      reactives$viz_graph <- g

      # Reset ciu results
      reactives$meta_ciu <- NULL
      reactives$ciu_colored_vis <- NULL
    })

    # visNetwork visualisation mode changed, force update of visNetwork
    shiny::observeEvent(input$graph_visualisation_type, {
      reactives$ciu_colored_vis <- NULL
    })

    # Here we need to reset CI values in the graph, as well as in the visNetwork
    shiny::observeEvent(input$instance, {
      # if ("utility" %in% vertex_attr_names(reactives$viz_graph)) {
      #   reactives$viz_graph <- delete_vertex_attr(reactives$viz_graph, "utility")
      # }
      reactives$ciu_colored_vis <- NULL
    })

    # React to vertex clicks
    shiny::observeEvent(input$selected_node_id, {
      shiny::updateSelectInput(session, "selected_node_list", selected = input$selected_node_id)
    })

    # We have to force an update of visNetwork when this happens
    shiny::observeEvent(input$graph_hierarchical, {
      reactives$visnet <- NULL
      reactives$ciu_colored_vis <- NULL
    })

    # Set selection for visNetwork
    shiny::observe({
      visNetwork::visNetworkProxy("network") %>%
        visNetwork::visSelectNodes(input$selected_node_list)
    })

    # Show description/definition of node
    output$node_definition <- shiny::renderText({
      # We have to check that both graph and selected_node_list have been updated
      # if ever there has been a change in partner model.
      shiny::req(input$selected_node_list %in% igraph::V(reactives$viz_graph)$name)

      # Now we can go ahead
      g <- reactives$viz_graph
      node <- igraph::V(g)[input$selected_node_list]
      description <- node$description
      if ( is.na(description) || is.null(description) ) {
        if ( !(is.na(node$type) || is.null(node$type)) && node$type == "Intermediate concept" )
          description = "This is an Intermediate Concept"
        else
          description <- "No description for selected node."
      }
      else {
        data_col <- data[,igraph::V(g)[input$selected_node_list]$name]
        if ( is.factor(data_col) ) {
          vals <- unlist(levels(data_col))
          description <- paste(description, "<br>This is a <b>categorical</b> feature.<br>",
                               "Possible values are: ",
                               paste(vals, collapse = ", "))
        }
        else if ( is.numeric(data_col) ) {
          description <- paste(description, "<br>This is a <b>numeric</b> feature.<br>")
        }
      }
      return(description)
    })

    # Show information about the instance, predicted output value
    output$instance_information <- shiny::renderText({
      shiny::req(input$instance)
      ciu_obj <- ciu$as.ciu()
      min_out_val <- ciu_obj$abs.min.max[output_value_index, 1]
      max_out_val <- ciu_obj$abs.min.max[output_value_index, 2]
      outval <- ciu_obj$predict.function(model, reactives$instance)[output_value_index]
      outval <- round(outval, 2) # Will do for all cases for the moment
      s <- paste0("Predicted output value: ", outval,
                  " is at ",
                  signif(((outval - min_out_val)/(max_out_val - min_out_val))*100, 2),
                  "% of the possible output value interval [", min_out_val, ",",
                  max_out_val, "]."
      )
    })

    # Memorize what plot type was selected so that we can restore it for the
    # dynamic IC plot after change of selected concept or whatever.
    shiny::observeEvent(input$comparative_plot_type, {
      comparative_plot_type <<- input$comparative_plot_type
    })

    # Render the dynamic UI for CIU bar plot explanations
    output$xai_plot <- shiny::renderUI({
      # We have to check that both graph and selected_node_list have been updated
      # if ever there has been a change in partner model.
      shiny::req(input$selected_node_list %in% igraph::V(reactives$viz_graph)$name)

      # Now we should be fine to go ahead
      g <- reactives$viz_graph
      concept <- input$selected_node_list
      if ( igraph::V(g)[concept]$type == "Intermediate concept") {
        shiny::verticalLayout(
          shiny::radioButtons(inputId = "comparative_plot_type",
                              label = "Visualisation type:",
                              choices = c("PI-plot", "Influence", "Contrastive"),
                              selected = comparative_plot_type,
                              inline = TRUE
          ),
          shiny::uiOutput("xai_plot_options"),
          shiny::plotOutput("ciuplot_output", height = "300px")
        )
      }
      else {
        shiny::verticalLayout(
          shiny::checkboxInput(inputId = "illustrate_ciu", label = "Illustrate CIU?", value = TRUE),
          shiny::plotOutput("ciuplot_output", height = "300px")
        )
      }
    })

    # Memorize what instance id the second to use for contrastive comparisons.
    shiny::observeEvent(input$contrastive_instance_2, {
      reactives$contrastive_instance_2 <- input$contrastive_instance_2
    })

    # Render the dynamic UI for the parameters of CIU plot explanations
    output$xai_plot_options <- shiny::renderUI({
      if (input$comparative_plot_type == "Contrastive") {
        shiny::numericInput("contrastive_instance_2", "Compare with Instance #:", min = 1,
                            value = reactives$contrastive_instance_2, max = max_inst_ind)
      }
    })

    # Bar plot explanation for ICs, IO plot for basic features.
    output$ciuplot_output <- shiny::renderPlot({
      # We have to check that both graph and selected_node_list have been updated
      # if ever there has been a change in partner model.
      shiny::req(input$selected_node_list %in% igraph::V(reactives$viz_graph)$name)
      shiny::req(!is.null(input$comparative_plot_type))

      # Now we should be fine to go ahead
      # Different plots for ICs and basic features.
      g <- reactives$viz_graph
      concept <- input$selected_node_list
      if ( igraph::V(g)[concept]$type == "Intermediate concept") {
        shiny::req(reactives$meta_ciu)
        if (input$comparative_plot_type == "Contrastive") {
          inst2_ind <- reactives$contrastive_instance_2
          instance2 <- data[inst2_ind, in_col_names]
          target_concept <- concept <- input$selected_node_list
          if ( length(igraph::E(g)[.to(target_concept) & relation=="feature-of"]) == 0 )
            target_concept <- NULL
          child_features <- ciu.kg.get.child.features(g, concept)
          meta_ciu2 <- ciu$meta.explain(instance2, target.concept=target_concept, concepts.to.explain=child_features, n.samples = input$nsamples_IC)
          ciuvals1 <- ciu.list.to.frame(reactives$meta_ciu$ciuvals, out.ind = output_value_index)
          ciuvals2 <- ciu.list.to.frame(meta_ciu2$ciuvals, out.ind = output_value_index)
          contrastive <- ciu.contrastive(ciuvals1, ciuvals2)
          p <- ciu.ggplot.contrastive(reactives$meta_ciu, contrastive) +
            theme(legend.position = "none") +
            labs(title = paste0("Instance ", input$instance, " vs. instance ", inst2_ind))
        }
        else {
          use_influence <- ifelse(input$comparative_plot_type == "PI-plot", FALSE, TRUE)
          # Apparently this doesn't go as we hope if we set output.names
          # for models with only one output. Maybe something to fix
          # in ggplot.col.ciu in the future.
          if ( length(ciu$as.ciu()$output.names) == 1 )
            oname <- NULL
          else
            oname <- output_name
          p <- ciu$ggplot.col.ciu(ciu.meta = reactives$meta_ciu, output.names = oname,
                                  plot.mode = "overlap", use.influence = use_influence)
        }
      }
      else {
        instance <- data[input$instance, in_col_names]

        # We have to deal with the case that the input hasn't been rendered yet.
        if (is.null(input$illustrate_ciu))
          illustrate.ciu <- TRUE
        else
          illustrate.ciu <- input$illustrate_ciu
        p <- ciu$ggplot.ciu(instance, which(colnames(instance) == concept),
                            ind.output = output_value_index, illustrate.CIU = illustrate.ciu)
      }
      return(p)
    })

    # Show textual explanation
    output$text_explanation <- shiny::renderText({
      # We have to check that both graph and selected_node_list have been updated
      # if ever there has been a change in partner model.
      shiny::req(input$selected_node_list %in% igraph::V(reactives$viz_graph)$name)

      # Now we should be fine to go ahead
      # Different plots for ICs and basic features.
      g <- reactives$viz_graph

      # Different explanations for ICs and basic features.
      concept <- input$selected_node_list
      if ( igraph::V(g)[concept]$type == "Intermediate concept") {
        shiny::req(reactives$meta_ciu)
        s <- paste("<b>Textual explanation</b><br>",
                   ciu$textual(ciu.meta = reactives$meta_ciu, ind.output = output_value_index))
      }
      else {
        instance <- data[input$instance, in_col_names] #subset(data[input$instance,], select=-Sale_Price)
        inp_ind <- which(colnames(instance) == concept)
        ciuvals <- ciu$explain(instance, ind.inputs.to.explain = inp_ind)
        ciuvals <- ciuvals[output_value_index,]
        s <- paste0(
          "The feature <b>", igraph::V(g)[concept]$label, "</b>'s importance is ",
          signif(ciuvals$CI*100, 3), "%. The utility of the feature's value <i>",
          instance[,inp_ind],
          "</i> is ", signif(ciuvals$CU*100, 3), "%.<br>",
          "The feature's influence is ", signif(ciu$influence(ciuvals), 3),
          " when 'neutral' utility is ", 0.5, ".<br><br>"
        )
        s <- paste(
          s,
          "The importance, utility and influence values can be 'read' directly",
          "from the <i>Input-Output (IO) plot</i> above. An IO plot shows how the output value ",
          "changes as a function of the selected input/feature value, ",
          "while maintaining the values of other features fixed at the",
          "ones of the studied instance.<br>",
          "The plot shows the whole range of possible output values on the y-axis,",
          "as indicated by 'MIN', 'MAX' and the corresponding blue lines.",
          "If the output value changes a lot when modifying the feature",
          "value (as indicated by the red 'ymin' and green 'ymax' lines)",
          "then the feature is important. <br>",
          "The current input value and the corresponding output value",
          "are indicated by the red dot in the plot. If the red dot is",
          "in the upper part of the [ymin, ymax] range, then the input",
          "value has 'high utility' (i.e. is 'good', 'typical', ...)",
          "and, correspondingly, if it is in the lower part of the range,",
          "then it has low utility (i.e. is 'bad', not typical', ...).<br>",
          "The influence value is calculated as <i>importance*(utility - neutral)</i>",
          "and indicates to what extent the feature has a positive or negative",
          "influence on the result compared to a reference value 'neutral'.<br>",
          "Influence can also be used for contrastive explanations that compare",
          "two instances with each other."
        )
      }
      return(s)
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server, ...)
}
