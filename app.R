library(tidyverse)
library(shiny)
library(fs)

datasets <- c(
  "Narcissism" = "data/npi.Rds",
  "Big five: A" = "data/big5a.Rds",
  "Big five: B" = "data/big5b.Rds"
)

methods <- list(
  "Mean" = mean,
  "Median" = median,
  "Min" = min,
  "Max" = max,
  "Lower quartile" = function(x) quantile(x, probs = 0.25),
  "Upper quartile" = function(x) quantile(x, probs = 0.75),
  "range" = function(x) max(x) - min(x),
  "IQR" = IQR,
  "SD" = sd,
  "MAD" = mad,
  'n' = length
)

# ------------------------------------ UI -------------------------------------
ui <- fluidPage(
  titlePanel("Exploratory data analysis demo"),
  fluidRow(
    column(
      3,
      helpText("Choose a dataset and then, if this dataset has more than one numeric variable, select a variable to analyse.
               Choose a method of data visualization to visualize the distribution of values of the variable.
               Likwise, choose some quantitative methods to statistically summarize the variable.
               You may optionally subgroup your chosen variable according to some discrete grouping variable,
               and then you can visualize
               and summarize the values of the variable separately for each subgroup."),
      selectInput("dataset", "Choose a dataset:",
        choices = names(datasets)
      ),
      conditionalPanel(
        condition = "input.dataset != 'Narcissism'",
        radioButtons(
          "variable", "Select a variable:",
          c("Dummy Item A")
        ),
      ),
      selectInput("plot_type", "Select a plot type:",
        choices = c("Histogram", "Boxplot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        sliderInput(inputId = "bins", "Number of bins", min = 5, max = 25, value = 10),
      ),
      checkboxGroupInput("descriptives", "Select descriptives:",
        choices = names(methods),
        selected = "Mean"
      ),
      radioButtons(
        "grouping",
        "Select a grouping variable:",
        c("Dummy Item A", "Dummy Item B")
      ),
    ),
    column(
      6,
      mainPanel(
        plotOutput("distPlot"),
        tableOutput("table")
      )
    )
  )
)

# ---------------------------------- Server -----------------------------------
server <- function(input, output, session) {
  dataset_df <- reactive({
    readRDS(datasets[input$dataset])
  })

  numeric_dataset_df <- reactive({
    dataset_df() %>% select(where(is.numeric))
  })

  grouping_dataset_df <- reactive({
    dataset_df() %>% select(where(~ !is.numeric(.)))
  })

  observe({
    updateRadioButtons(session, "variable",
      choices = names(numeric_dataset_df()),
      selected = names(numeric_dataset_df())[1]
    )
    updateRadioButtons(session, "grouping",
      choices = c("None", names(grouping_dataset_df())),
      selected = "None"
    )
  })

  input_var <- reactive({
    # Immediately, the chosen variable is "Dummy Item A"
    if (!(input$variable %in% names(numeric_dataset_df()))) {
      # by default select first column of numeric dataset
      names(numeric_dataset_df())[1]
    } else {
      input$variable
    }
  })

  output$distPlot <- renderPlot({
    if (input$grouping == "None" | input$grouping == "Dummy Item A") {
      # without grouping variables

      p1 <- ggplot(
        data = dataset_df(),
        mapping = aes(x = .data[[input_var()]])
      ) +
        geom_histogram(bins = input$bins, colour = "white") +
        theme_classic() +
        scale_color_brewer()

      p2 <- ggplot(
        data = dataset_df(),
        mapping = aes(x = "", y = .data[[input_var()]])
      ) +
        geom_boxplot(width = 1 / 3) +
        coord_flip() +
        theme_classic() +
        xlab(NULL) +
        theme(axis.ticks = element_blank()) +
        scale_color_brewer()
    } else {
      # with grouping variables

      p1 <- ggplot(
        data = dataset_df(),
        mapping = aes(x = .data[[input_var()]])
      ) +
        geom_histogram(bins = input$bins, colour = "white") +
        facet_wrap(vars(.data[[input$grouping]])) +
        theme_classic() +
        scale_color_brewer()

      p2 <- ggplot(
        data = dataset_df(),
        mapping = aes(x = .data[[input$grouping]], y = .data[[input_var()]])
      ) +
        geom_boxplot(width = 1 / 3) +
        coord_flip() +
        theme_classic() +
        xlab(NULL) +
        theme(axis.ticks = element_blank()) +
        scale_color_brewer()
    }



    if (input$plot_type == "Histogram") {
      p1
    } else {
      p2
    }
  })

  output$table <- renderTable({
    if (input$grouping == "None" | input$grouping == "Dummy Item A") {
      # without grouping variables
      the_dataset <- dataset_df()
    } else {
      # with grouping variables
      the_dataset <- dataset_df() %>%
        group_by(.data[[input$grouping]])
    }

    the_dataset %>%
      summarise(across(all_of(input_var()),
        .fns = methods[input$descriptives]
      ) %>% rename_with(.fn = ~ str_remove_all(., pattern = "[a-z]+_")))
  })
}

# ---------------------------------- Run app ----------------------------------

shinyApp(ui = ui, server = server)
