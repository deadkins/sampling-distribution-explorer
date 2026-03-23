source("helpers.R", local = TRUE)

library(shiny)

default_state <- list(
  population = "sleep",
  estimator = "Mean",
  n = 30,
  reps = 1000,
  seed = 1234,
  normal_overlay = TRUE
)

population_cache <- new.env(parent = emptyenv())

get_population_cached <- function(key) {
  if (!exists(key, envir = population_cache, inherits = FALSE)) {
    assign(key, generate_population(key), envir = population_cache)
  }
  get(key, envir = population_cache, inherits = FALSE)
}

summary_card <- function(title, value, id = NULL) {
  div(
    class = "summary-card",
    if (!is.null(id)) id = id,
    div(class = "summary-label", title),
    div(class = "summary-value", value)
  )
}

ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css")
  ),
  titlePanel(
    div(
      class = "title-wrap",
      h1("Sampling Distribution Explorer"),
      p(
        class = "subtitle",
        "Synthetic social-science populations for learning; not real survey estimates."
      ),
      p(class = "byline", "Developed by Daniel E. Adkins")
    )
  ),
  div(
    class = "app-shell",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "population",
          "Population / variable",
          choices = population_choices(),
          selected = default_state$population
        ),
        selectInput(
          "estimator",
          "Estimator",
          choices = c("Mean", "Median"),
          selected = default_state$estimator
        ),
        sliderInput(
          "n",
          "Sample size (n)",
          min = 5,
          max = 250,
          value = default_state$n
        ),
        sliderInput(
          "reps",
          "Number of repeated samples",
          min = 10,
          max = 3000,
          step = 10,
          value = default_state$reps
        ),
        numericInput(
          "seed",
          "Random seed",
          value = default_state$seed,
          min = 1,
          step = 1
        ),
        checkboxInput(
          "normal_overlay",
          "Show normal curve overlay",
          value = default_state$normal_overlay
        ),
        div(
          class = "button-row",
          actionButton("run_sim", "Run simulation", class = "primary-btn"),
          actionButton("reset_defaults", "Reset defaults")
        ),
        div(
          class = "sidebar-note",
          strong("Interpretive note:"),
          " The population distribution, one sample, and the sampling distribution are different objects."
        ),
        textOutput("binary_note")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(
            width = 4,
            div(
              class = "info-card",
              h3(textOutput("var_name", inline = TRUE)),
              p(tags$strong("Units: "), textOutput("var_units", inline = TRUE)),
              p(tags$strong("Shape: "), textOutput("var_shape", inline = TRUE)),
              p(textOutput("var_rationale", inline = TRUE)),
              p(class = "muted-note", "Synthetic, illustrative population for teaching only.")
            )
          ),
          column(
            width = 8,
            div(
              class = "summary-grid",
              uiOutput("summary_true"),
              uiOutput("summary_true_se"),
              uiOutput("summary_avg"),
              uiOutput("summary_sd"),
              uiOutput("summary_cov")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(class = "plot-card", plotOutput("population_plot", height = 290))
          ),
          column(
            width = 6,
            div(class = "plot-card", plotOutput("sample_plot", height = 290))
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(class = "plot-card", plotOutput("sampling_plot", height = 310))
          ),
          column(
            width = 6,
            div(class = "plot-card", plotOutput("ci_plot", height = 310))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset_defaults, {
    updateSelectInput(session, "population", selected = default_state$population)
    updateSelectInput(session, "estimator", selected = default_state$estimator)
    updateSliderInput(session, "n", value = default_state$n)
    updateSliderInput(session, "reps", value = default_state$reps)
    updateNumericInput(session, "seed", value = default_state$seed)
    updateCheckboxInput(session, "normal_overlay", value = default_state$normal_overlay)
  })

  current_meta <- reactive({
    get_population_metadata(input$population)
  })

  output$var_name <- renderText(current_meta()$label)
  output$var_units <- renderText(current_meta()$units)
  output$var_shape <- renderText(current_meta()$shape)
  output$var_rationale <- renderText(current_meta()$rationale)
  output$binary_note <- renderText(binary_median_note(input$population, input$estimator))

  simulation_result <- eventReactive(input$run_sim, {
    population <- get_population_cached(input$population)
    sim <- simulate_repeated_samples(
      population = population,
      n = input$n,
      reps = input$reps,
      estimator = input$estimator,
      seed = input$seed,
      keep_last = 50L
    )
    true_value <- compute_true_parameter(population, input$estimator)
    true_se <- compute_true_se(population, input$n)
    ci_df <- compute_last20_cis(
      sim$last_samples,
      estimator = input$estimator,
      bootstrap_B = 300L
    )
    ci_df$contains_true <- ci_df$lower <= true_value & ci_df$upper >= true_value

    list(
      population = population,
      sim = sim,
      true_value = true_value,
      true_se = true_se,
      ci_df = ci_df
    )
  }, ignoreInit = FALSE)

  output$summary_true <- renderUI({
    res <- simulation_result()
    summary_card(
      "True population parameter",
      format_value(res$true_value, input$population)
    )
  })

  output$summary_avg <- renderUI({
    res <- simulation_result()
    summary_card(
      "Average of simulated sample estimates",
      format_value(mean(res$sim$estimates), input$population)
    )
  })

  output$summary_true_se <- renderUI({
    res <- simulation_result()
    summary_card(
      "True SE (population SD / sqrt(n))",
      format_value(res$true_se, input$population)
    )
  })

  output$summary_sd <- renderUI({
    res <- simulation_result()
    summary_card(
      "SD of simulated sample estimates",
      format_value(stats::sd(res$sim$estimates), input$population)
    )
  })

  output$summary_cov <- renderUI({
    res <- simulation_result()
    covered <- sum(res$ci_df$contains_true)
    summary_card("Coverage in last 50 CIs", sprintf("%d / %d", covered, nrow(res$ci_df)))
  })

  output$population_plot <- renderPlot({
    res <- simulation_result()
    op <- par(mar = c(4.9, 4.4, 2.8, 1.0))
    on.exit(par(op), add = TRUE)
    plot_population_distribution(res$population, input$population)
  }, res = 96)

  output$sample_plot <- renderPlot({
    res <- simulation_result()
    op <- par(mar = c(4.5, 4.4, 2.8, 1.0))
    on.exit(par(op), add = TRUE)
    plot_sample_distribution(res$sim$example_sample, input$population)
  }, res = 96)

  output$sampling_plot <- renderPlot({
    res <- simulation_result()
    op <- par(mar = c(5.4, 4.4, 3.2, 1.0))
    on.exit(par(op), add = TRUE)
    plot_sampling_distribution(
      estimates = res$sim$estimates,
      population = res$population,
      true_value = res$true_value,
      estimator = input$estimator,
      key = input$population,
      show_normal_curve = input$normal_overlay
    )
  }, res = 96)

  output$ci_plot <- renderPlot({
    res <- simulation_result()
    op <- par(mar = c(4.2, 4.8, 2.8, 1.0))
    on.exit(par(op), add = TRUE)
    plot_last20_cis(
      ci_df = res$ci_df,
      true_value = res$true_value,
      estimator = input$estimator
    )
  }, res = 96)
}

shinyApp(ui = ui, server = server)
