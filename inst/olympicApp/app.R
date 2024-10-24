library(shiny)
library(ggplot2)

ui <- fluidPage(

  theme = shinythemes::shinytheme("cerulean"),

  # Application title
  titlePanel("Gender Trends in the Olympics"),

  sidebarLayout(
    sidebarPanel(
      tags$img(src = "image.png", align= "center", height='60%', width='95%'),

      # Description of inputs
      p("Use the options below to explore gender trends in the Olympics based on season, year range, and sport."),

      # Select input for choosing season
      selectInput("season", "Select season: Choose Summer or Winter Olympics.", choices = c("Summer", "Winter"), selected = "Summer"),

      # Slider for year range
      sliderInput("yearRange", "Select year range: Filter the data based on a range of years.", min = 1900, max = 2016, value = c(1900, 2016), step = 4, sep = ""),

      # Placeholder select input for sports, initially empty
      selectInput("sport", "Choose sport: Select one or more sports to explore gender participation.", choices = NULL, multiple = TRUE),

      # Checkbox for "All Sports"
      checkboxInput("allSports", "Select all sports", value = FALSE),

      # Instruction for the "Select all sports" checkbox
      p("Note: When 'Select all sports' is checked, the data for all available sports will be displayed. Uncheck the box to manually select specific sports.")
    ),

    mainPanel(
      plotOutput("participationPlot"),
      p("The above plot shows the number of male and female athletes over time for the selected sport(s) and season."),

      plotOutput("sportsPlot"),
      p("The second plot shows the number of sports in which male and female athletes participated over time.")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive to filter data based on the selected season and year range
  filtered_data <- reactive({
    olympics_participation |>
      dplyr::filter(season == input$season,
                    year >= input$yearRange[1] & year <= input$yearRange[2])
  })

  # Dynamically update sports choices based on the selected season
  observe({
    sports <- filtered_data() |>
      dplyr::distinct(sport) |>
      dplyr::arrange(sport) |>
      dplyr::pull(sport)

    # If the checkbox is checked, select all sports, otherwise update the selectInput with available sports
    if (input$allSports) {
      updateSelectInput(session, "sport", choices = sports, selected = sports)
    } else {
      updateSelectInput(session, "sport", choices = sports, selected = "Basketball")
    }
  })

  # Plot for gender participation over time
  output$participationPlot <- renderPlot({

    sex_color <- c("red","blue")
    names(sex_color) <- c("F","M")

    data <- filtered_data() |>
      dplyr::filter(if (!input$allSports) sport %in% input$sport else TRUE) |>
      dplyr::group_by(year, sex) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    ggplot2::ggplot(data, ggplot2::aes(x = year, y = count, color = sex)) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(name = "Sex", values = sex_color) +
      ggplot2::labs(
        title = paste("Gender Participation in", input$season, "Olympics Over Time"),
        x = "Year",
        y = "Number of Athletes"
      ) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14),
        legend.position = "top"
      )
  })

  # Plot for sports participation by gender
  output$sportsPlot <- renderPlot({
    data <- filtered_data() |>
      dplyr::group_by(year) |>
      dplyr::summarise(womens_sports = dplyr::n_distinct(sport[sex == "F"]),
                       mens_sports = dplyr::n_distinct(sport[sex == "M"]))

    ggplot2::ggplot(data, ggplot2::aes(x = year)) +
      ggplot2::geom_line(ggplot2::aes(y = womens_sports, color = "Women's sports"), size = 1.2) +
      ggplot2::geom_line(ggplot2::aes(y = mens_sports, color = "Men's sports"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = womens_sports, color = "Women's sports"), size = 3) +
      ggplot2::geom_point(ggplot2::aes(y = mens_sports, color = "Men's sports"), size = 3) +
      ggplot2::labs(
        title = paste("Sports Participation in", input$season, "Olympics by Gender Over Time"),
        x = "Year",
        y = "Number of Sports"
      ) +
      ggplot2::scale_color_manual(values = c("Women's sports" = "red", "Men's sports" = "blue")) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14),
        legend.position = "top"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
