library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Gender Trends in the Olympics"),

    sidebarLayout(
      sidebarPanel(
        # Select input for choosing season
        selectInput("season", "Select season:", choices = c("Summer", "Winter"), selected = "Summer"),

        # Slider for year range
        sliderInput("yearRange", "Select year range:", min = 1900, max = 2016, value = c(1900, 2016), step = 4),

        # Placeholder select input for sports, initially empty
        selectInput("sport", "Choose sport:", choices = NULL, multiple = TRUE)
      ),

      mainPanel(
        plotOutput("participationPlot"),
        plotOutput("sportsPlot")
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

    # Update the sports input with filtered sports choices
    updateSelectInput(session, "sport", choices = sports, selected = "Basketball")
  })

  # Plot for gender participation over time
  output$participationPlot <- renderPlot({

    sex_color<-c("red","blue")
    names(sex_color)<-levels(c("F","M"))

    data <- filtered_data() |>
      dplyr::filter(sport %in% input$sport) |>
      dplyr::group_by(year, sex) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    ggplot2::ggplot(data, ggplot2::aes(x = year, y = count, color = sex)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_color_manual(name = "Sex", values = sex_color) +
      ggplot2::labs(title = paste("Gender Participation in", input$season, "Olympics Over Time")) +
      ggplot2::theme_classic()
  })

  # Plot for sports participation by gender
  output$sportsPlot <- renderPlot({
    data <- filtered_data() |>
      dplyr::group_by(year) |>
      dplyr::summarise(womens_sports = dplyr::n_distinct(sport[sex == "F"]),
                       mens_sports = dplyr::n_distinct(sport[sex == "M"]))

    ggplot2::ggplot(data, ggplot2::aes(x = year)) +
      ggplot2::geom_line(ggplot2::aes(y = womens_sports, color = "Women's sports")) +
      ggplot2::geom_line(ggplot2::aes(y = mens_sports, color = "Men's sports")) +
      ggplot2::geom_point(ggplot2::aes(y = womens_sports, color = "Women's sports")) +
      ggplot2::geom_point(ggplot2::aes(y = mens_sports, color = "Men's sports")) +
      ggplot2::labs(
        title = paste("Sports Participation in", input$season, "Olympics by Gender Over Time"),
        x = "Year",
        y = "Number of sports"
      ) +
      ggplot2::scale_color_manual(values = c("Women's sports" = "red", "Men's sports" = "blue")) +
      ggplot2::theme_classic()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
