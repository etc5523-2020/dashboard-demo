library(tidyverse)
library(plotly)
library(shinydashboard)
library(shiny)

theme_set(theme_bw())

yields <- read_csv("data/coffee-yields.csv") %>%
  janitor::clean_names() %>%
  rename_with(~"tonnes_per_hectare", .cols = 4)

continent <- yields %>%
  filter(is.na(code)) %>%
  count(entity) %>%
  filter(!entity %in% c("Ethiopia PDR",
                       "Land Locked Developing Countries",
                       "Least Developed Countries",
                       "Low Income Food Deficit Countries",
                       "Net Food Importing Developing Countries",
                       "Small island developing States")) %>%
  pull(entity)

production <- read_csv("data/coffee-production-by-region.csv") %>%
  janitor::clean_names() %>%
  rename_with(~"tonnes", .cols = 4)

ui <- dashboardPage(
    dashboardHeader(title = "All about coffee!"),
    dashboardSidebar(
      selectInput(inputId = "continent",
                  label = "Continent",
                  choices = continent,
                  selected = "Africa")
    ),
    dashboardBody(
      fluidRow(
        box(
          title = "Key Points", solidHeader = TRUE, width = 4,
            infoBoxOutput("latest_yield")
        ),
        box(plotOutput("yield_lineplot"))
      ),
      fluidRow(
        box(plotOutput("production_lineplot"))
      )
    )
)

make_line_plot <- function(data, y, type) {
  y <- rlang::enquo(y)
  continent <- unique(data$entity)
  y_lab <- stringr::str_to_title(
    stringr::str_replace_all(rlang::as_label(y), "_", " ")
  )
  ggplot(data, aes(x = year, y = {{y}})) +
    geom_line() +
    labs(x = "Year",
         y = y_lab,
         title = paste0("Coffee ", type, " over time in ", continent))

}

server <- function(input, output) {

  yield_continent <- reactive({
    filter(yields, entity == input$continent)
  })

  production_continent <- reactive({
    filter(production, entity == input$continent)
  })

  output$latest_yield <- renderInfoBox({
    yield_subset <- yield_continent() %>%
      slice(which.max(year))

    title <- paste("Coffe yield in", yield_subset$year)
    subtitle <- yield_subset$entity

    infoBox(title = title,
            subtitle = subtitle,
            value = paste(yield_subset$tonnes_per_hectare,
                          "tonnes per hectare"),
            color = "purple",
            width = "100%")
  })

  output$yield_lineplot <- renderPlot({
    yield_continent() %>%
      make_line_plot(y = tonnes_per_hectare, type = "yield")

  })

  output$production_lineplot <- renderPlot({
    production_continent() %>%
      make_line_plot(y = tonnes, type = "production")
  })
}


shinyApp(ui, server)
