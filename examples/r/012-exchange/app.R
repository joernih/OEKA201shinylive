if (grepl("wasm", sessionInfo()[[2]])) {
  # If the session info contains "wasm", install the package from the specified repository
  webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
  library("WASMP")
} else {
  # If the session info does not contain "wasm", load the package from the local library
  library("WASMP")
}
# WASMP packages
library(shiny)
library(dplyr)
library(ggplot2)
# 
data_vk <- data.frame(nb_ts[[1]])


ui <- fluidPage(
  titlePanel("Mange land"),
  sidebarLayout(
   sidebarPanel(
       selectInput("veks","Form",c("value_EUR","value_USD","value_SEK")),
       textInput("datef", "Select fra dato:", value = "2020-01-01"),
       textInput("datet", "Select til dato:", value = "2021-10-05")
    ),
   mainPanel(
     tabsetPanel(
       tabPanel("Plot",plotOutput("plot")),
       tabPanel("Tabell",tableOutput("table"))
            )
    )
  )
)
server <- function(input, output) {
  res <- reactive({
    veks <- input$veks
    startd_i <- as.Date(input$datef)
    startd_n <- as.Date(input$datet)
    mvg <- data_vk %>% dplyr::mutate(date=base::as.Date(dato)) %>%
     # Filter out all observations starting earlier or equal than startd_i
     dplyr::filter(date>startd_i) %>%
     # Filter out all observations starting later or equal than startd_n
     dplyr::filter(date<startd_n) 
    list(mvg,veks)
  })
  output$plot <- renderPlot({
    ggplot2::ggplot(res()[[1]], aes(x = date, y =!!as.name(res()[[2]]))) + 
      ggplot2::geom_point() +
      ggplot2::labs(x = "Date", y = "Exchange Rate", color = "Currency") +
      ggplot2::theme_classic()

  })
  output$table <- renderTable({
    res()[[1]]
  })
}
shinyApp(ui = ui, server = server)

