if (grepl("wasm", sessionInfo()[[2]])) {
  # If the session info contains "wasm", install the package from the specified repository
  webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
  library(WASMP)
} else {
  # If the session info does not contain "wasm", load the package from the local library
  library("WASMP")
}
library(shiny)
library(dplyr)
library(ggplot2)
### **Gjøre i stand dataene om pengemengdeveksten fra FRED**
data_ma <- fred_ts[[2]]
vd1 <- as.Date("2022-01-01")
vd2 <- as.Date("2020-02-01")
plnv <- c('nvalue','growth')[1]
ui <- fluidPage(
  titlePanel("Mange land"),
  sidebarLayout(
   sidebarPanel(
       selectizeInput("cntr","Land",choices=c("USA","JPN","EUZ","GBR","CAN","NOR","DEN","SWE","SKE"),
		      multiple =TRUE,options=list(maxItems=4), selected=c("NOR")),
       selectInput("vars","Variable",c("mb3","cpi","une","mba","int","gdp")),
       selectInput("veks","Form",c("nvalue","growth")),
       textInput("datef", "Select fra dato:", value = "2020-01-01"),
       textInput("datet", "Select til dato:", value = "2024-01-01")
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
    cntr <- input$cntr
    varv <- input$vars
    veks <- input$veks
    startd_i <- as.Date(input$datef)
    startd_n <- as.Date(input$datet)
    dfg <- dplyr::filter(data_ma,id==varv) %>%
      dplyr::mutate(date=base::as.Date(date)) %>%
      dplyr::filter(date>startd_i) %>%
      dplyr::filter(date<startd_n) %>%
      dplyr::group_by(country) %>%
      dplyr::filter(country%in%cntr) %>%
      dplyr::mutate(couid=paste0(country,id)) %>%
      dplyr::mutate(nvalue=100*value/value[1]) %>%
      dplyr::mutate(lnalue=log(value)) %>% 
      dplyr::mutate(lvalue=dplyr::lag(value,n=12)) %>% 
      dplyr::mutate(growth=round(value/lvalue-1, 6)*100) %>%
      dplyr::ungroup() 
    mv <- mean(dfg[[veks]], na.rm=TRUE)
    list(dfg,mv,cntr,veks)
  })
  output$plot <- renderPlot({
    ggplot2::ggplot(data=res()[[1]], aes(x = date, y =!!as.name(res()[[4]]))) +
      geom_line(aes(color = couid)) + ggplot2::labs(title='Tidsserier') +
      ggplot2::theme_minimal()
  })
  output$table <- renderTable({
    dplyr::select(res()[[1]],date,growth,country,nvalue,lnalue)
  })
}
shinyApp(ui = ui, server = server)








