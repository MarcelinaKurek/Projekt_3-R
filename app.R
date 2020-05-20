library(shiny)
library(shinythemes)

ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage("Analiza danych NYC Bike Share", theme = shinytheme("united"), header=tags$head(includeCSS("styles.css")) ,
             tabPanel("Najczęstsze trasy",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("okres", "Miesiące", 
                                      min = 1, max = 12, value = c(1,12)
                          )
                        ),
                        mainPanel(
                          plotOutput("favorite_routes")
                        )
                      )
             )
  )
  
)


server <- function(input, output) {
  library(data.table)
  library(readr)
  library(ggplot2)
  
  Routes <- data.table(read_csv("routes_to_plot.csv"))
  
  output$favorite_routes <- renderPlot({
    
    routes_to_plot2 <- Routes[month >= input$okres[1] & month <= input$okres[2],]
    routes_to_plot2 <- routes_to_plot2[,sum(N), by=list(`start latitude`,`start longitude`, `end latitude`, `end longitude`)]
    routes_to_plot2 <- routes_to_plot2[order(V1, decreasing = TRUE)]
    routes_to_plot2 <- routes_to_plot2[1:100,]
    routes_to_plot2 <- routes_to_plot2[,ID := .I]
    routes_to_plot2 <- rbind(routes_to_plot2[,c("start latitude", "start longitude", "ID")],
                             routes_to_plot2[,c("end latitude", "end longitude", "ID")], use.names=FALSE)
    
    ggplot(routes_to_plot2, aes(x=`start longitude`, y=`start latitude`, group=ID)) +
      geom_point(size=2, color="black") +
      geom_line(color="red")
  })
}

shinyApp(ui = ui, server = server)