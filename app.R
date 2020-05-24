library(shiny)
library(shinythemes)

ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage("Analiza danych NYC Bike Share", theme = shinytheme("cosmo"), header=tags$head(includeCSS("styles.css")) ,
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
             ),
             tabPanel("Kto więcej wypożycza?",
                      sidebarLayout(
                        sidebarPanel(
                         p("Zauważamy, że znacznie częściej z rowerów korzystają osoby z roczną subskrypcją.")
                        ),
                        mainPanel(
                          plotOutput("sub_vs_cust")
                        ),
                        
                      )
             ),
             tabPanel("Długość podróży",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "btns", label = "Wybierz długość podróży",
                                       choices = c("bardzo krótka - 0 - 15 min"=1, "krótka - 15 - 30 min" = 2,
                                                   "średnia - 30 - 45 min"=3, "długa - ponad 45 min"=4))
                        ),
                        mainPanel(
                          plotOutput("dlPodrozy")
                        )
                      ))
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
  
  output$sub_vs_cust <- renderPlot({
    df <- read_csv(file = "sub_vs_customers.csv")
    ggplot(data=df, aes(x = usertype, y=N, fill=as.factor(N))) +
      geom_bar(stat="identity") +
      scale_fill_manual(values=c("purple", "orange")) +
      theme(legend.position = "none") +
      xlab("Typ użytkownika") +
      ggtitle("Liczba wypożyczeń z uwagi na typ użytkownika w skali roku")
  
  })
  
  output$dlPodrozy <- renderPlot({
    trips_hours <- read_csv("tripduration_over_hours.csv")
    dt <- data.table(NULL)
    print(input$btns[1])
      for (i in 1: length(input$btns)) {
        for (j in 1:4) {
          if (input$btns[i] == j) {
            if (j == 1) {
              data <- data.table(hour=c(1:24),trips_hours[,"0-15min.N"])
              setnames(data, new="N", old = "0-15min.N")
              data <- data[,`długość podróży`:="1. bardzo krótka"]
              dt <- rbind(dt,data)
            }
            else if (j == 2) {
              data <- data.table(hour=c(1:24),trips_hours[,"15-30min.N"])
              setnames(data, new="N", old = "15-30min.N")
              data <- data[,`długość podróży`:="2. krótka"]
              dt <- rbind(dt, data, use.names=FALSE )
            }
            else if (j == 3) {
              data <- data.table(hour=c(1:24),trips_hours[,"30-45min.N"])
              setnames(data, new="N", old = "30-45min.N")
              data <- data[,`długość podróży`:="3. średnia"]
              dt <- rbind(dt, data, use.names=FALSE )
            }
            else {
              data <- data.table(hour=c(1:24),trips_hours[,"over 45min.N"])
              setnames(data, new="N", old = "over 45min.N")
              data <- data[,`długość podróży`:="4. długa"]
              dt <- rbind(dt, data, use.names=FALSE )
            }
          }
        }
      }
    
    if (dim(dt) != 0) {
      ggplot(data=dt,aes(x=hour, y = `długość podróży`, color = `długość podróży`, size=N)) +
        geom_point(alpha=0.5) +
        scale_size( name="Liczba wypożyczeń (N)")
    }
    
    
  })
}

shinyApp(ui = ui, server = server)