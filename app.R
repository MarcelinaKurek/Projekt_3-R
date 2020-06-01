library(shiny)
library(shinythemes)

ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage("Analiza danych NYC Bike Share", header=tags$head(includeCSS("styles.css")) ,
             tabPanel("Ogólne",
                      sidebarLayout(
                        sidebarPanel(),
                        mainPanel(
                          h2("Liczba wypożyczeń w ciągu roku - 20.551.697"),
                          h2("Liczba wszystkich używanych rówerów - 19.571"),
                          h2("Liczba wszystkich stacji ~ 1.000"),
                          br(),
                          h3("Wypożyczenia w poszczególnych miesiącach.")
                          
                        )
                        )
                      ),
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
                                                   "średnia - 30 - 45 min"=3, "długa - ponad 45 min"=4), selected = c(1,2,3,4))
                        ),
                        mainPanel(
                          plotOutput("dlPodrozy", width = "100%"),
                          plotOutput("barPlotDlPodrozy")
                        )
                      )),
             tabPanel("Zagęszczenie ruchu na stacjach",
                      sidebarLayout(
                        sidebarPanel(
                        ),
                        mainPanel(
                          plotOutput("stacje"),
                          plotOutput("rano"),
                          plotOutput("wieczor")
                        )
                      ))
  )
  
)


server <- function(input, output) {
  library(data.table)
  library(readr)
  library(ggplot2)
  
  
  output$favorite_routes <- renderPlot({
   # Routes <- data.table(read_csv("przeliczone_dane/routes_to_plot.csv"))
    #routes_to_plot2 <- Routes[1:100,ID := .I]
    #routes_to_plot2 <- rbind(routes_to_plot2[,c("start latitude", "start longitude", "ID")],
     #                        routes_to_plot2[,c("end latitude", "end longitude", "ID")], use.names=FALSE)
    #ggplot(routes_to_plot2[,c("start latitude", "start longitude", "ID")], aes(x=`start longitude`, y=`start latitude`, group=ID)) +
     # geom_point(size=2, color="black") +
     # geom_line(color="red")
  })
  
  output$sub_vs_cust <- renderPlot({
    df <- read_csv(file = "przeliczone_dane/sub_vs_customers.csv")
    ggplot(data=df, aes(x = usertype, y=N, fill=as.factor(N))) +
      geom_bar(stat="identity") +
      scale_fill_manual(values=c("#6a040f", "#e85d04")) +
      theme(legend.position = "none") +
      xlab("Typ użytkownika") +
      ggtitle("Liczba wypożyczeń z uwagi na typ użytkownika w skali roku")
  
  })
  
  output$dlPodrozy <- renderPlot({
    trips_hours <- read_csv("przeliczone_dane/tripduration_over_hours.csv")
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
        scale_size( name="Liczba wypożyczeń (N)", range = c(1,12)) +
        ggtitle("Długość podróży w poszczególnych godzinach.(czerwiec - wrzesień)")
    }
    
  })
  
  output$barPlotDlPodrozy <- renderPlot({
    
    dane <- read.csv("ile_podrozy_okreslonej_dl.csv")
    ggplot(data=dane,aes(x = `typ.podrozy`, y = N, fill = as.factor(N))) +
      geom_bar(stat="identity", width = 0.4) +
      scale_fill_manual(values=c("#05668d", "#028090", "#00a896", "#02c39a" )) +
      theme(legend.position = "none") +
      xlab("Typ podróży") +
      ggtitle("Liczba podróży danej długości(czerwiec - wrzesień)")
  })
  
  output$stacje <- renderPlot({
    busyStations <- read_csv("busy_stations.csv")
    ggplot(busyStations, aes(x=longitude, y=latitude, color=N)) + 
      geom_point(alpha=0.3, size = 4) + 
      scale_size(range=c(.1, 10)) +
      scale_color_gradient(low = "pink", high = "purple") +
      ggtitle("Zagęszczenie ruchu na stacjach") + 
      theme(legend.title = element_text("Liczba wypożyczonych rowerów na stacji"))
  })
  
  output$rano <- renderPlot({
    morning <- read_csv("end_stations_morning.csv")
    ggplot(morning, aes(x=longitude, y=latitude, color=N)) + 
      geom_point(alpha=0.4, size = 4) + 
      scale_size(range=c(.1, 10)) +
      scale_color_gradient(low="orange", high="blue") +
      ggtitle("Stacje docelowe rano (7 - 10, pon - pt)") +
      theme(legend.title = element_text("Liczba zwrotów rowerów na stacji"))
    
  })
  output$wieczor <- renderPlot({
    evening <- read_csv("end_stations_evening.csv")
    ggplot(evening, aes(x=longitude, y=latitude, color=N)) + 
      geom_point(alpha=0.3, size = 4) + 
      scale_size(range=c(.1, 10)) +
      scale_color_gradient(low="orange", high="blue") +
      ggtitle("Stacje docelowe wieczorem (16 - 19, pon - pt)") +
      theme(legend.title = element_text("Liczba zwrotów rowerów na stacji"))
    
  })
}

shinyApp(ui = ui, server = server)