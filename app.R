library(shiny)
library(rworldmap)
dati_fin<-read.table("https://raw.githubusercontent.com/dariopiovesana/SLproject/master/dati_fin.csv", sep = ",", header = T)
dati_fin<-dati_fin[,-1]

ui<-fluidPage(titlePanel("VARIABILI OGGETTO DI ANALISI"),
              sidebarLayout(      
                sidebarPanel(
                  selectInput("variabili", "Variabili:", 
                              choices=colnames(dati_fin[,-11]))
                ),
                mainPanel(
                  plotOutput("data_map_europe")  
)))
server<-function(input, output) {
  selectedData <- reactive({
  joinCountryData2Map(dati_fin, "ISO3", nameJoinColumn = "Country", mapResolution = "medium")
  })
      output$data_map_europe <- renderPlot({

      mapCountryData(mapToPlot= selectedData() , nameColumnToPlot = input$variabili, mapRegion = "europe", catMethod = "fixedWidth", colourPalette = "heat", addLegend = T, borderCol = "black", mapTitle = input$variabili, missingCountryCol = "grey")
    })
  }
shinyApp(ui, server)