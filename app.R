

setwd("..")

source("functions/utils.R")
library(shiny)
library(chillR)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("PhenoFlex Manual Calibration"),
    selectInput("location", "Choose a location:",
                japan_regions <- list(
                  "Sapporo", "Aomori", "Akita", "Morioka", "Yamagata", "Sendai", "Fukushima", "Niigata",
                  "Kanazawa", "Toyama", "Nagano", "Utsunomiya", "Fukui", "Maebashi", "Kumagaya", "Mito",
                  "Gifu", "Nagoya", "Kofu", "Choshi", "Tsu", "Shizuoka", "Tokio", "Yokohama",
                  "Kyoto", "Hikone", "Kobe", "Osaka", "Nara", "Matsuyama", "Takamatsu", "Kochi",
                  "Tokushima", "Hiroshima", "Okayama", "Matsue", "Tottori",
                  "Shimonoseki", "Fukuoka", "Saga", "Oita", "Nagasaki",
                  "Kumamoto", "Kagoshima", "Miyazaki"
                )
                
    ),
    plotOutput("plot",height = 720, width = "100%")

)

# Define server logic required to draw a histogram
server = function(input, output) {
  print("Test")
  output$plot <- renderPlot({
    Kyoto <- readRDS(paste("../data/cluster_input/solo_validation/",input$location,".RDS",sep=""))
    pheno<-Kyoto[[2]]
    
    
    weather_RDS<-Kyoto[[3]]
    weather <- read.csv(paste("../data/weather/cleanv2/",input$location,".csv",sep=""))
    JD_weather<-make_JDay(weather)
    
    PLS_results<-PLS_pheno(JD_weather,pheno)
    
    plot <- ggplot_PLS(PLS_results)
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
