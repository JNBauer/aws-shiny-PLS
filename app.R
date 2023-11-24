



source("utils.R")
library(shiny)
library(chillR)



# Add directory of static resources to Shiny's web server



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
    plotOutput("plot",height = 720, width = "100%"),

)

# Define server logic required to draw a histogram
server = function(input, output) {
  print("Test")
  output$plot <- renderPlot({
    Kyoto <- readRDS(paste0("data/PLS_",input$location,".RDS"))
    plot <- ggplot_PLS(Kyoto)
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
