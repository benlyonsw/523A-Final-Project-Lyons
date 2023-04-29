#
# This shinyapp calculates the 100-year global warming potential of
# various greenhouse gases using values from the IPCC Fifth Assessment,
# Working Group 1, Chapter 8, Table 8.7.
# The output will be in kilotonnes.
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Global Warming Potential Calculator"),
  
  h5(
    "In this app you can calculate the 100-year GWP of six different greenhouse gases.
Default 100-year GWP values are from the IPCC Fifth Assessment, Working Group 1, Chapter 8,
Table 8.7."
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "unit",
        label = "Units",
        choices = list("t", "kt", "Mt", "Gg", "Tg"),
        multiple = FALSE,
        selected = "kt"),
      
      # Input: Greenhouse Gases
      numericInput("CO2", "Carbon Dioxide", 0, min = 0, max = (1*10^100)),
      numericInput("CH4", "Methane", 0, min = 0, max = (1*10^100)),
      numericInput("N2O", "Nitrous Oxide", 0, min = 0, max = (1*10^100)),
      numericInput("HFC.134a", "HFC-134a", 0, min = 0, max = (1*10^100)),
      numericInput("CFC.11", "CFC-11", 0, min = 0, max = (1*10^100)),
      numericInput("CF4", "Carbon tetrafluoride", 0, min = 0, max = (1*10^100))
    ),
    
    mainPanel(plotlyOutput("gwp_plot")),
    position = "left"
  )
)

server <- function(input, output){
  
  # Make a reactive object to graph
  CO2Equivalents <- reactive({

  # Input units
    CO2 = input$CO2
    
    CH4 = input$CH4
    
    N2O = input$N2O
    
    HFC.134a = input$HFC.134a
    
    CFC.11 = input$CFC.11
    
    CF4 = input$CF4
    
    CO2.units = as.character(input$unit)
    
    CH4.units = as.character(input$unit)
    
    N2O.units = as.character(input$unit)
    
    HFC.134a.units = as.character(input$unit)
    
    CFC.11.units = as.character(input$unit)
    
    CF4.units = as.character(input$unit)
    
    # Set default values
    
    if(CO2 == 0) {CO2.units <- "kt"}
    
    if(CH4 == 0) {CH4.units <- "kt"}
    
    if(N2O == 0) {N2O.units <- "kt"}
    
    if(HFC.134a == 0) {HFC.134a.units <-"kt"}
    
    if(CFC.11 == 0) {CFC.11.units <- "kt"}
    
    if(CF4 == 0) {CF4.units <- "kt"}
    
    ####### Convert input units, if necessary.
    
    if(CO2.units == "t") {CO2.kt <- CO2/1000} else if
    (CO2.units == "kt") {CO2.kt <- CO2} else if
    (CO2.units == "Mt") {CO2.kt <- CO2*1000} else if
    (CO2.units == "Gg") {CO2.kt <- CO2} else if
    (CO2.units == "Tg") {CO2.kt <- CO2*1000} else if
    (!CO2.units %in% units) {stop("Carbon dioxide input must be in one of the following units: t, kt, Mt, Gg, Tg!")}
    
    if(CH4.units == "t") {CH4.kt <- CH4/1000} else if
    (CH4.units == "kt") {CH4.kt <- CH4} else if
    (CH4.units == "Mt") {CH4.kt <- CH4*1000} else if
    (CH4.units == "Gg") {CH4.kt <- CH4} else if
    (CH4.units == "Tg") {CH4.kt <- CO2*1000} else if
    (!CH4.units %in% units) {stop("Methane input must be in one of the following units: t, kt, Mt, Gg, Tg!")}
    
    if(N2O.units == "t") {N2O.kt <- N2O/1000} else if
    (N2O.units == "kt") {N2O.kt <- N2O} else if
    (N2O.units == "Mt") {N2O.kt <- N2O*1000} else if
    (N2O.units == "Gg") {N2O.kt <- N2O} else if
    (N2O.units == "Tg") {N2O.kt <- N2O*1000} else if
    (!N2O.units %in% units) {stop("Nitrous oxide input must be in one of the following units: t, kt, Mt, Gg, Tg!")}
    
    if(HFC.134a.units == "t") {HFC.134a.kt <- HFC.134a/1000} else if
    (HFC.134a.units == "kt") {HFC.134a.kt <- HFC.134a} else if
    (HFC.134a.units == "Mt") {HFC.134a.kt <- HFC.134a*1000} else if
    (HFC.134a.units == "Gg") {HFC.134a.kt <- HFC.134a} else if
    (HFC.134a.units == "Tg") {HFC.134a.kt <- HFC.134a*1000} else if
    (!HFC.134a.units %in% units) {stop("HFC-134a input must be in one of the following units: t, kt, Mt, Gg, Tg!")}
    
    if(CFC.11.units == "t") {CFC.11.kt <- CFC.11/1000} else if
    (CFC.11.units == "kt") {CFC.11.kt <- CFC.11} else if
    (CFC.11.units == "Mt") {CFC.11.kt <- CFC.11*1000} else if
    (CFC.11.units == "Gg") {CFC.11.kt <- CFC.11} else if
    (CFC.11.units == "Tg") {CFC.11.kt <- CFC.11*1000} else if
    (!CFC.11.units %in% units) {stop("CFC-11 input must be in one of the following units: t, kt, Mt, Gg, Tg!")}
    
    if(CF4.units == "t") {CF4.kt <- CF4/1000} else if
    (CF4.units == "kt") {CF4.kt <- CF4} else if
    (CF4.units == "Mt") {CF4.kt <- CF4*1000} else if
    (CF4.units == "Gg") {CF4.kt <- CF4} else if
    (CF4.units == "Tg") {CF4.kt <- CF4*1000} else if
    (!CF4.units %in% units) {stop("CF4 input must be in one of the following units: t, kt, Mt, Gg, Tg!")}
    
    ####### Multiply by emission factors.
    
    CO2.eq <- (CO2.kt*1)
    CH4.CO2.eq <- (CH4.kt*28)
    N2O.CO2.eq <- (N2O.kt*265)
    HFC.134a.CO2.eq <- (HFC.134a.kt*1300)
    CFC.11.CO2.eq <- (CFC.11.kt*4660)
    CF4.CO2.eq <- (CF4.kt*6630)
    
    ####### Generate data frame.
    
    CO2Data <- data.frame("Greenhouse_Gas" = c("Carbon Dioxide", "Methane", "Nitrous Oxide",
                                                      "HFC-134a", "CFC-11", "CF4"),
                                 "Kilotons_CO2e" = c(CO2.eq, CH4.CO2.eq, N2O.CO2.eq, HFC.134a.CO2.eq,
                                                     CFC.11.CO2.eq, CF4.CO2.eq))
    
    return(CO2Data)
    
  })                          
  # Create plotly plot
  
  output$gwp_plot <- renderPlotly({
    plotly::plot_ly(data = CO2Equivalents(), x = ~Greenhouse_Gas, y = ~Kilotons_CO2e,
                    type = "bar") %>%
      layout(title = "100y GWP of User-Selected GHGs", 
             xaxis = list(title = "Greenhouse Gas"),
             yaxis = list(title = "Kilotonnes CO2 Equivalent"))
    
    
  })
  
}

shinyApp(ui = ui, server = server)
