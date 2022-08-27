source("www/functions/readingdata.R")
source("www/functions/ObesityTop10.R")
source("www/functions/OverWeight.R")
source("www/functions/PyramidTrend.R")


################################################################################
ui <- fluidPage(
  #theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  #shinyjs::inlineCSS(list(body = "color:#336600")),
  #setSliderColor("#DAA520",1),
  #setBackgroundColor(
  #color = ("#FFF8DC"),
  #gradient = "linear"
  #),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "comp.css")
  ),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  theme = shinytheme("sandstone"),
  navbarPage(title = "Physical Activity & Obesity",
             tabsetPanel(
               tabPanel(
                 "Obesity Vs Physical Inactivity",
                 mainPanel(
                   width = 12,
                   fluidRow(splitLayout(
                     cellWidths = c("80%", "20%"),
                     column(11, uiOutput("slider1")),
                     column(
                       1,
                       actionButton(
                         class = "reset-btn",
                         inputId = "reset1",
                         label = "Reset Data",
                         icon = icon("refresh"),
                         align = "center"
                       )
                     )
                   ),),
                   fluidRow(class = "plot-row",
                            splitLayout(
                              column(4, plotOutput(
                                "plot1", width = "450px", height = "500px"
                              )),
                              column(4, plotOutput(
                                "plot2", width = "450px", height = "500px"
                              )),
                              column(4, plotOutput(
                                "plot3", width = "450px", height = "500px"
                              ))
                            ), )
                 )
               ),
               tabPanel(
                 "Socio-economic Influence on Overweight",
                 mainPanel(
                   width = 12,
                   fluidRow(splitLayout(
                     cellWidths = c("80%", "20%"),
                     column(11, uiOutput("slider2")),
                     column(
                       1,
                       actionButton(
                         class = "reset-btn",
                         inputId = "reset2",
                         label = "Reset Data",
                         icon = icon("refresh"),
                         align = "center"
                       )
                     )
                   ),),
                   fluidRow(
                     class = "plot-row",
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       column(6, plotOutput(
                         "OWP1", width = "550px", height = "500px"
                       )),
                       column(6, plotOutput(
                         "OWP4", width = "550px", height = "500px"
                       ))
                     ),
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       column(6, plotOutput(
                         "OWP3", width = "550px", height = "500px"
                       )),
                       column(4, plotOutput(
                         "OWP2", width = "550px", height = "500px"
                       ))
                     ),
                   )
                 )
               ),
               tabPanel(
                 "Physical Activity Pyramid",
                 mainPanel(
                   width = 12,
                   fluidRow(splitLayout(
                     cellWidths = c("50%", "30%", "20%"),
                     column(6, class= "tab-3-slider", uiOutput("slider3")),
                     column(5, class ="input-dropdown",uiOutput("race")),
                     column(
                       1,
                       actionButton(
                         class = "reset-btn",
                         inputId = "reset3",
                         label = "Reset Data",
                         icon = icon("refresh"),
                         align = "center"
                       )
                     )
                   ),),
                   fluidRow(class = "plot-row",
                            splitLayout(
                              cellWidths = c("50%", "50%"),
                              column(6, plotOutput(
                                "tab1", width = "550px", height = "500px" )),
                              column(6, plotOutput(
                                "tab2", width = "550px", height = "500px" ))
                            ),),
                   fluidRow(
                     column(12, align ="center",imageOutput("trend1")))
                   )
                 ),
               tabPanel(
                 "Obesity Epidemic in USA",
                 mainPanel(
                   width = 12,
                   fluidRow(
                     column(12,class="plotly-map", plotlyOutput("geo1"))
                   ),
                   fluidRow(
                     column(12,class="plotly-map", plotlyOutput("geo2"))
                   )
                 )
               )
             ))
)

server <- function(input, output, session) {
  data <- readingdata()
  
  output$slider1 <- renderUI({
    sliderInput(
      inputId = "sliderinput1",
      label = "Year:",
      min = 2011,
      max = 2020,
      value = 2011,
      sep = ""
    )
  })
  
  output$slider2 <- renderUI({
    sliderInput(
      inputId = "sliderinput2",
      label = "Year:",
      min = 2011,
      max = 2020,
      value = 2011,
      sep = ""
    )
  })
  
  output$slider3 <- renderUI({
    sliderInput(
      inputId = "sliderinput3",
      label = "Year:",
      min = 2011,
      max = 2020,
      value = 2011,
      sep = ""
    )
  })
  
  output$race <- renderUI({
    selectInput(
      inputId = "dropdown1",
      label = "Race/Ethnicity:",
      choices = c(
        "Asian",
        "Hawaiian/Pacific Islander",
        "Hispanic",
        "Non-Hispanic Black",
        "Non-Hispanic White",
        "American Indian/Alaska Native",
        "2 or more races",
        "Other"
      ),
      selected = "Non-Hispanic Black", 
      multiple = FALSE,
      width = "100%")
  })
  
  output$plot1 <- renderPlot({
    ObesityTop10(data, input$sliderinput1)
  })
  
  output$plot2 <- renderPlot({
    physicalTop10(data, input$sliderinput1)
  })
  
  output$plot3 <- renderPlot({
    jointdata(data)
  })
  
  output$OWP1 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[1]]
  })
  
  output$OWP2 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[2]]
  })
  
  output$OWP3 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[3]]
  })
  
  output$OWP4 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[4]]
  })
  
  output$tab1 <- renderPlot({
    gender(data, input$sliderinput3)
  })
  
  output$tab2 <- renderPlot({
    yearTrend(data,input$dropdown1)
  })
  
  output$trend1 <- renderImage({
    trendani(data)
  }, deleteFile = TRUE)
  
  output$geo1 <- renderPlotly({
    geoani(data)
  })
  
  output$geo2 <- renderPlotly({
    obesityani(data)
  })
  
  observeEvent(input$reset1, {
    #shinyjs::reset("slider1")
    updateSliderInput(session=session,"sliderinput1",value=2011)
  })
  
  observeEvent(input$reset2, {
    updateSliderInput(session=session,"sliderinput2",value=2011)
  })
  
  observeEvent(input$reset3, {
    updateSliderInput(session=session,"sliderinput3",value=2011)
    updateSelectInput(session=session,"dropdown1",selected="Non-Hispanic Black")
})



}
shinyApp(ui, server)
