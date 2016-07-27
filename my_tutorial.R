# video 1, 1:19:37

library(shiny)

ui <- fluidPage(
  titlePanel("My First Shiny App"),
  
  sidebarPanel(
    sliderInput(inputId = "num", 
                label = "here's a slider",
                value = 10, min = 1, 
                max = 100),
    
    
    textInput(inputId = "text_1",
              label = "put your text here",value = "Hi"),
    verbatimTextOutput("stats"),
    textInput(inputId = "name_1",label = "what is your name?"),
    verbatimTextOutput("name"),
    
    actionButton(inputId = "clicks",
                 label = "Click me")
  ),
  mainPanel(
    plotOutput(outputId = "hist"),
    
    plotOutput(outputId = "click_plot")

  )
)


server <- function(input, output){
  
  data <- reactive({
    rnorm(input$num)
  })
  
  data_clicks <- reactive({rnorm(input$clicks)})
  
  output$hist <- renderPlot({ 
    #title <- "This is a histogram"
    hist(data(),main = isolate({input$text_1})) 
  })
  output$stats <- renderPrint({
    summary(data())
  })
  output$name <- renderPrint({
    print(paste0("Your name is ", input$name_1))
  })
  
  observeEvent(input$clicks, {
    output$click_plot <- renderPlot({hist(data_clicks(), main = "From clicks")})
    
    })
}



shinyApp(ui = ui, server = server)