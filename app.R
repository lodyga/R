# div("install", style = "color:red")
# p("Shiny", span("RS", style = "color:blue"))
# br()
# code('install.packages("shiny")'),
# p("text ", a("Shiny homepage.", href = "http://shiny.rstudio.com")),
# img(src = "rstudio.png", height = 35, width = 100),

# Control widgets
# fluidRow()
# column(3,)
# actionButton("action", "Action")
# checkboxInput("checkbox", "Choice A", value = F)),
# checkboxGroupInput("checkGroup", h3("Checkbox group"), choices = c("Choice 1" = 1, "Choice 2" = 2, selected = 1)),
# dateInput("date", h3("Date input"), value = "2014-01-01"))
# dateRangeInput("dates", h3("Date range"))),
# fileInput("file", h3("File input"))),
# helpText("Note: help text isn't a true widget,")
# numericInput("num", h3("Numeric input"), value = 1))  
# radioButtons("radio", h3("Radio buttons"), choices = list("Choice 1" = 1, "Choice 2" = 2),selected = 1)),
# selectInput("select", h3("Select box"), choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1))
# sliderInput("slider2", "",min = 0, max = 100, value = c(25, 75))
# textInput("text", h3("Text input"), value = "Enter text..."))
# Reactive output 
# textOutput("selected_var") Output w panelu
# output$selected_var <- renderText({paste("You have selected this", input$var)}) na serwerze
# source("helpers.R") to use within function percent_map()
# 
#
#







library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display", 
                  choices = c("Percent White" = "Percent White", 
                                 "Percent Black",
                                 "Percent Hispanic", 
                                 "Percent Asian"),
                  selected = 1),
      
      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))

 
    
    
    ),
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max"),
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
 output$selected_var <- renderText({
   paste("You have selected this", input$var)
   })
 
 output$min_max <- renderText({
   paste("You have chosen a range that goes from", input$range[1], "to", input$range[2])
 })
 
}

# Run the app ----
shinyApp(ui = ui, server = server)
  

