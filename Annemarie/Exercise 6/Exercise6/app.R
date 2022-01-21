#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny) # App
library(mlbench) # Dataset
library(ggplot2) # Plot
library(DT) # Data table
library(psych) # Descriptive statstics

# Load data
data("BostonHousing")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predicting Housing Prices in Boston"),

    # Sidebar with a drop dpwn menu input for the predictor in the scatterplot
    sidebarLayout(
        sidebarPanel("This app enables you to first inspect the Boston Housing dataset by Harrison and Rubinfeld (1979) and then to do a simple regression with it.",  
            selectInput(
                inputId = "xaxis", 
                label = "Choose one predictor", 
                choices = colnames(BostonHousing[, -14])
            ),
            sliderInput(inputId = "bins",
                        label = "Choose a number of bins",
                        min = 10,
                        max = 50,
                        value = 20),
            radioButtons(inputId = "color",
                         label = "Choose a color for the histograms and scatterplot",
                         choiceNames = list("Blue", "Red", "Yellow"),
                         choiceValues = list("#9AC8D5", "#EE9FA9", "#F9CA7F"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Inspect the data", tabsetPanel(
                            tabPanel("Raw data", dataTableOutput("table1")), # Raw data table
                            tabPanel("Descriptive statistics", tableOutput("table2")), # Table with descriptive statistics
                            tabPanel("Distribution medv", plotOutput("distPlot1")), # Distribution medv
                            tabPanel("Distribution predictor", plotOutput("distPlot2")), # Distribution predictor
                            tabPanel("Scatterplot", plotOutput("scatterplot")))), # Scatterplot
                        tabPanel("Regression", verbatimTextOutput("regression")) # Regression 
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Raw data, data table
    output$table1 <- DT::renderDataTable({
        DT::datatable(BostonHousing, options = list(lengthMenu = c(5, 15, 30), pageLength = 5))
    })
    # Descriptive statistics
    output$table2 <- renderTable({
        psych::describe(BostonHousing)
    })
    # Distribution dependent variable
    output$distPlot1 <- renderPlot({
        hist(BostonHousing[,14], breaks = input$bins, col = input$color,
             xlab = "medv",
             main = "medv")
    })
    # Distribution predictor
    output$distPlot2 <- renderPlot({
        hist(BostonHousing[,input$xaxis], breaks = input$bins, col = input$color,
             xlab = input$xaxis,
             main = input$xaxis)
    })
    # Scatterplot
    output$scatterplot <- renderPlot({
        ggplot(BostonHousing,
               aes_string(x = input$xaxis, y = BostonHousing$medv)) +
            geom_point(color = input$color) +
            ylab("medv") +
            theme_bw()
            
    })
    # Regression 
    output$regression <- renderPrint({
        fit <- lm(BostonHousing$medv ~ BostonHousing[,input$xaxis])
        names(fit$coefficients) <- c("Intercept", input$xaxis)
        summary(fit)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
