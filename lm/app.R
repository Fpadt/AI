#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
rd0 <- rnorm(np, mean=0, sd = 20)
rd1 <- rd0[order(abs(rd0), decreasing = TRUE)]
np <- 500
xr <- 100
DT <- data.table(x = sample(1:xr, size = np, replace = TRUE))
DT[, f:= 3*x ]
# DT[, r:= + rnorm(np, mean=20,sd = 20)]
DT[, y:= f+20+rd1]
# setorder(DT, r)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data collection"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        helpText("           "),
        # sidebarPanel(
        #
        # ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        ,
        sliderInput("points",
                    "Number of data points:",
                    min     = 1,
                    max     = np,
                    value   = 2,
                    step    = 1,
                    animate = TRUE)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {



    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        #
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')

        myLM <- lm(lm(y ~ x, DT[1:input$points]))
        parA <- round(coefficients(myLM)[1], 1)
        parB <- round(coefficients(myLM)[2], 1)
        form <- if(is.na(parB) == TRUE){
            paste("y = ", format(parA, nsmall=1))
        } else {
            paste("y = ", format(parB, nsmall=1),
                  " * X +", format(parA, nsmall=1))
        }

        ggplot(data = DT[1:input$points], mapping = aes(x=x, y=y)) +
            geom_smooth(color="red", method = lm, se = FALSE,
                        formula = y ~ x, size=3) +
            geom_point(shape=4, size=5) +
            ylim(0, 5*xr) +
            xlim(0, 3*xr) +
            annotate(
                "text", x = 50, y = 450 ,
                label = form, size = unit(12, "pt"),
                colour = "#FE5000")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
