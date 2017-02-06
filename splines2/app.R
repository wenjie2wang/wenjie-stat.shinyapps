library(shiny)
library(splines2)


ui <- fluidPage(
    titlePanel("Splines Explorer"),
    sidebarLayout(
        sidebarPanel(
            selectInput("fun", "Splines",
                        c("B-splines", "M-splines", "I-splines", "C-splines")),
            selectInput("nKnots", "Number of Internal Knots", seq.int(0, 5), 3),
            selectInput("degree", "Degree", seq.int(0, 4), 2)
        ),
        mainPanel(
            plotOutput("matplot")
        )
    )
)


server <- function(input, output)
{
    output$matplot <- renderPlot({
        fun <- switch(input$fun,
                      "B-splines" = "bSpline",
                      "M-splines" = "mSpline",
                      "I-splines" = "iSpline",
                      "C-splines" = "cSpline")
        x <- seq.int(0, 1, 0.01)
        df <- as.numeric(input$nKnots) + as.numeric(input$degree) + 1L
        resMat <- do.call(fun, list(x = x, df = df, degree = input$degree,
                                    intercept = TRUE))
        matplot(x, resMat, type = "l",
                xlab = "x", ylab = "Spline bases")
        abline(v = attr(resMat, "knots"), col = "gray", lty = 2)
    })
}


shinyApp(ui, server)
