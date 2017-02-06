library(shiny)
library(miniUI)
library(DT)
library(splines2)


ui <- miniPage(
    gadgetTitleBar("Splines Explorer"),
    miniTabstripPanel(
        miniTabPanel("Input", icon = icon("sliders"),
                     miniContentPanel(
                         selectInput("fun", "Splines",
                                     c("B-splines", "M-splines",
                                       "I-splines", "C-splines")),
                         selectInput("nKnots", "Number of Internal Knots",
                                     seq.int(0, 5), 3),
                         selectInput("degree", "Degree", seq.int(0, 4), 2))),
        miniTabPanel("Output", icon = icon("table"),
                     miniContentPanel(DT::dataTableOutput("table"))),
        miniTabPanel("Visualize", icon = icon("area-chart"),
                     miniContentPanel(plotOutput("matplot", height = "100%")))
    )
)


server <- function(input, output, session)
{
    x <- seq.int(0, 0.99, 0.01)
    output$matplot <- renderPlot({
        df <- as.numeric(input$nKnots) + as.numeric(input$degree) + 1L
        fun <- switch(input$fun,
                      "B-splines" = "bSpline",
                      "M-splines" = "mSpline",
                      "I-splines" = "iSpline",
                      "C-splines" = "cSpline")
        resMat <- do.call(fun, list(x = x, df = df, degree = input$degree,
                                    intercept = TRUE, Boundary.knots = c(0, 1)))
        matplot(x, resMat, type = "l",
                xlab = "x", ylab = "Spline Bases")
        abline(v = attr(resMat, "knots"), col = "gray", lty = 2)
    })

    output$table <- DT::renderDataTable({
        df <- as.numeric(input$nKnots) + as.numeric(input$degree) + 1L
        fun <- switch(input$fun,
                      "B-splines" = "bSpline",
                      "M-splines" = "mSpline",
                      "I-splines" = "iSpline",
                      "C-splines" = "cSpline")
        resMat <- do.call(fun, list(x = x, df = df, degree = input$degree,
                                    intercept = TRUE, Boundary.knots = c(0, 1)))
        outDat <- data.frame(round(resMat, 3))
        colnames(outDat) <- paste("Spline", seq_len(df))
        cbind(x = x, outDat)
    })

    observeEvent(input$done, {
        stopApp(TRUE)
    })
}


shinyApp(ui, server)
