library(shiny)
library(miniUI)
library(DT)
library(splines2)


ui <- miniPage(
    gadgetTitleBar("Splines Explorer"),
    miniTabstripPanel(
        miniTabPanel("Input", icon = icon("sliders"),
                     miniContentPanel(
                         selectInput("fun", "Spline Function",
                                     c("B-splines", "M-splines",
                                       "I-splines", "C-splines")),
                         selectInput("nKnots",
                                     paste("Number of Internal Knots",
                                           "(equidistant)"),
                                     seq.int(0, 8), 3),
                         selectInput("degree", "Degree", seq.int(0, 3), 2),
                         checkboxInput('intercept', 'Intercept', value = TRUE),
                         sliderInput("lwd", "Line Width", min = 0.5, max = 3,
                                     value = 1.5, step = 0.5))),
        miniTabPanel("Output", icon = icon("table"),
                     miniContentPanel(DT::dataTableOutput("table"))),
        miniTabPanel("Visualize", icon = icon("area-chart"),
                     miniContentPanel(padding = 0,
                                      plotOutput("matplot", height = "100%"))),
        selected = "Visualize"
    )
)


server <- function(input, output, session)
{
    x <- seq.int(0, 1, 0.01)
    output$matplot <- renderPlot({
        fun <- switch(input$fun,
                      "B-splines" = "bSpline",
                      "M-splines" = "mSpline",
                      "I-splines" = "iSpline",
                      "C-splines" = "cSpline")
        df <- as.integer(input$nKnots) + as.integer(input$degree) +
            as.integer(input$intercept)
        resMat <- do.call(fun, list(x = x, df = df, degree = input$degree,
                                    intercept = input$intercept))
        par(mar = c(2.5, 2.5, 0, 0), mgp = c(1.5, 0.5, 0))
        matplot(x, resMat, type = "l", xlab = "x",
                ylab = "Spline Bases", lwd = input$lwd)
        abline(v = attr(resMat, "knots"), col = "gray",
               lty = 2, lwd = input$lwd)
    })

    output$table <- DT::renderDataTable({
        fun <- switch(input$fun,
                      "B-splines" = "bSpline",
                      "M-splines" = "mSpline",
                      "I-splines" = "iSpline",
                      "C-splines" = "cSpline")
        df <- as.integer(input$nKnots) + as.integer(input$degree) +
            as.integer(input$intercept)
        resMat <- do.call(fun, list(x = x, df = df, degree = input$degree,
                                    intercept = input$intercept))
        outDat <- data.frame(round(resMat, 3))
        colnames(outDat) <- paste("Spline", seq_len(df))
        cbind(x = x, outDat)
    })

    observeEvent(input$done, {
        stopApp(TRUE)
    })
}


shinyApp(ui, server)
