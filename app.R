library(factoextra)
library(shiny)
ui <- fluidPage(
    titlePanel("PCA Tool"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("mc",
                        "x1 x2 %shared variance",
                        min = -100,
                        max = 100,
                        value = 0,
                        step = 1)
            ),
        mainPanel(
           plotOutput("pcaPlot")
        )
        )
)
server <- function(input, output) {

    output$pcaPlot <- renderPlot({
        mc<-input$mc
        x1<-rnorm(100)
        x2<-(mc*x1)+(100-abs(mc))*rnorm(100) #generates an x2 with a variance composition specified by input
        df<-data.frame(x1,x2)
        df.pca<-prcomp(df,scale=TRUE)
        fviz_pca_var(df.pca,repel=TRUE)
    })
}
shinyApp(ui = ui, server = server)