library(shiny)
library(wordcloud)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Predict Re-Admission Project "),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            

            selectInput(inputId = "dataset",
                        label = "COHORT (Target):",
                        choices = c("747 : SCYou:Total admission",
                                    "748 : SCYou:ED visit")),
            
            selectInput(inputId = "dataset1",
                        label = "COHORT (Outcome):",
                        choices = c("748 : SCYou:ED visit",
                                    "747 : SCYou:Total admission")),
            
            selectInput(inputId = "dataset2",
                        label = "Note Type",
                        choices = c("Discharge_record",
                                    "extra_record")),
            
            actionButton("goButton", "OK")

            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: HTML table with requested number of observations ----
            plotOutput("Plot1")
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    datasetInput <- eventReactive(input$goButton,{
        switch(input$dataset,
               "747 : SCYou:Total admission" = 조건1,
               "748 : SCYou:ED visit" = 조건2)
    }, ignoreNULL = FALSE)
    
    
    data1 =eventReactive(input$goButton,{
        d <- wordcloud_real#조건에 따라 바뀔 수 있다.
        #d <- wordcloud_no
        list (d1=d)}, ignoreNULL = FALSE)
    
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$Plot1 <- renderPlot({
        
        data2=data1()$d1  
        set.seed(3)
        wordcloud_rep(words = data2$word, freq = data2$freq,scale= c(4,1),min.freq = 2,
                      max.words=200, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))   },height = 800, width = 1000)
    
}

# Create Shiny app ----
shinyApp(ui, server)

?wordcloud
