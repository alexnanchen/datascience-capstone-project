library(dplyr)
library(data.table)
library(wordcloud)

source("code/predictapp.R")

shinyServer(function(input, output) {
  
    #Monitor new button press and get values
    ret <- eventReactive(input$predictButton, {
                            if(input$predictButton > 0) 
                                predictNextWord(input$words)
                            })
            
    #Render prediction summary
    output$summary <- renderTable({
         if(input$predictButton > 0 && as.integer(input$options==1)) {
             head(ret()$dfResult, n=10)
         }
    })
    
    output$verbose <- renderText({
        if(input$predictButton > 0 && as.integer(input$options)==1) {
            verbose <- paste(ret()$log,collapse="<br>")
            verbose <- gsub("<unk>", "&lt;unk&gt;", verbose)
            verbose <- gsub("<s>", "&lt;s&gt;", verbose)
            verbose <- gsub("</s>", "&lt;/s&gt;", verbose)
            verbose <- paste("<h4>Algorithm trace</h4>",verbose)
        }
    })
    
    output$wordCloud <- renderPlot({
        if(input$predictButton > 0 && as.integer(input$options)==2) {
            df <- head(ret()$dfResult, n=50)
            wordcloud(df$word, 500*exp(-as.integer(row.names(df))/20), 
                      colors=brewer.pal(8, "Dark2"),scale=c(5,1), rot.per=0.3, 
                      random.order = F)
        }
    })
})

