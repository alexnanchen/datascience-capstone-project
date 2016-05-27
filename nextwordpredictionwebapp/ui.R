library(shiny)

shinyUI(
    fluidPage(title="Next word prediction Web App",
        navbarPage(
          title=div(span("Next word prediction Web App", class="title"), 
                    img(src="cousera.png", height="25px"),
                    img(src="swiftkey.png", height="25px")),
          theme = "style.css",
          tabPanel("Application",
              sidebarLayout(
                  sidebarPanel(
                    div(
                    textInput("words", "Input text:", value = "a", width = "300px"),
                    textOutput("prediction")),
                    br(style="margin: 10px 0;"),
                    radioButtons("options", "Options:",
                                 choices = list("Top 10 words" = 1, "Words cloud" = 2), 
                                 selected = 1),
                    br(style="margin: 15px 0;"),
                    actionButton("predictButton", label="predict"),
                    br(style="margin: 20px 0;"),
                    h4("Examples sentences:"),
                    p(span("how are you", class="sentence"),br(),
                      span("what is the", class="sentence"), br(),
                      span("a good", class="sentence"))
                     ),
                  
                  mainPanel(
                     tags$br(),
                     conditionalPanel(condition="input.predictButton>0 && input.options==1",
                                      div(id="topTitle", h4("Top 10 words")),
                                      htmlOutput("summary"),
                                      htmlOutput("verbose")),
                
                     conditionalPanel(condition="input.predictButton>0 && input.options==2",
                                      plotOutput("wordCloud", width="800px"))
                  )
              )
          ),
          tabPanel("Help"),
          tabPanel("Algorithm")
)))
