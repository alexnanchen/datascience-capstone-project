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
                    textInput("words", "Input text:", value = "a good", width = "300px"),
                    textOutput("prediction")),
                    br(style="margin: 10px 0;"),
                    radioButtons("options", "Options:",
                                 choices = list("Top 10 words" = 1, "Words cloud" = 2), 
                                 selected = 1),
                    br(style="margin: 15px 0;"),
                    actionButton("predictButton", label="predict"),
                    br(style="margin: 20px 0;"),
                    h4("Examples sentences:"),
                    p(span("how are", class="sentence"),br(),
                      span("what is the", class="sentence"), br(),
                      span("what do you intend", class="sentence"), br(),
                      span("what are ophthamologist (unknown word)", class="sentence"))
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
          tabPanel("Algorithm",
                   mainPanel(
                       div(id="models", class="well",
                           HTML("<h2>N-gram model</h2>
                                 <p>The deployed model is a 4-grams Kneser-Ney backoff model with a vocabulary
                                    of 41'376 words, including the '<i>&lt;unk&gt;</i>' symbol for unknown words.
                                    Here is a details of the counts:</p>
                                      <ul><li>1-gram: 41376 entries</li>
                                          <li>2-gram: 164534 entries</li>
                                          <li>3-gram: 331930 entries</li>
                                          <li>4-gram: 183427 entries</li>
                                      </ul>
                                 <p class='doc'>In order to save memory, part of the text composing the n-gram is converted into
                                    a signed integer. This allows for a gain of memory of a factor of 2.6.
                                 </p>"),
                           HTML("<h2>Algorithm</h2>
                                 <p>The following steps are performed in order to select the best words matches:
                                    <ol><li>Prepare inputed words
                                         <ul><li>Lower case all words</li>
                                             <li>Replace unknow words with the '&lt;unk&gt;' token</li>
                                         </ul>
                                        <li>Select start N-gram from inputed words</li>
                                        <li>Predict using highest order then backoff to lower orders until unigrams are reached</li>
                                        <li>Keep all predictions of all orders with associated scores</li>
                                        <li>Average all available scores for each word</li>
                                        <li>Normalize top 10 scores to sum to 100%</li>
                                        <li>Display results by decreasing model order</li>
                                    </ol>
                                 </p>")
                           )
                       )
                   )
)))
