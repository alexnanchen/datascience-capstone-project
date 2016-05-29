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
                      span("the rest of", class="sentence"), br(),
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
          tabPanel("Help",
                   mainPanel(
                        div(id="trace", class="well-trace",
                           HTML("<p>The trace of the backing off algorithm can be a little challenging to understand.
                                 Here is a detail explanation of the next word prediction for:</p>
                                 <p class='keywords'><b>\"the rest of\"</b></p>
                                 <p class='center'><img src='algorithm.jpg'></p>
                                 <p class='pbm'>
                                    <ol>
                                    <li>A 4-gram search is performed (the star denote any distinct word)</li>
                                    <li>21 results are found, no backoff is added (numbers are in log10 domain)</li>
                                    <li>\"the rest\" words are hashed (a trick to reduce memory footprint). The word \"of\" is appended</li>
                                    <li>A backoff weight of -1.45 has been found. This correspond to the relative (to higher order) free mass to share</li>
                                    <li>A 3-gram search is performed and yield two results. The score of the results are adjusted with the backoff weight</li>
                                    <li>The algorithm search for a backoff value for \"rest of\" in bigram model, without success</li>
                                    <li>The algorithm search for a backoff value for \"of\" in the unigram model, with success</li>
                                    <li>All unigram words are potential results. They are adjusted with the found backoff weight</li>
                                    </ol>
                                 </p>
                                 <p style='margin-top:2em'>
                                  Finally, for each found word, all scores for all orders (here 1-gram, 3-gram and 4-gram) are
                                  averaged without forgetting to move to the probability domain before averaging.
                                 </p>")
                          )       
                   )),
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
                                    a signed integer. This allows for a reduction of memory footpring of a factor of 2.8.
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
