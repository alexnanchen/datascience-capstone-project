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
                      span("my name is", class="sentence"), br(),
                      span("the ophthamologist watch (unknown word)", class="sentence"))
                     ),
                  
                  mainPanel(
                     tags$br(),
                     conditionalPanel(condition="input.predictButton>0 && input.options==1",
                                      div(id="topTitle", h4("Top 10 words")),
                                      htmlOutput("summary"),
                                      htmlOutput("verbose")),
                
                     conditionalPanel(condition="input.predictButton>0 && input.options==2",
                                      plotOutput("wordCloud", width="800px")),
                     
                     conditionalPanel(condition="input.predictButton==0",
                                        div(id="welcome",
                                            HTML("<h2 class='centerfull'>Welcome to the</h2>
                                                  <h2 class='centerfullmargin'>Next Word Prediction Web App!</h2>
                                                  <p class='pbmsmall'>This application is part of the Capstone Project of the 
                                                      <a target='_blank' href='https://www.coursera.org/specializations/jhu-data-science'>
                                                         Data Science Specialization</a> on Coursera from Johns Hopkins University.</p>
                                                  <p class='pbmsmall'>Using the <span class='highlight'>input box</span> on the left, you can enter a sentence or part of sentence and see
                                                      which word the algorithm will predict as a next word.</p>
                                                  <p class='pbmsmall'><span class='highlight'>A words cloud</span> is also available to see the 50 potential most frequent words that
                                                      will complete your text input.</p>
                                                  <p class='pbmsmall'>A fun think to do is to try building a sentence using the predicted words.</p>
                                                  <p class='pbmsmall'>Please refer to the top menu for help and other information.</p>
                                                  <p class='pbmsmall'>Have fun!</p>
                                                 ")
                                        )
                                      )
                  )
              )
          ),
          tabPanel("Help",
                   mainPanel(
                        div(id="trace", class="well-large",
                           HTML("<p></p><p>The trace of the backing off algorithm can be a little challenging to understand.
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
                       div(id="models", class="well-large",
                           HTML("<h2 style=margin-bottom:20px>N-gram model</h2>
                                 <p>The deployed model is a <b>4-grams Kneser-Ney interpolated </b> model with a vocabulary
                                    of 50'217 words, including the '<i>&lt;unk&gt;</i>' symbol for unknown words.</p>
                                 <p>The model parameters and its performance have the same values as the one obtained
                                    with the <a target='_blank' href='https://github.com/mitlm/mitlm'>MITLM toolkit</a>
                                    when setting the smoothing parameter to <b>FixKN</b>.</p>
                                  <p> Here is a details of the counts:</p>
                                      <ul><li>1-gram: 50'217</li>
                                          <li>2-grams: 1'615'062</li>
                                          <li>3-grams: 3'949'634</li>
                                          <li>4-grams: 4'979'310</li>
                                      </ul>
                                 <p class='doc'>In order to save memory, part of the text composing the n-gram is converted into
                                    a signed integer. This allows for a reduction of memory footpring of a factor of <b>2.8</b>.
                                 </p>"),
                           HTML("<h2 style=margin-bottom:20px>Algorithm</h2>
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
                                        <li>Display results by decreasing score and model order</li>
                                    </ol>
                                 </p>")
                           )
                       )
                   ),
          tabPanel("Evaluation",
                   mainPanel(
                       div(id="evaluation", class="well-large shiny-html-output",
                           HTML("<h2 style=margin-bottom:20px>Model evaluation</h2><br>
                                 <p>Here is a summary of the perplexity measures for different models
                                    against different test sets:</p><br><br>"),
                           tableOutput("results"),
                           HTML("<p>The unpruned model contains:
                                    <ul><li>2-grams: 1'615'062</li>
                                        <li>3-grams: 3'949'634</li>
                                        <li>4-grams: 4'979'310</li>
                                    </ul>
                                 </p>
                                 <p>The pruned model contains:
                                    <ul><li>2-grams: 237'527</li>
                                        <li>3-grams: 429'717</li>
                                        <li>4-grams: 220'105</li>
                                    </ul>
                                 </p>
                                 <p>Both models have a vocabulary of size <b>50'217</b> words.</p>
                                 <p>From the table we can see that the best perplexity (330.15) is obtained
                                    from the unpruned interpolated model while the deployed model
                                    achieves a perplexity of <b>449.74</b>.</p>
                                 <p>The pruning was done by removal of n-grams below
                                    a count threshold and need some improving.</p>"
                            )
                       )
                   )
          ),
          tabPanel("Bibliography",
                   mainPanel(
                       div(id="bibliography", class="well-large shiny-html-output",
                           HTML("<h2 style=margin-bottom:20px>Bibliography</h2><br>
                                 <p>Here is a list of the main resources used to understand
                                    and implement the algorithm:</p>
                                 <p>Stanley F. Chen and Joshua Goodman. 1998. <a target='_blank' href='http://www.speech.sri.com/projects/srilm/manpages/pdfs/chen-goodman-tr-10-98.pdf'>An Empirical Study of Smoothing Techniques for Language Modeling</a></p>
                                 <p>Daniel Jurafsky & James H. Martin. 2014. <a target='_blank' href='https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf'>Speech and Language Processing Chapter 4 N-Grams</a></p>
                                 <p>Jon Gauthier. 2014. <a target='_blank' href='http://www.foldl.me/2014/kneser-ney-smoothing/'>Kneser-Ney smoothing explained</a> </p>
                                 <p>Roger Levy. Winter 2008. <a target='_blank' href='http://idiom.ucsd.edu/~rlevy/lign256/winter2008/kneser_ney_mini_example.pdf'>Kneser-Ney mini-example</a>. Linguistics/CSE 256, page 2</p>
                                ")
                       )
                   )
          ),
          tabPanel("Code and other",
                   mainPanel(
                       div(id="resources", class="well-large shiny-html-output",
                           HTML("<h2 style=margin-bottom:20px>Resources</h2><br>
                                <p>The code is publicly available on <a target='_blank' href='https://github.com/alexnanchen/datascience-capstone-project.git'>github</a>.</p>
                                <p>A <a target='_blank' href='http://rpubs.com/alexnanchen/capstonepitchdeck'>Pitch Deck</a> is also available.</p>
                                <p>During the specialization many data analyses where performed. You can find them on <a target='_blank' href='http://rpubs.com/alexnanchen'>RPubs</a>.
                                ")
                       )
                   )
          )
)))
