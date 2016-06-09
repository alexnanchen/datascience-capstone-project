---
title       : Word prediction pitch
subtitle    : Web application for data
author      : Alexandre Nanchen
job         : 
framework   : html5slides   # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides

---

## &nbsp;
<div id="outer" style="position:relative; top:-175px">  
    <h1>Next word prediction Web App</h1>
    <hr>
    <h3>Data-science Capstone project</h3>
    <h4>Alexandre Nanchen</h4>
    <img id="logos"style="margin-left:40px;margin-top:80px;" src="images/logos.png">
</div>

<style>
.slides > article {
    color: rgb(102, 102, 102);
    font-family: "Open Sans",Arial,sans-serif;
    font-size: 25px;
    letter-spacing: -1px;
    line-height: 36px;
}
</style>

--- .class 

## The task

1. Next word prediction using statistical N-gram models
2. Model implementation and evaluation
3. Challenges
    - Good generalization
    - Large amount of data preparation, filtering and tokenization
    - Reduced language model memory footprint
    - Prediction reactivity
    - Robustness to unknown words

---

## Features highlight

1. Training
    - HC corpora data: blogs, news and twitter (> 100 Mio words) 
    - Vocabulary selection to retain 90% of the occurences
    - Out of vocabulary modeling unsing \<unk\> symbol
    - Fast training time < 40 seconds for more than 10 Mio N-grams
    - KNeser-Ney interpolated model of order 4 with fix smoothing
    - Model pruning
2. Evaluation
    - Model and perplexity comparison with <a href='https://github.com/mitlm/mitlm'>MITLM</a> open source toolkit
    - Perplexity comparison per sources (Twitter, Blogs and news) and model type

The full evaluation results are on the Web App.

---

### Predictive algorithm

1. Model compression during loading trough N-gram context hashing
2. Memory footpring reduction of a factor of 2.82
3. Selection of most probable words for each N-gram order using backoff weights and continuation probabilities
4. Averaging of probabilities through all N-gram orders
5. Ordering by decreasing scores and N-gram order

For a detailed description, see the Algorithm tab on the Web App.

---

### Word prediction web app
1. Top 10 words display
2. Verbose mode to see the algorithm in action
3. Words cloud display to see the 50 most frequent words
4. Fast prediction time

[Next word prediction web application](https://alexnanchen.shinyapps.io/nextwordpredictionwebapp/)

<img style="margin-left:0px;margin-top:0px;width:800px;height:300px" src="images/overview.png">


