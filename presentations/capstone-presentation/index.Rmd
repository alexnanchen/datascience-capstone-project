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
    <img style="margin-left:40px;margin-top:80px;" src="images/logos.png">
</div>

--- .class 

## The task

1. Next word prediction using statistical n-gram models
2. Model implementation and evaluation
3. Challenges
   - Large amount of data preparation, filtering and tokenization
   - Reduced language model memory footprint
   - Prediction reactivity
   - Robustness to unknown words

---

## Training

1. HC corpora data: blogs, news and twitter (> 100 Mio words) 
2. Vocabulary selection to retain 90% of the occurences
3. Out of vocabulary modeling unsing \<unk\> symbol
4. KNeser-Ney backoff model of order 4
   - Continuation probability
   - Fix KNeser-Ney smoothing
5. Perplexity comparison with other toolkits (srilm, mitlm and irstlm)

---

### Predictive algorithm

1. Model compression during loading trough ngram hashing
2. Memory footpring reduction of a factor of 2.82
3. Selection of most probable words for each N-gram order using backoff weights and continuation probabilities
4. Averaging of probability trough all N-gram orders

---

### Word prediction web app
1. Top 10 words display
2. Verbose mode to see algorithm in action
3. Word cloud display to see 50 top words

[Next word prediction web application]()
