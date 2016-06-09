Next word prediction Web App
============================
author: Alexandre Nanchen
date: July 2016
css: style.css

<div id="titlelogos"> 
    <img id="logos" src="pitchdeck-figure/logos.png">
</div>

The task
========
1. Next word prediction using statistical N-gram models
2. Model implementation and evaluation
3. Challenges
    - Good generalization
    - Large amount of data preparation, filtering and tokenization
    - Reduced language model memory footprint
    - Prediction reactivity
    - Robustness to unknown words

Features highlight
==================
1. Training
    - HC corpora data: blogs, news and twitter (> 100 Mio words) 
    - Vocabulary selection to retain 90% of the occurences
    - Out of vocabulary modeling unsing \<unk\> symbol
    - Fast training time < 40 seconds for more than 10 Mio N-grams
    - KNeser-Ney interpolated model of order 4 with fix smoothing
    - Model pruning
2. Evaluation
    - Model and perplexity comparison with <a target="_blank" href='https://github.com/mitlm/mitlm'>MITLM</a> open source toolkit
    - Perplexity comparison per sources (Twitter, Blogs and news) and model type

The full evaluation results are on the Web App.

Predictive algorithm
====================
1. Model compression during loading trough N-gram context hashing
2. Memory footpring reduction of a factor of 2.82
3. Selection of most probable words for each N-gram order using backoff weights and continuation probabilities
4. Averaging of probabilities through all N-gram orders
5. Ordering by decreasing scores and N-gram order

For a detailed description, see the Algorithm tab on the Web App.

Word prediction web app
=======================
1. Top 10 words display
2. Verbose mode to see the algorithm in action
3. Words cloud display to see the 50 most frequent words
4. Fast prediction time

[Next word prediction web application](https://alexnanchen.shinyapps.io/nextwordpredictionwebapp/)

<img style="margin-left:0px;margin-top:0px;width:800px;height:300px" src="pitchdeck-figure/overview.png">

