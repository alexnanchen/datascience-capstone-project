README
=======
README for the data-science capstone project.

### File structure
The file structure is has following:
``` 
- data : original data
    - en_US
        - en_US.blogs.txt
        - en_US.news.txt
        - en_US.twitter.txt
- sample: sampled data
- clean: prepared data
- datascience-capstone-project: git
    - setenv.R
 ``` 

### Training
Here  are the steps to train the models

1. source("datascience-capstone-project/setenv.R")
2. source("datasplit.R") - Split the data into trainin, development and testing sets
3. source("datasampling.R") - Sample from training set 300'000 sentences, 100'000 per source
4. source("datapreparation.R") - Clean and prepare all data sets
5. source("extractcounts.R")
    - Vocabulary extraction. Interactive process where the user has to check the vocabulary list
      (careful not removing \<s\> and \</s\>)
    - 1 to 4 grams extraction
    - \<unk\> token for words that are not in the vocabulary list
    - Check that counts[1-4].txt files are ok
    - Check that training.txt is ok
6. source("training.R")
    - Unpruned model training
    - Model training with mitlm
  ```
    sed 's/<s> //g' < training.txt | sed 's#</s>##g' > training_raw.txt
    estimate-ngram -text training_raw.txt -order 4 -smoothing FixKN -write-lm mitlm4.arpa
  ```
    - Models comparison: counts, logprob and backoff weight.
      For same counts, results should be the same. Counts may differ due to utf-8 characters.
    - Prune model training: change PRUNE variable to TRUE and re-source "training.R"
  ```
    1-grams 50217 
    2-grams 237527 
    3-grams 429717 
    4-grams 220105 
  ```
8. source("evaluate.R")
   - Evaluation on twitter test set (it seems the set matching the most our use case). The script take
     some time, it should be optimized.
   - Perplexity should be around **400** with no out of vocabulary.
 ```
 Total log: -34665.43 - nb words: 12365 - oov: 0 nb sentences: 979 - ppl: 396.122
 ```
7. source("predict.R")
    - Compression of trained models
    - Prediction of next word for "what do you"

8. Deployment
    - Copy ngram[1-4]c.txt and vocabulary.txt into nextwordpredictionwebapp/models
