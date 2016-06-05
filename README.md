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
    - 1 to 4 grams extraction
    - \<unk\> token for words that are not in the vocabulary list
6. source("training.R")
