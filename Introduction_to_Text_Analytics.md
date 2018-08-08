Introduction to Text Alanytics
================
Ravindra Reddy Tamma
8 August 2018

``` r
#install.packages('tm')
library(wordcloud)
```

    ## Warning: package 'wordcloud' was built under R version 3.4.3

    ## Loading required package: RColorBrewer

``` r
library(tm)
```

    ## Warning: package 'tm' was built under R version 3.4.4

    ## Loading required package: NLP

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.4.3

``` r
modi <- fread("E:/Unstructured Data Analysis/Lecture 01 - Introduction to Unstructured Data Analysis/narendramodi_tweets/narendramodi_tweets.csv")
```

\#\# Building the Corpus
------------------------

``` r
# Corpus : It is a collection of Files
# 3 types of Source
#1. Directory source - All files in sub-folders will be Induvidual Files
#2. Vector Source - Contains a vector of values
#3. Data Frame Source - Contains both the labels and text value
docs = VCorpus(VectorSource(modi$text))
docs
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 3220

``` r
inspect(docs[[1]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 136
    ## 
    ## The President's address wonderfully encapsulated India's strengths, aspirations, potential &amp; the efforts towards #TransformingIndia.

``` r
# VCorpus  -  Volatile Corpus will be cleaned off if RStudio is Closed
# PCorpus  -  Permenant Corpus will be peremenantly stored.
inspect(docs[[2]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 133
    ## 
    ## Rashtrapati Ji's address to both Houses of Parliament was in-depth &amp; extensive. Do hear. https://t.co/rdKQtjgNNx @RashtrapatiBhvn

\#\# Basic Transformations
--------------------------

``` r
# This provides a set of Transformations that are commonly done on a Text Data
# These are the built in Transformations present in the tm package
getTransformations()
```

    ## [1] "removeNumbers"     "removePunctuation" "removeWords"      
    ## [4] "stemDocument"      "stripWhitespace"

``` r
# The corpus should be overwritten every time when some transformations are made.
# If the Transformations that needs to be made are not part of tm package then we should use explicit content_transformer
docs = tm_map(docs,content_transformer(tolower))
inspect(docs[[1]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 136
    ## 
    ## the president's address wonderfully encapsulated india's strengths, aspirations, potential &amp; the efforts towards #transformingindia.

\#\# Dealing with Regular Expressions
-------------------------------------

``` r
## Replace a word with specified character
gsub('with','-', 'This is a sentence with 123 and &^%$')
```

    ## [1] "This is a sentence - 123 and &^%$"

``` r
## Replace the Digits in a String
gsub('[0-9]','-', 'This is a sentence with 123 and &^%$')
```

    ## [1] "This is a sentence with --- and &^%$"

``` r
## Replace the Characters in the String
gsub('[a-z]','-', 'This is a sentence with 123 and &^%$')
```

    ## [1] "T--- -- - -------- ---- 123 --- &^%$"

``` r
## Replace the Characters in the String
gsub('[a-zA-Z0-9]','-', 'This is a sentence with 123 and &^%$')
```

    ## [1] "---- -- - -------- ---- --- --- &^%$"

``` r
## Remove Special Characters in the Text.
## ^ (cap) Symbol means that other than operator (i.e not operator)
## This removes all characters other than [a-z]
gsub('[^a-z]','', 'This is a sentence with 123 and &^%$')
```

    ## [1] "hisisasentencewithand"

``` r
## This will remove everything other [a-z]/[A-Z]/( )space
gsub('[^a-zA-Z ]','', 'This is a sentence with 123 and &^%$')
```

    ## [1] "This is a sentence with  and "

``` r
gsub('[^a-zA-Z  #]','', 'This is a sentence with 123 and &^%$#')
```

    ## [1] "This is a sentence with  and #"

``` r
gsub('[^a-zA-Z #@0-9]','', 'This is a sentence with 123 and &^%$#')
```

    ## [1] "This is a sentence with 123 and #"

``` r
apply_regex <- function(s)gsub('[^a-zA-Z0-9 #@]','',s)
docs = VCorpus(VectorSource(modi$text))
docs = tm_map(docs,content_transformer(tolower))
docs = tm_map(docs,content_transformer(apply_regex))
inspect(docs[[1]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 129
    ## 
    ## the presidents address wonderfully encapsulated indias strengths aspirations potential amp the efforts towards #transformingindia

\#\# Stop Words
---------------

``` r
# Stop words are mostly pronouns and articles wich are identified in the Literature
common_stop_words = stopwords()
custom_stop_words = c("amp")
all_stop_words = c(common_stop_words,custom_stop_words)
docs = tm_map(docs,removeWords,all_stop_words)
inspect(docs[[1]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 120
    ## 
    ##  presidents address wonderfully encapsulated indias strengths aspirations potential   efforts towards #transformingindia

\#\# Stem Document
------------------

``` r
# Stemming is the process of Identifying the underlying root word
# Ex. books - book, Playing - play, Languages - Language
# Stemming can also be harmful, because it will remove the tense of the Text.
# It can also convert the entire word into meaningless word.
# So, Generally we donot prefer stemming in our Text Analytics.
inspect(tm_map(docs,stemDocument)[[1]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 92
    ## 
    ## presid address wonder encapsul india strength aspir potenti effort toward #transformingindia

``` r
# docs = tm_map(docs, stemDocument)
```

\#\# Document Term Matrix
-------------------------

``` r
dtm = DocumentTermMatrix(docs)
dtm
```

    ## <<DocumentTermMatrix (documents: 3220, terms: 8813)>>
    ## Non-/sparse entries: 31455/28346405
    ## Sparsity           : 100%
    ## Maximal term length: 54
    ## Weighting          : term frequency (tf)

``` r
#  Caharecteristics of dtm
#1. Weigthing here is term frequency, It may be tfidf which are normalized values
#Documents and Terms
# Sparsity is 100% which means a term may appear cery less all tweets/docs
# Tha above paprmeter mean sparsity around 99%
# Non-Sparsity = 32647 / 28348433
# Maximal term Length = 54, The word with highest length = 54
# Rows are Documents
# Columns are Terms

# Converting dtm to a data frame
df_dtm = as.data.frame(as.matrix(dtm))
dim(df_dtm)
```

    ## [1] 3220 8813

``` r
View(df_dtm[1:10,100:150])
# RowSums will give us the Document Length
# ColSums will give us the Frequency of a particular Term in the Corpus

word_freq = data.frame("Freq" = colSums(df_dtm),word = names(colSums(df_dtm)))
#View(sort(word_freq,decreasing = T))
head(word_freq)
```

    ##                               Freq                          word
    ## #1crpplgaveuplpgsubsidysalute    1 #1crpplgaveuplpgsubsidysalute
    ## #32daystogo                      1                   #32daystogo
    ## #350thprakashparv                3             #350thprakashparv
    ## #70yearsofindependence           1        #70yearsofindependence
    ## #aazadi70saal                    2                 #aazadi70saal
    ## #advancingasiaconference         1      #advancingasiaconference

\#\# Word Cloud
---------------

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(RColorBrewer)
top50_words = word_freq %>% arrange(-Freq) %>% head(50)
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.3

``` r
wordcloud(top50_words$word,top50_words$Freq,colors = brewer.pal(8,'Dark2'),random.color = T,scale = c(5,0.1))
```

![](Introduction_to_Text_Analytics_files/figure-markdown_github/unnamed-chunk-9-1.png)

Bigrams using RWeka
-------------------

``` r
library(RWeka)
```

    ## Warning: package 'RWeka' was built under R version 3.4.4

``` r
# RWeka is used to generate BiGrams from the Documents
BigramTokenizer <- function(x)NGramTokenizer(x,Weka_control(min = 2,max = 2))

dtm_bigrams <- DocumentTermMatrix(docs,control = list(tokenize = BigramTokenizer))

df_dtm_bigram = as.data.frame(as.matrix(dtm_bigrams))
dim(df_dtm_bigram)
```

    ## [1]  3220 24398

``` r
View(df_dtm_bigram[1:10,100:150])
# RowSums will give us the Document Length
# ColSums will give us the Frequency of a particular Term in the Corpus

word_freq = data.frame("Freq" = colSums(df_dtm_bigram),word = names(colSums(df_dtm_bigram)))
#View(sort(word_freq,decreasing = T))
head(word_freq)
```

    ##                  Freq             word
    ## # #                 1              # #
    ## # #azadike70saal    1 # #azadike70saal
    ## # #love             1          # #love
    ## # #tirangayatra     1  # #tirangayatra
    ## # #yogaday          1       # #yogaday
    ## # @abpnewstv        1     # @abpnewstv

``` r
## Word Cloud

library(dplyr)
library(RColorBrewer)
top50_words = word_freq %>% arrange(-Freq) %>% head(50)
wordcloud(top50_words$word,top50_words$Freq,colors = brewer.pal(8,'Dark2'),random.color = T,scale = c(5,0.1))
```

    ## Warning in wordcloud(top50_words$word, top50_words$Freq, colors =
    ## brewer.pal(8, : rt @pmoindia could not be fit on page. It will not be
    ## plotted.

![](Introduction_to_Text_Analytics_files/figure-markdown_github/unnamed-chunk-10-1.png)
