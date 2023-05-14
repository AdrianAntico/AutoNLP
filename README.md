![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

<img src="https://raw.githubusercontent.com/AdrianAntico/prettydoc/master/Images/AutoNLP.PNG" align="center" width="800" />

# AutoNLP

R automated natural language processing. The intent is to abstract away the nuances of running various functions and to enable users to generate output (or new columns) on any number of text columns you have in a dataset. I'm not sure yet how this package will evolve but there are a lot of possible directions. 

## Install AutoNLP
 
```r
install.packages('bit64')
install.packages('data.table')
install.packages('tidytext')
install.packages('tibble')
install.packages('stopwords')
install.packages('SentimentAnalysis')
install.packages('quanteda')
install.packages('quanteda.textstats')
devtools::install_github("AdrianAntico/AutoNLP", upgrade = FALSE)
```

## Functions

##### TextSummary()

##### Sentiment()

##### Readability()

##### LexicalDiversity()

##### TextColsSimilarity()

## Code Examples

#### **TextSummary()**


</p>
</details>

<details><summary>Code Example</summary>
<p>

```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)

# Run Function
dt <- AutoNLP::TextSummary(
  dt = dt,
  TextColumns = "Comment",
  RemoveStats = NULL)
```

</p>
</details>

<details><summary>Function Description</summary>
<p>

Stuff here 

</p>
</details>

#### **Sentiment()**

<details><summary>Code Example</summary>
<p>
 
```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)

# Run Function
dt <- AutoNLP::Sentiment(
  dt,
  TextColumns = "Comment",
  Response = "numeric",
  CombineTextGroupVar = NULL,
  Language = "english",
  RemoveStopWords = TRUE,
  Stemming = TRUE)
```

</p>
</details>

<details><summary>Function Description</summary>
<p>
 
Stuff here
 
</p>
</details>

#### **Readability()**

<details><summary>Code Example</summary>
<p>

```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)

# Run Function
dt <- AutoNLP::Readability(
  dt,
  TextColumns = "Comment",
  Measures = "Flesch",
  RemoveHyphens = TRUE,
  MinSentenceLength = 1,
  MaxSentenceLength = 10000,
  Intermediate = TRUE)
```

</p>
</details>

<details><summary>Function Description</summary>
<p>

Stuff here

</p>
</details>


#### **LexicalDiversity()**

<details><summary>Code Example</summary>
<p>

```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)

# Run Function
dt <- AutoNLP::LexicalDiversity(
  dt,
  TextColumns = "Comment",
  Measures = "TTR",
  RemoveSymbols = TRUE,
  RemoveHyphens = TRUE,
  RemovePunctuation = TRUE,
  RemoveNumbers = TRUE,
  LogBase = 10,
  MATTR_Window = 100L,
  MSTTR_Segment = 100L)
```

</p>
</details>

<details><summary>Function Description</summary>
<p>

Stuff here

</p>
</details>



#### **TextColsSimilarity()**

<details><summary>Code Example</summary>
<p>

```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)
dt2 <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)
dt[, Comment2 := dt2$Comment]

# Run Function
dt <- AutoNLP::TextColsSimilarity(
  dt,
  TextCol1 = "Comment",
  TextCol2 = "Comment2")
```

</p>
</details>

<details><summary>Function Description</summary>
<p>

Stuff here

</p>
</details>

