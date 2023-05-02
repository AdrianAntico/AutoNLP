![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

<img src="https://raw.githubusercontent.com/AdrianAntico/prettydoc/master/Images/AutoNLP.PNG" align="center" width="800" />

# AutoNLP

R automated natural language processing

## Functions

##### CleanData()

##### Sentiment()

##### NGram()

##### Readability()

##### LexicalDiversity()


## Code Examples

#### **CleanData()**


</p>
</details>

<details><summary>Code Example</summary>
<p>

```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)

# Run Function
Output <- AutoNLP::CleanText(
  TrainData = dt,
  ValidationData = NULL,
  TestData = NULL,
  TextColumn = "Comment",
  MergeColumns = "Factor_1",
  RemovePunctuation = TRUE,
  StopWords = "en",
  StopWordsSource = 'stopwords-iso')
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



#### **NGram()**

<details><summary>Code Example</summary>
<p>

```r
# Data
dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)

# Run Function
dt <- AutoNLP::N_Grams(
  dt,
  dt_type = "raw",
  TextColumns = "Comment",
  IDcols = c("Factor_1", "Factor_2"),
  N = 2,
  StopWords = "en",
  StopWordsSource = 'stopwords-iso')
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
dt <- AutoQuant::FakeDataGenerator(N = 1000, AddComment = TRUE)

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
