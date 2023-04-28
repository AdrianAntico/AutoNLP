#' @title CleanText
#'
#' @description Remove punctuation and stopwords
#'
#' @author Adrian Antico
#' @family Word2Vec
#'
#' @param TrainData Training data. If ValidationData or TestData is supplied, w2v will be created on TrainData only and applied to ValidationData and TestData.
#' @param ValidationData Validation data from a modeling context. TextColumnNames must match TrainData.
#' @param TestData Test data from a modeling context. TextColumnNames must match TrainData.
#' @param TextColumn Character vector of column names to utilize
#' @param MergeColumns Columns used to join to TrainData (and the others). Data is split during stopword removal and must be joined back to source data.
#' @param RemovePunctuation Logical or vector of logical that is the same length as TextColumnNames
#' @param StopWords Default "none". For stopwords package usage supply language code e.g. "en" for english https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes. Otherwise, supply a character vector of stopwords. If you want to supply a modifed version of what's returned from the stopwords package, run this and modify as you wish, stopwords::stopwords(language = 'en', source = 'stopwords-iso')
#' @param StopWordsSource Default "stopwords-iso" and only applies if you supply a language code in the StopWords parameter. Other options include "snowball", "stopwords-iso", "misc", "smart", "marimo", "ancient", "nltk", "perseus".
#'
#' @examples
#' \dontrun {
#'
#' Output <- AutoQuant::PostGRE_Query(
#'   Query = paste0("SELECT * FROM ", shQuote('rawdata')),
#'   Host = 'localhost',
#'   CloseConnection = FALSE,
#'   DBName = 'RemixAutoML',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa1028#@')
#' rawdata <- Output$data
#'
#' output <- CleanText(
#'   TrainData = rawdata,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TextColumn = "reviewText",
#'   MergeColumns = "restoId",
#'   RemovePunctuation = TRUE,
#'   StopWords = "en",
#'   StopWordsSource = 'stopwords-iso')
#' }
#' @export
CleanText <- function(TrainData = NULL,
                      ValidationData = NULL,
                      TestData = NULL,
                      TextColumn = NULL,
                      MergeColumns = NULL,
                      RemovePunctuation = TRUE,
                      StopWords = "none",
                      StopWordsSource = 'stopwords-iso') {

  ####################################### ----
  # DataSet Management                    ----
  ####################################### ----
  if(!data.table::is.data.table(TrainData)) tryCatch({data.table::setDT(TrainData)}, error = function(x) {
    TrainData <- data.table::as.data.table(TrainData)
  })
  if(length(ValidationData) > 0L) {
    if(!data.table::is.data.table(ValidationData)) tryCatch({data.table::setDT(ValidationData)}, error = function(x) {
      ValidationData <- data.table::as.data.table(ValidationData)
    })
  }
  if(length(TestData) > 0L) {
    if(!data.table::is.data.table(TestData)) tryCatch({data.table::setDT(TestData)}, error = function(x) {
      TestData <- data.table::as.data.table(TestData)
    })
  }

  # ----

  # ----

  ####################################### ----
  # Remove Punctuation                    ----
  ####################################### ----

  # TrainData
  train_data <- TrainData[, .SD, .SDcols = c(MergeColumns, TextColumn)]
  train_data <- train_data[, eval(TextColumn) := gsub(
    pattern = '[[:punct:]]+',
    replacement = '',
    x = gsub(
      pattern = '\\\\n|\\.|\\,|\\;',
      replacement = ' ', x = tolower(substr(x = get(TextColumn), start = 1L, nchar(get(TextColumn))))
    )
  )]

  # ValidationData
  if(length(ValidationData) > 0L) {
    validation_data <- ValidationData[, .SD, .SDcols = c(MergeColumns, TextColumn)]
    validation_data <- validation_data[, eval(TextColumn) := gsub(
      pattern = '[[:punct:]]+',
      replacement = '',
      x = gsub(
        pattern = '\\\\n|\\.|\\,|\\;',
        replacement = ' ', x = tolower(substr(x = get(TextColumn), start = 1L, nchar(get(TextColumn))))
      )
    )]
  } else {
    validation_data <- NULL
  }

  # TestData
  if(length(TestData) > 0L) {
    test_data <- TestData[, .SD, .SDcols = c(MergeColumns, TextColumn)]
    test_data <- test_data[, eval(TextColumn) := gsub(
      pattern = '[[:punct:]]+',
      replacement = '',
      x = gsub(
        pattern = '\\\\n|\\.|\\,|\\;',
        replacement = ' ', x = tolower(substr(x = get(TextColumn), start = 1L, nchar(get(TextColumn))))
      )
    )]
  } else {
    test_data <- NULL
  }

  # ----

  # ----

  ####################################### ----
  # StopWord Management                   ----
  ####################################### ----
  if(length(StopWords) == 0L) {
    sw <- NULL
  } else if(length(StopWords) == 1L && tolower(StopWords) == "none") {
    sw <- NULL
  } else if(is.character(StopWords) && length(StopWords) == 1L) {
    if(length(StopWordsSource) == 0L) StopWordsSource <- 'stopwords-iso'
    if(length(StopWordsSource) > 1L) StopWordsSource <- StopWordsSource[1L]
    if(!StopWordsSource %in% c("snowball", "stopwords-iso", "misc", "smart", "marimo", "ancient", "nltk", "perseus")) {
      StopWordsSource <- 'stopwords-iso'
    }
    sw <- stopwords::stopwords(language = tolower(StopWords), source = tolower(StopWordsSource))
  } else if(is.character(StopWords) && length(StopWords) > 1L) {
    sw <- StopWords
  } else {
    sw <- NULL
  }

  # Create data.table with stop words and indicator (for filtering)
  if(length(sw) > 0L) {
    stop_words <- data.table::data.table(word = unique(sw), stopword = 1)

    returnList <- list()
    returnList[["StopWords"]] <- stop_words

    # Tokenize text cols and remove stopwords then join back to source data
    train_data <- tibble::as_tibble(train_data[, .SD, .SDcols = c(MergeColumns, TextColumn)])
    train_data <- data.table::setDT(tidytext::unnest_tokens(tbl = train_data, output = 'word', input = TextColumn))
    train_data <- merge(train_data, stop_words, by = 'word', all.x = TRUE)[is.na(stopword)]
    returnList[["train_data"]] <- train_data

    if(length(validation_data) > 0L) {
      validation_data <- tibble::as_tibble(validation_data[, .SD, .SDcols = c(MergeColumns, TextColumn)])
      validation_data <- data.table::setDT(tidytext::unnest_tokens(tbl = validation_data, output = 'word', input = TextColumn))
      validation_data <- merge(validation_data, stop_words, by = 'word', all.x = TRUE)[is.na(stopword)]
      returnList[["validation_data"]] <- validation_data
    }

    if(length(test_data) > 0L) {
      test_data <- tibble::as_tibble(test_data[, .SD, .SDcols = c(MergeColumns, TextColumn)])
      test_data <- data.table::setDT(tidytext::unnest_tokens(tbl = test_data, output = 'word', input = TextColumn))
      test_data <- merge(test_data, stop_words, by = 'word', all.x = TRUE)[is.na(stopword)]
      returnList[["test_data"]] <- test_data
    }
  }

  # ----

  # ----

  ####################################### ----
  # Return                                ----
  ####################################### ----
  return(returnList)
}

#' @title Sentiment
#'
#' @description Generate readability stats
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param Measures Default is "Flesch". Also available "ARI", "Bormuth.MC", "Bormuth.GP", "Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Flesch.PSK", "Flesch.Kincaid", 'FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "Fucks", "Linsear.Write", "LIW", "nWS", "nWS.2", "nWS.3", "nWS4", "RIX", "Scrabble", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain", "Traenkle.Bailer", "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"
#' @param TextColumns Names of text columns to analyze
#' @param Response Default "numeric" is a sentiment score. "Binary" returns either "positive" or "negative". "Direction" is Binary along with a "neutral" response.
#' @param CombineTextGroupVar A factor variable by which documents can be grouped. This helpful when joining e.g. news from the same day or move reviews by the same author
#' @param Language Default "english"
#' @param RemoveStopWords Logical
#' @param Stemming Logical
#'
#' @export
Sentiment <- function(dt,
                      TextColumns = NULL,
                      Response = "numeric",
                      CombineTextGroupVar = NULL,
                      Language = "english",
                      RemoveStopWords = TRUE,
                      Stemming = TRUE) {

  if(length(TextColumns) > 0L) {
    for(tc in TextColumns) {
      if(tolower(Response) == "binary") {
        dt[, paste0(tc, " Sentiment") := as.character(SentimentAnalysis::convertToBinaryResponse(SentimentAnalysis::analyzeSentiment(
          x = dt[[tc]],
          language = Language,
          aggregate = CombineTextGroupVar,
          removeStopwords = RemoveStopWords,
          stemming = Stemming
        )))]
      } else if(tolower(Response) == "direction") {
        dt[, paste0(tc, " Sentiment") := as.character(SentimentAnalysis::convertToDirection(SentimentAnalysis::analyzeSentiment(
          x = dt[[tc]],
          language = Language,
          aggregate = CombineTextGroupVar,
          removeStopwords = RemoveStopWords,
          stemming = Stemming
        )))]
      } else if(tolower(Response) == "numeric") {
        dt[, paste0(tc, " Sentiment") := SentimentAnalysis::analyzeSentiment(
          x = dt[[tc]],
          language = Language,
          aggregate = CombineTextGroupVar,
          removeStopwords = RemoveStopWords,
          stemming = Stemming
        )]
      }

    }
  }
  return(dt)
}

#' @title N_Gram
#'
#' @description Generate n-gram columns from text columns
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param dt_type Default "raw" which means non-tokenized text columns. "tokenized" means the dt text column has already been tokenized
#' @param N The N in N-Gram
#' @param StopWords Default "none". For stopwords package usage supply language code e.g. "en" for english https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes. Otherwise, supply a character vector of stopwords. If you want to supply a modifed version of what's returned from the stopwords package, run this and modify as you wish, stopwords::stopwords(language = 'en', source = 'stopwords-iso')
#' @param StopWordsSource Default "stopwords-iso" and only applies if you supply a language code in the StopWords parameter. Other options include "snowball", "stopwords-iso", "misc", "smart", "marimo", "ancient", "nltk", "perseus".
#'
#' @export
N_Grams <- function(dt,
                    dt_type = "raw",
                    TextColumns = NULL,
                    IDcols = NULL,
                    N = 2,
                    StopWords = "en",
                    StopWordsSource = 'stopwords-iso') {

  ####################################### ----
  # StopWord Management                   ----
  ####################################### ----
  if(length(StopWords) == 0L) {
    sw <- NULL
  } else if(length(StopWords) == 1L && tolower(StopWords) == "none") {
    sw <- NULL
  } else if(is.character(StopWords) && length(StopWords) == 1L) {
    if(length(StopWordsSource) == 0L) StopWordsSource <- 'stopwords-iso'
    if(length(StopWordsSource) > 1L) StopWordsSource <- StopWordsSource[1L]
    if(!StopWordsSource %in% c("snowball", "stopwords-iso", "misc", "smart", "marimo", "ancient", "nltk", "perseus")) {
      StopWordsSource <- 'stopwords-iso'
    }
    sw <- stopwords::stopwords(language = tolower(StopWords), source = tolower(StopWordsSource))
  } else if(is.character(StopWords) && length(StopWords) > 1L) {
    sw <- StopWords
  } else {
    sw <- NULL
  }

  # Prepare data
  if(tolower(dt_type) != "raw") {
    ngrams <- dt[, list(word = paste(word, collapse = ' ')), by = c(IDcols)]
  } else {
    ngrams <- dt
    data.table::setnames(ngrams, TextColumns, "word")
  }

  # NGrams
  ngrams <- as_tibble(ngrams)
  ngrams <- data.table::setDT(tidytext::unnest_tokens(
    tbl = ngrams,
    output = "ngram",
    token = "ngrams",
    n = N,
    word))

  # remove ngrams containing stopwords
  cols <- c()# i = 1
  for(i in seq_len(N)) cols <- c(cols, paste0("word",i))
  ngrams_united <- ngrams[, (cols) := data.table::tstrsplit(
    x = ngram,
    " ")]
  for(i in seq_len(N)) {
    ngrams_united <- ngrams_united[!get(cols[i]) %in% eval(stop_words$word)]
  }
  return(ngrams_united)
}

#' @title Readability
#'
#' @description Generate readability stats
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param Measures Default is "Flesch". Also available "ARI", "Bormuth.MC", "Bormuth.GP", "Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Flesch.PSK", "Flesch.Kincaid", 'FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "Fucks", "Linsear.Write", "LIW", "nWS", "nWS.2", "nWS.3", "nWS4", "RIX", "Scrabble", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain", "Traenkle.Bailer", "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"
#' @param TextColumns Names of text columns to analyze
#' @param RemoveHyphens TRUE. FALSE to not remove them
#' @param MinSentenceLength Defautl 1
#' @param MaxSentenceLength Default 10000
#' @param Intermediate Logical. TRUE to include intermediate quantities in the output
#'
#' @export
Readability <- function(dt,
                        TextColumns = NULL,
                        Measures = "Flesch",
                        RemoveHyphens = TRUE,
                        MinSentenceLength = 1,
                        MaxSentenceLength = 10000,
                        Intermediate = TRUE) {

  if(length(TextColumns) > 0L) {
    for(tc in TextColumns) {
      for(i in Measures) {
        dt[, paste0(i, " Readability") := quanteda.textstats::textstat_readability(
          dt[[tc]],
          measure = i,
          remove_hyphens = RemoveHyphens,
          min_sentence_length = MinSentenceLength,
          max_sentence_length = MaxSentenceLength,
          intermediate = Intermediate
        )[[2L]]]
      }
    }
  }
  return(dt)
}
