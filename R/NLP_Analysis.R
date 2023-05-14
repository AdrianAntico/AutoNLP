# AutoNLP is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title TextSummary
#'
#' @description Generate text summary stats
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param RemoveStats NULL. If you want any metrics suppressed, supply as a character vector. Metrics include, "document", "chars", "sents", "tokens", "types", "puncts", "numbers", "symbols", "urls", "tags", "emojis"
#'
#' @export
TextSummary <- function(dt = NULL,
                        TextColumns = NULL,
                        RemoveStats = NULL) {
  library(quanteda)
  for(tc in TextColumns) {# tc = "Comment"
    cols <- c("document", "chars", "sents", "tokens", "types", "puncts", "numbers", "symbols", "urls", "tags", "emojis")
    dt[, paste0(tc, " ", cols) := quanteda.textstats::textstat_summary(quanteda::tokens(dt[[tc]]))]
    if(length(RemoveStats) > 0L) {
      data.table::set(dt, j = paste0(tc, " ", RemoveStats), value = NULL)
    }
  }
  return(dt)
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

  library(SentimentAnalysis)
  if(length(TextColumns) > 0L) {
    cols <- c("WordCount", "SentimentGI", "NegativityGI", "PositivityGI", "SentimentHE", "NegativityHE", "PositivityHE", "SentimentLM", "NegativityLM", "PositivityLM", "RatioUncertaintyLM", "SentimentQDAP", "NegativityQDAP", "PositivityQDAP")
    for(tc in TextColumns) {# tc = "Comment"
      if(tolower(Response) == "binary") {
        dt[, paste0(tc, cols) := as.character(SentimentAnalysis::convertToBinaryResponse(SentimentAnalysis::analyzeSentiment(
          x = dt[[tc]],
          language = Language,
          aggregate = CombineTextGroupVar,
          removeStopwords = RemoveStopWords,
          stemming = Stemming
        )))]
      } else if(tolower(Response) == "direction") {
        dt[, paste0(tc, cols) := as.character(SentimentAnalysis::convertToDirection(SentimentAnalysis::analyzeSentiment(
          x = dt[[tc]],
          language = Language,
          aggregate = CombineTextGroupVar,
          removeStopwords = RemoveStopWords,
          stemming = Stemming
        )))]
      } else if(tolower(Response) == "numeric") {
        dt[, paste0(tc, cols) := SentimentAnalysis::analyzeSentiment(
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

#' @title Readability
#'
#' @description Generate readability stats
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param Measures Default is "Flesch". Also available "ARI", "Bormuth.MC", "Bormuth.GP", "Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Flesch.PSK", "Flesch.Kincaid", "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "Fucks", "Linsear.Write", "LIW", "nWS", "nWS.2", "nWS.3", "nWS4", "RIX", "Scrabble", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain", "Traenkle.Bailer", "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"
#' @param dt data.table
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

  library(quanteda)
  if(length(TextColumns) > 0L) {
    for(tc in TextColumns) {# tc = "Comment"
      for(i in Measures) {
        if(Intermediate) {
          dt <- cbind(dt, quanteda.textstats::textstat_readability(
            dt[[tc]],
            measure = i,
            remove_hyphens = RemoveHyphens,
            min_sentence_length = MinSentenceLength,
            max_sentence_length = MaxSentenceLength,
            intermediate = Intermediate
          ))
        } else {
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
  }
  return(dt)
}

#' @title LexicalDiversity
#'
#' @description Generate lexical divrsity stats
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param Measures Default is "TTR". Also available "C", "R", "CTTR", "U", "S", "K", "I", "D", "Vm", "Maas", "MATTR", "MSTTR", "all"
#' @param TextColumns Names of text columns to analyze
#' @param RemoveHyphens Logical
#' @param RemoveSymbols Logical
#' @param RemovePunctuation Logical
#' @param RemoveNumbers Logical
#' @param MATTR_Window Numeric value defining the size of the moving average window for computation of the Moving-Average Type-Token Ration
#' @param MSTTR_Segment Numeric value defining the size of each segment for the computation of the Mean Segmental Type-Token Ratio
#'
#' @export
LexicalDiversity <- function(dt,
                             TextColumns = NULL,
                             Measures = "TTR",
                             RemoveSymbols = TRUE,
                             RemoveHyphens = TRUE,
                             RemovePunctuation = TRUE,
                             RemoveNumbers = TRUE,
                             LogBase = 10,
                             MATTR_Window = 100L,
                             MSTTR_Segment = 100L) {

  library(quanteda)
  for(tc in TextColumns) {# tc = "Comment"
    for(m in Measures) {# m = "CTTR"
      dt[, paste0(tc, " ", m, " ", "LexicalDiversity") := quanteda.textstats::textstat_lexdiv(
        quanteda::tokens(dt[[tc]]),
        measure = m,
        remove_numbers = RemoveNumbers,
        remove_punct = RemovePunctuation,
        remove_symbols = RemoveSymbols,
        remove_hyphens = RemoveHyphens,
        log.base = LogBase,
        MATTR_window = MATTR_Window,
        MSTTR_segment = MSTTR_Segment)[[2L]]]
    }
  }
  return(dt)
}

#' @title TextColsSimilarity
#'
#' @description Generate similarity metrics between two text columns
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param TextCol1 Text column 1
#' @param TextCol2 Text column 2
#' @param Margin "documents" or "features"
#' @param Method "correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamann", "simple matching"
#'
#' @examples
#' \dontrun{
#' dt <- AutoNLP::FakeDataGenerator(N=1000, AddComment = TRUE)
#' dt1 <- AutoNLP::FakeDataGenerator(N=1000, AddComment = TRUE)
#' dt[, Comment2 := dt1$Comment]
#' TextCol1 <- "Comment"
#' TextCol2 <- "Comment2"
#' }
#'
#' @export
TextColsSimilarity <- function(dt,
                               TextCol1 = NULL,
                               TextCol2 = NULL,
                               Margin = "documents",
                               Method = "cosine",
                               MaxSampleSize = 10000) {

  if(dt[,.N] > SampleSize) dt1 <- dt[seq_len(MaxSampleSize)] else dt1 <- data.table::copy(dt)

  library(quanteda)
  for(i in Method) {
    x <- quanteda.textstats::textstat_simil(
      x = quanteda::dfm(quanteda::tokens(dt1[[TextCol1]])),
      y = quanteda::dfm(quanteda::tokens(dt1[[TextCol2]])),
      margin = Margin,
      method = i)
    gg <- data.table::as.data.table(x)
    tc1 <- gg[, mean(get(i)), by = "document1"][order(document1)]
    tc2 <- gg[, mean(get(i)), by = "document2"][order(document2)]
    dt1[, paste0(TextCol1, " sim ", TextCol2, " ", i) := tc1$V1]
    dt1[, paste0(TextCol2, " sim ", TextCol1, " ", i) := tc2$V1]
  }
  return(dt1)
}

#' @title TextColsDistance
#'
#' @description Generate distance metrics between two text columns
#'
#' @author Adrian Antico
#' @family NLP Stats
#'
#' @param dt data.table
#' @param TextCol1 Text column 1
#' @param TextCol2 Text column 2
#' @param Margin "documents" or "features"
#' @param DistanceMeasure "euclidean", "manhattan", "maximum", "canberra", "minkowski"
#' @param MinkowskiPower Default 2. Only used when DistanceMeasure is "minkowski"
#' @param MaxSampleSize Default 10000
#'
#' @examples
#' \dontrun{
#' dt <- AutoNLP::FakeDataGenerator(N=10000, AddComment = TRUE)
#' dt1 <- AutoNLP::FakeDataGenerator(N=10000, AddComment = TRUE)
#' dt[, Comment2 := dt1$Comment]
#' TextCol1 <- "Comment"
#' TextCol2 <- "Comment2"
#' }
#'
#' @export
TextColsDistance <- function(dt,
                             TextCol1 = NULL,
                             TextCol2 = NULL,
                             Margin = "documents",
                             DistanceMeasure = "euclidean",
                             MinkowskiPower = 2,
                             MaxSampleSize = 10000) {

  library(quanteda)
  for(i in DistanceMeasure) {
    x <- quanteda.textstats::textstat_dist(
      x = quanteda::dfm(quanteda::tokens(dt[[TextCol1]])),
      y = quanteda::dfm(quanteda::tokens(dt[[TextCol2]])),
      margin = Margin,
      method = i)
    gg <- data.table::as.data.table(x)
    tc1 <- gg[, mean(get(i)), by = "document1"][order(document1)]
    tc2 <- gg[, mean(get(i)), by = "document2"][order(document2)]
    dt[, paste0(TextCol1, " dist ", TextCol2, " ", i) := tc1$V1]
    dt[, paste0(TextCol2, " dist ", TextCol1, " ", i) := tc2$V1]
  }
  return(dt)
}

