# WORDCLOUD - DEBATE PRESIDENCIAL 2016
# AUTORIA: JEANNE FRANCO
# DATA: 30/04/2021

# Informacoes sobre os pacotes  -------------------------------------------

# Required R packages
# The following packages are required for the rquery.wordcloud() function :

# tm for text mining
# SnowballC for text stemming
# wordcloud for generating word cloud images
# RCurl and XML packages to download and parse web pages
# RColorBrewer for color palettes


# Pacotes -------------------------------------------------------------------------------------

install.packages(c("tm", "SnowballC", "wordcloud", 
                   "RColorBrewer", "RCurl", "XML"))
library("tm") # cria vetor apenas contendo texto
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("bitops")
library("ggplot2")
#install.packages("ggwordcloud")
#library("ggwordcloud")
#install.packages("wordcloud2")
#library("wordcloud2") 

# Informacoes de codigos --------------------------------------------------

# x : character string (plain text, web URL, txt file path)
# type : specify whether x is a plain text, a web page URL or a .txt file path
# lang : the language of the text. This is important to be specified in order to remove the common stopwords (like 'the', 'we', 'is', 'are') from the text before further analysis. Supported languages are danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish and swedish.
# excludeWords : a vector containing your own stopwords to be eliminated from the text. e.g : c("word1", "word2")
# textStemming : reduces words to their root form. Default value is FALSE. A stemming process reduces the words "moving" and "movement" to the root word, "move".
# colorPalette : Possible values are :
# a name of color palette taken from RColorBrewer package (e.g.: colorPalette = "Dark2")
# color name (e.g. : colorPalette = "red")
# a color code (e.g. : colorPalette = "#FF1245")
#min.freq : words with frequency below min.freq will not be plotted
#max.words : maximum number of words to be plotted. least frequent terms dropped



# Carregar dados ------------------------------------------------------------------------------

#source('http://www.sthda.com/upload/rquery_wordcloud.r')
#url = "https://storage.googleapis.com/kagglesdsdata/datasets/902905/1531441/presidential_debate_transcript.txt?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210430%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210430T221119Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=3913841b6c3cd9580fc4a10f08e792ac44aa08f16ef5c0e566d6cccca1ad40c44d78f872a8c56dc5e945433d4571412a5c11d43b485deb8582b809ab9a77d78f7976c44dad70c5170784cb993a8434a7086af784dc8cab9493ad5733041e5cd30eb0e45964b2d3a1e042a566e7d9df238d03718a0a7a4e97400b53a70206c4002d86cdd269d0170d8e0134e40946f7e651f4044b5da4dcd2d8e8db6826bc0bf25f5ff536523fa363467cfa12a9a60cc07f623f4b0177283e0e6fa28f3fd2b18cf9b04ebffa5a5c6f46b99b2c2a27d23c039edc6c47414d5c3d4086302210145374d9ec82d41b3d2d2d6b07325e2a637dda587accdcf539d1d96b73f59523ab0c"
text <- readLines(file.choose())

# Wordcloud Debate presidência 2020 ----------------------------------------------------------------------------------------
# Biden and Trump

docs <- Corpus(VectorSource(text))
inspect(docs)

# Cleaning text

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("the","trump", "biden", 
                                    "wallace", "you", "an",
                                    "that", "have", "not",
                                    "dont", "because", "are",
                                    "your", "but", "this",
                                    "crosstalk", "will", "well",
                                    "sir", "just", "say", "joe",
                                    "even", "ask", "minute", "came",
                                    "answer", "wait", "yes",
                                    "one", "two", "question",
                                    "number", "hillary", "clinton",
                                    "trump", "donald", "get", "got",
                                    "let", "holt", "kaine")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
d

set.seed(1234)
par(bg="black") 
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=Inf, random.order=FALSE, rot.per=0.4, 
          colors=brewer.pal(8, "Set3"), scale = c(3, .4), 
          backgroundColor="black")
warnings()

# scale = c(3, 0.4)

# Gráfico de barras - Frequência --------------------------------------------------------------

library(tibble)
library(tidyr)
library(dplyr)
library(forcats)
library(magrittr)

tibble(d)
view(d)
d

d1 <- d %>%
  filter(freq >= 100) %>%
  mutate(name = fct_reorder(word, freq)) 
d1
View(d1)

ggplot(d1, aes(x = name, y = freq)) +
  geom_col(color = "#01665e", fill = "#1b9e77",
           width=.7, alpha = 0.6) +
  geom_text(aes(label = freq), position = position_dodge(0.9),
            color = "black", size = 2.6, hjust = 1) +
  coord_flip() +
  labs(x = "", y = "Frequência",
       title = "Frequência de termos - Debate presidencial 2016
                Estados Unidos") +
  theme_get() +
  theme(axis.title = element_text(size = 12,
                                  face = "bold"),
        axis.text = element_text(size = 12, 
                                 face = c("bold"))) 


