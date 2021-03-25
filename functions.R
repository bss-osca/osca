library(wordcloud2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(RCurl)
library(XML)
library(tidyverse)

add_graph_legend <- function(graph, x, y) {
  graph %>%
    add_node(
      label = c("", "Color:", "mandatory", "alternative", "extra", "Shape:", "non-interactive", "interactive"),
      node_data =
        node_aes(x = c(x+2.4, x-0.23, x, x+1, x+2, x+3.5-0.3, x+3.5, x+4.75),
                 y = c(y-0.34, y, rep(y-0.4, 3), y, rep(y-0.4, 2)),
                 fontcolor = c("white", "black", rep("white", 3), "black", "white", "white"),
                 fontsize = 9,
                 shape = c("rect", "none", rep("rect", 3), "none", "rect", "egg"),
                 fillcolor = c(NA, "none", "DarkSeaGreen4", "DarkOrange4", "PeachPuff3", "none", rep("Grey40", 2)),
                 width = c(6, rep(0.8, 4), rep(1,3)),
                 height = c(1, 0, NA, NA, NA, 0, NA, NA),
                 penwidth = c(0.5, 0, 2, 2, 2, 0, 2, 2),
                 tooltip = c("", "",
                             "Mandatory syllabus.",
                             "Alternative syllabus if you prefer another learning style.",
                             "Extra learning if you are interested (not part of syllabus).",
                             "",
                             "Non-interactive learning content (e.g. reading).",
                             "Interactive learning content (tutorial, exercises etc.).")
        ))
}

create_diagram <- function(url, nodes, edges, x_legend = 0, y_legend = 0, fontsize = 12) {
  # gs4_deauth()
  nodes <- read_sheet(url, sheet = nodes, col_types = "iciicccdd")
  edges <- read_sheet(url, sheet = edges, col_types = "iiic")
  edges <-
    create_edge_df(
      from = edges$from,
      to =   edges$to,
      tooltip = edges$tooltip)
  graph <-
    create_graph(nodes_df = nodes, edges_df = edges) %>%
    mutate_node_attrs(
      x = as.numeric(semester) * 6,
      style = "filled",
      fixedsize = FALSE,
      fontsize = fontsize,
      fontcolor = "white",
      penwidth = 2,
      margin = case_when(
        type == "Infobox" ~ 0.1,
        TRUE ~ 0.3),
      fontname = "Helvetica-bold",
      shape = case_when(
        type == "Mandatory" ~ "rect",
        type == "Elective" ~ "rect",
        type == "Infobox" ~ "egg",
        TRUE ~ "oval"),
      emo = case_when(
        type == "reading" ~ "📖 ",
        type == "recap" ~ "📖 ",
        type == "tutorial" ~ "💡 ",
        type == "exercises" ~ "💻 ",
        type == "video" ~ "🎬 ",
        TRUE ~ ""),
      label = str_c(emo, "", label),
      fillcolor = case_when(
        type == "Mandatory" ~ "DarkOrange4",
        type == "Elective" ~ "DarkSeaGreen4",
        type == "Infobox" ~ "PeachPuff3",
        TRUE ~ "#F4A261")
    ) %>%
    mutate_edge_attrs(
      color = "black",
      alpha = 0.15,
      penwidth = 3,
      arrowsize = 2,
      arrowhead = "vee") %>%
    add_graph_legend(x_legend, y_legend)
  return(graph)
}



#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() : Word cloud generator
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++
# x : character string (plain text, web url, txt file path)
# type : specify whether x is a plain text, a web page url or a file path
# lang : the language of the text
# excludeWords : a vector of words to exclude from the text
# textStemming : reduces words to their root form
# colorPalette : the name of color palette taken from RColorBrewer package, 
# or a color name, or a color code
# min.freq : words with frequency below min.freq will not be plotted
# max.words : Maximum number of words to be plotted. least frequent terms dropped
# value returned by the function : a list(tdm, freqTable)
create_wordcloud <- function(x, type=c("url", "text", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette='random-dark',
                             min.freq=3, max.words=50)
{ 
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v) %>% 
    filter(freq >= min.freq) %>% 
    arrange(desc(freq)) %>% 
    slice_head(n = max.words)
  
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  
  # Plot the word cloud
  set.seed(1234)
  # print(
    wcloud <- wordcloud2(
      d,
      minRotation = -pi / 2,
      maxRotation = -pi / 2,
      # rotateRatio = 0.9,
      color = colors,
      # shape = "rectangle"
      size = 1
    )
  # )
  # wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
  #           random.order=FALSE, rot.per=0.35, 
  #           use.r.layout=FALSE, colors=colors)
  
  invisible(
    list(tdm=tdm, 
         freqTable = d,
         wcloud = wcloud))
}


#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don’t want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}