library(data.table)


corpus <- fread("data/example_papers.txt")


#' Given a corpus of texts, in the example format
#'  this function will extract all the titles a
#'  @param corpus A corpus pre loaded with fread as the sample above.
#'  @return A character vector with all titles

extract_titles <- function(corpus){
  title_vector <- c()
  corpus <- corpus[nchar(corpus$V1)>20,]
  for (i in corpus[[1]]){
  print(i)
  list_for_item <- strsplit(as.character(i), " . ")[[1]]
  end_info <- list_for_item[length(list_for_item)]
  title <- strsplit(end_info, split = "\\. ")[[1]][1]
  title_vector <- c(title_vector,title)
  }
  
  return(title_vector)
}

title_vector <- extract_titles(corpus)

# Some of them are not exact titles. These will be removed. 
title_vector <- title_vector[nchar(title_vector)>50]



#Now it is time to search rentrez with these titles 


library(rentrez)
library(xml2)

#' get abstracts from title vector
#' @param title_vector A character vector with titles
#' @return A list with the abstracts from each title
get_abstracts_from_titles <- function(title_vector){
  abs_list <- list()
  message("Searching abstracts via rentrez")
  for (i in title_vector){
    print(i)
    id <- rentrez::entrez_search(db="pubmed", term=i)$ids
    
    xml <- rentrez::entrez_fetch(db="pubmed", id = id, rettype = 'xml')
    x <- read_xml(xml)
    
    uh <- xml_find_all(x, ".//Abstract")
    abs_list[[i]] <-     xml_text(uh)
    
    # This chunk will remove all texts that come after the abstract,
    # which sometimes come together for open pubs
    abs_list <- lapply(abs_list, `[[`, 1)
    
  }
  return(abs_list)
  
}


abs_list <- get_abstracts_from_titles(title_vector)


# In the end, we want to have a bag of words with the terms most
# used in the original abstracts.
# The code in this section comes from http://uc-r.github.io/creating-text-features#bag

#' get bag of words from abstract list
#' @param abs_list A list of abstracts
#' @param stemming A boolean to decide if you want stemming or not. Defaults to false. 
#' @return A dataframe with the bag of words for this corpus

library(tidytext)
library(stringr)
library(corpus)
get_bag_of_words_from_abs_list <- function(abs_list, stemming  = FALSE){
  df <- as.data.frame(unlist(abs_list))
  colnames(df) <- "Abstracts"
  
  df$Abstracts <- as.character(df$Abstracts)
  
  
  bag <- df %>%
    select(Abstracts) %>%
    unnest_tokens(word, Abstracts) %>%
    anti_join(stop_words) %>%
    filter(
      !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
      !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
    )
  
  if(!stemming){
    bag_nostem <- bag %>%
      count(word, sort = TRUE) %>%
      filter(n >= 5) %>% # filter for words used 5 or more times
      arrange(desc(n))
    return(bag_nostem)
  } else {
    bag_stem <- bag %>%
      mutate(word = corpus::text_tokens(word, stemmer = "en") %>% unlist()) %>% # add stemming process
      count(word) %>% 
      group_by(word) %>%
      summarize(n = sum(n)) %>%
      arrange(desc(n))
    return(bag_stem)
  }
  
  
}


bag_of_words <- get_bag_of_words_from_abs_list(abs_list)
