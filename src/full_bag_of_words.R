source("src/rentrez_search_from_paper_list_functions.R")

library(data.table)
files <- list.files(path = "./data/papers/", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


final_bag <- data.frame(researcher = 'NULL', word = 'NULL', n = 'NULL')
final_bag <- final_bag[-1,]
done_files <- c()
for (f in files[!files %in% done_files]){
  if (!is.na(f)){
    corpus <- fread(paste0("./data/papers/",f), encoding = 'Latin-1', fill = T)
    title_vector <- extract_titles(corpus)
    mini_tv <- title_vector[1:10]
    if (any(is.na(mini_tv))){
      mini_tv <- title_vector[1:2]
    }
      abs_list <- get_abstracts_from_titles(mini_tv)
      if (length(abs_list)>0){
        bag_of_words <- get_bag_of_words_from_abs_list(abs_list, min_times = 2)
        bag_of_words$researcher <- paste0("#",gsub("(.*).txt", "\\1", f))
        final_bag <- rbind(final_bag,bag_of_words)
      }
      done_files <- c(done_files, f)
    

  }
}


library(tidyr)

filtered_final_bag <- final_bag %>% filter(is.na(as.numeric(word)))

filtered_final_bag<- distinct(filtered_final_bag)

bag_of_words_table <- spread(filtered_final_bag,key = 'word', value = 'n')

unique(filtered_final_bag$researcher)

write.table(bag_of_words_table, "outputs/bag_of_words_10_abs.tsv", sep = '\t', row.names = F)



