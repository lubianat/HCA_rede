
# files from pmid
source("src/rentrez_search_from_paper_list_functions.R")

library(data.table)
files <- list.files(path = "./data/papers/", pattern = NULL, all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


final_df <- data.frame(researcher = 'NULL', paper = 'NULL', value = 'NULL')
final_df <- final_df[-1,]

for (f in files[26:length(files)]){
  corpus <- fread(paste0("./data/papers/",f), encoding = 'Latin-1', fill = T)
  title_vector <- extract_titles(corpus)
  
  if (length(title_vector) > 0){
    pmid_list <- get_pmids_from_titles(title_vector)
    pmid_list <- lapply(pmid_list, `[[`, 1)
    df <- gather(as.data.frame(pmid_list), key = 'paper')
    df$researcher <- paste0("#",gsub("(.*).txt", "\\1", f))
    final_df <- rbind(final_df, df)
  }

}

write.table(final_df, 'outputs/pmids.tsv', sep ='/t', row.names = F)
