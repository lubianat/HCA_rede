library(data.table)


bla <- fread("data/example_papers.txt")

bla[2]

list_for_item <- strsplit(as.character(bla[2]), " . ")[[1]]

end_info <- list_for_item[length(list_for_item)]

end_info <- 
