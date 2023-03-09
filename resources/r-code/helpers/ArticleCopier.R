#
# Kopiowanie wyszukanych plikow po zadanych kwerandach
#
prefPath <- "c:\\Users\\mmironczuk\\Dokumenty lokalne\\article\\article-my\\articles\\art-15-sr-ma\\"
pathFrom <- paste0(prefPath, "resources\\search-results\\2-search-results\\2-3-search-results-without-duplicates\\")
fileIndexPathFrom <- "local-join-path-without-duplicate.txt"
pathTo <- paste0(prefPath, "\\resources\\search-results\\3-collected-articles\\1-2-art-selected-by-20-newsgroups-bydate-db-local\\")

pathLines <- readLines(paste0(pathFrom, fileIndexPathFrom))

for (i in 1:length(pathLines)) {
  tmpPathTo <- paste0(pathTo, paste0(i, " - ", basename(pathLines[i])))
  shell(paste0("copy", " ", '"', pathLines[i], '"', " ", '"', tmpPathTo, '"'))
}

prefPath <- "c:\\Users\\mmironczuk\\Dokumenty lokalne\\article\\article-my\\articles\\art-15-sr-ma\\"
pathFrom <- paste0(prefPath, "resources\\search-results\\3-collected-articles\\1-3-1-art-selected-by-20-newsgroups-bydate-db-local-second\\")
pathTo <- paste0(  "c:\\Users\\mmironczuk\\Downloads\\tmp\\")

pathLines <- list.files(pathFrom)
setwd(pathFrom)

startIndx <- 157
for (i in 1:length(pathLines)) {
  tmpPathTo <- paste0(pathTo, paste0(startIndx, " - ", stringr::str_trim(stringr::str_replace(basename(pathLines[i]), "^\\+", ""))))
  shell(paste0("copy", " ", '"', pathLines[i], '"', " ", '"', tmpPathTo, '"'))
  startIndx <- startIndx + 1
}




