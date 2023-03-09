#
# Script to check is schema question of files is ok.
#

#
# Clear workspace
#
rm(list = ls())

#
# Set language to En
#
Sys.setlocale(category = "LC_ALL", locale = "english")

#
#
#
libraries <- c("readxl", "stringr", "openxlsx", "boot", "nortest", "json64", "fitdistrplus", "MASS", "ggplot2")

if (length(setdiff(libraries, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = T)  
}

#if (length(setdiff(c("Rgraphviz"), rownames(installed.packages()))) > 0) {
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#  
#  BiocManager::install("Rgraphviz") 
#}

#
#
#
sapply(libraries, function(libName) { 
  library(libName, character.only = TRUE)
})

#
# Functions
#
renameFile <- function(fileName) {
  tNewFileNameList <- stringr::str_split(fileName, "\\.")
  fileId <- stringr::str_trim(stringr::str_split(tNewFileNameList[[1]][1], "-")[[1]][1])
  tNewFileName <- paste0(fileId, "-", stringr::str_sub(json64::j_encode(tNewFileNameList[[1]][1], F), 1, 20), ".", tNewFileNameList[[1]][2])
  return(tNewFileName)
}
environment(renameFile) <- new.env(parent = baseenv())

copyFile <- function(xmlsPath, fileName, pasteTo) {
  if (base::Sys.info()['sysname'] == "Windows") {
    xmlsPath <-  base::gsub("/", "\\", xmlsPath, fixed = TRUE)
    pasteTo <-  base::gsub("/", "\\", pasteTo, fixed = TRUE)
    
    base::shell(base::paste0("copy ", base::paste0('"', xmlsPath, fileName, '" ', pasteTo)))
  } else {
    base::file.copy(paste0(xmlsPath, fileName), pasteTo)
  }
}
environment(copyFile) <- new.env(parent = baseenv())

createAggDf <- function(dataFrame) {
  aggDf <- data.frame("question" = "", "answer" = "", "count" = 0)
  
  for (i in 1:base::ncol(dataFrame)) {
    res <- base::table(dataFrame[,i])
    for (j in 1:base::length(res)) {
      aggDf <- base::rbind(aggDf, data.frame("question" = base::colnames(dataFrame)[i], 
                                             "answer" = base::names(res[j]), "count" = res[j]))
    }
  }
  
  aggDf <- aggDf[-c(1), ]
  row.names(aggDf) <- c(1:base::nrow(aggDf))
  
  return(aggDf)
}
environment(createAggDf) <- new.env(parent = baseenv())

ggplotQc <- function(dataDf, ttile, xlab, ylab) {
  ggplot2::ggplot(data = dataDf, ggplot2::aes_string(x = "question", y = "count", fill = "answer")) + 
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), colour = "grey69") +
    ggplot2::geom_text(ggplot2::aes_string(label = "count"), vjust = -0.3, color = "black", position = ggplot2::position_dodge(0.9), size = 5.5) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = ttile, x = xlab, y = ylab) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
environment(ggplotQc) <- new.env(parent = baseenv())

ggplotCf <- function(dataDf, ttile, xlab, ylab) {
  ggplot2::ggplot(data = dataDf, ggplot2::aes_string(x = "count", y = "frequ")) + 
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), fill = "grey69") +
    ggplot2::geom_text(ggplot2::aes_string(label = "frequ"), vjust = -0.3, color = "black", position = ggplot2::position_dodge(0.9), size = 5.5) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = ttile, x = xlab, y = ylab) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
environment(ggplotCf) <- new.env(parent = baseenv())

createIndicatorValDf <- function(modelResultsDf, indicatorName, workIds) {
  indiDf <- modelResultsDf[modelResultsDf$workId %in% workIds, c("workId", "modelName", indicatorName)]
  naIndx <- base::which(base::is.na(indiDf[,indicatorName]))
  if (length(naIndx) != 0) {
    indiDf <- indiDf[-base::which(base::is.na(indiDf[,indicatorName])),]
  }
  tsplit <- base::split(indiDf, indiDf$workId)
  res <- base::lapply(X = tsplit,
                      FUN = function(record) record[which.max(record[,indicatorName]),])

  maxValDf <- base::as.data.frame(res[[1]][c("workId", "modelName", indicatorName)])
  for (i in 2:base::length(res)) {
    maxValDf <- base::rbind(maxValDf, base::as.data.frame(res[[i]][c("workId", "modelName", indicatorName)])) 
  }
  
  return(maxValDf)
}
environment(createIndicatorValDf) <- new.env(parent = baseenv())

simpleStats <- function(modelResultsDf, indicatorName, workIds) {
  aggDf <- base::get("createIndicatorValDf", envir = .GlobalEnv)(modelResultsDf, indicatorName, workIds)
  
  graphics::par(mfrow = c(1,2))
  graphics::hist(aggDf[,indicatorName])
  graphics::boxplot(aggDf[,indicatorName])
  graphics::par(mfrow = c(1,1))
  
  base::print(base::summary(aggDf[,indicatorName]))
  base::print(stats::shapiro.test(aggDf[,indicatorName]))
  base::print(base::dim(aggDf))
}
environment(simpleStats) <- new.env(parent = baseenv())

#
# Bibliography checking and generate simple stats
#
bib1 <- "../biblio//biblio-reviewed-article.bib"
bib2 <- "../biblio/biblio-reviewed-article-prop-form.bib"

linesBib1 <- readLines(bib1)
linesBib2 <- readLines(bib2)

regId <- "^@[:alpha:]{1,}\\{[:alpha:]{1,}[:digit:]{1,4}[:alpha:]{0,1},$"

idsBib1 <- linesBib1[stringr::str_detect(linesBib1, regId)]
idsBib2 <- linesBib2[stringr::str_detect(linesBib2, regId)]

table(stringr::str_to_lower(stringr::str_sort(idsBib1)) == stringr::str_to_lower(stringr::str_sort(idsBib2)))

regTitle = "^ {0,}title {0,}="

titlesBib1 <- linesBib1[stringr::str_detect(linesBib1, regTitle)]
titlesBib2 <- linesBib2[stringr::str_detect(linesBib2, regTitle)]

titlesBib1 <- stringr::str_sub(stringr::str_sort(stringr::str_to_lower(stringr::str_trim(stringr::str_replace_all(titlesBib1, "title|=|\\{|\\}| {2,}", "")))), 1, 50)
titlesBib2 <- stringr::str_sub(stringr::str_sort(stringr::str_to_lower(stringr::str_trim(stringr::str_replace_all(titlesBib2, "title|=|\\{|\\}| {2,}", "")))), 1, 50)

table(titlesBib1 == titlesBib2)

regYear <- "^ {0,}year {0,}="
yearBib1 <- linesBib1[stringr::str_detect(linesBib1, regYear)]
yearBib2 <- linesBib2[stringr::str_detect(linesBib2, regYear)]

regYearDigit <- "[:digit:]{4,4}"
yearsBib1 <- as.numeric(stringr::str_extract(yearBib1, regYearDigit))
yearsBib2 <- as.numeric(stringr::str_extract(yearBib1, regYearDigit))

table(table(yearsBib1) == table(yearsBib2))

dataSetsCountDf <- as.data.frame(table(yearsBib1))
colnames(dataSetsCountDf) <- c("count", "frequ")

ttile <- "Distribution of the number of publications in years"
xlab <- "Year"
ylab <- "Count of publications"
ggplotCf(dataSetsCountDf, ttile, xlab, ylab)

typesPubBib1 <- stringr::str_sort(stringr::str_to_lower(stringr::str_extract(idsBib1, "[:alpha:]{1,}")))
typesPubBib2 <- stringr::str_sort(stringr::str_to_lower(stringr::str_extract(idsBib2, "[:alpha:]{1,}")))

table(typesPubBib1)
table(typesPubBib2)

table(table(typesPubBib1) == table(typesPubBib2))

dataSetsCountDf <- as.data.frame(table(typesPubBib1))
colnames(dataSetsCountDf) <- c("count", "frequ")

ttile <- "Distribution of the number of publications by publication type"
xlab <- "Publication type"
ylab <- "Count of publications"
ggplotCf(dataSetsCountDf, ttile, xlab, ylab)

regJournal = "^ {0,}journal|^ {0,}booktitle"
journalBib2 <- linesBib2[stringr::str_detect(linesBib2, regJournal)]
journalBib2 <- stringr::str_remove(stringr::str_remove(journalBib2, "^ {0,}journal {0,}= \\{|^ {0,}booktitle {0,}= \\{"), "\\}\\,")

dataSetsCountDf <- as.data.frame(table(journalBib2))
colnames(dataSetsCountDf) <- c("count", "frequ")
dataSetsCountDf <- dataSetsCountDf[order(dataSetsCountDf$frequ, decreasing = T), ]

ttile <- "Distribution of the number of publications by publication type"
xlab <- "Publication type"
ylab <- "Count of publications"
ggplotCf(dataSetsCountDf[1:8, ], ttile, xlab, ylab)


#
# Schema chacking
#
xmlsPath <- "../search-results/4-qq-assesments/"

schemaFileName <- "v1 - article-assesment-schema.xlsx"

schemaFileNames <- list.files(xmlsPath)

# Remove from analysis 
noAnalysedArticles <- c("3 \\- Learning algorithms for the classification restricted Boltzmann machine\\.xlsx", 
                        "34 \\- HARAM A Hierarchical ARAM Neural Network for Large\\-Scale Text Classification\\.xlsx",
                        "v0 \\- article\\-assesment\\-schema\\.xlsx",
                        "v1 \\- article\\-assesment\\-schema\\.xlsx")
schemaFileNames <- schemaFileNames[-c(stringr::str_which(schemaFileNames, paste(noAnalysedArticles, collapse = "|")))]

defaultSchema <- readxl::read_excel(paste0(xmlsPath, schemaFileName))

defaultSchemaQuestions <- defaultSchema$Question

tmpFilePath <- "./tmp/" # temporary dir
do.call(file.remove, list(list.files(tmpFilePath, full.names = TRUE))) # Clear tmp dir

for (i in 1:length(schemaFileNames)) {
  tFileName <- schemaFileNames[i]
  message(paste0("Checking schema of question for file: ", tFileName))
  
  # Sometimes a file name is too long so we must transform it to shorter form and after that open
  tNewFileName <- renameFile(tFileName)
  pasteTo <- paste0(tmpFilePath, tNewFileName)
  copyFile(xmlsPath, tFileName, pasteTo)
 
  currentSchema <- readxl::read_excel(pasteTo)
  currentSchemaQuestions <- currentSchema$Question
 
  if (all((defaultSchemaQuestions == currentSchemaQuestions) == TRUE)) {
    message(paste0("Schema is fine"))
  } else {
    stop("Wrong schema")
  }
}

do.call(file.remove, list(list.files(tmpFilePath, full.names = TRUE))) # Clear tmp dir

#
# Generate raw datat to further analysis no. 1
#
colNames <- sapply(stringr::str_split(defaultSchemaQuestions, " "), function(x) x[1])
propColNames <- make.names(c("workId", colNames))

tMat <- matrix(ncol = length(propColNames), nrow = 1)
modelResultsDf <- as.data.frame(tMat, stringsAsFactors = F)
commentsDf <- as.data.frame(tMat, stringsAsFactors = F) 

tmpFilePath <- "./tmp/" # temporary dir
do.call(file.remove, list(list.files(tmpFilePath, full.names = TRUE))) # Clear tmp dir

for (i in 1:length(schemaFileNames)) {
  tFileName <- schemaFileNames[i]
  message(paste0("Checking schema of question for file: ", tFileName))
  
  # Sometimes a file name is too long so we must transform it to shorter form and after that open
  tNewFileName <- renameFile(tFileName)
  pasteTo <- paste0(tmpFilePath, tNewFileName)
  copyFile(xmlsPath, tFileName, pasteTo)
  
  resScheet <- readxl::read_excel(pasteTo, sheet = "Quality-1")

  modelResultsDf <- rbind(modelResultsDf, c(tFileName, resScheet$Assesment))
  commentsDf <- rbind(commentsDf, c(tFileName, resScheet$Comment))
}

modelResultsDf <- modelResultsDf[-c(1), ]
colnames(modelResultsDf) <- propColNames

commentsDf <- commentsDf[-c(1), ]
colnames(commentsDf) <- propColNames

# Remove unnesesary columns
indxs <- stringr::str_which(modelResultsDf[1,], "^-$")
modelResultsDf <- modelResultsDf[,-c(indxs)]

do.call(file.remove, list(list.files(tmpFilePath, full.names = TRUE))) # Clear tmp dir

str(modelResultsDf)
head(modelResultsDf)

#
# Taksonomia - Zgrubny podzial ale wystarczajacy aby oipisac zjawisko bez utraty ... Ba;nced unbalanced
#
worksIndxs <- list()

# Czy mamy zbalasnoswany niezbalanoswany dtga set jak bedziemy pokazywac example w calosci czy tylko torche czy bedzie kombinowac wyjsci aklasyfikacji etc.
worksIndxs$grLearninMethWorkIds <- c("129 - Multi-co-training for document classification using various document representations TF-IDF LDA and Doc2Vec.xlsx", 
                                     "29 - Manifold Adaptive Experimental Design for Text Categorization.xlsx",
                                     "53 - Text classification method based on self-training and LDA topic models.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grLearninMethWorkIds] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grLearninMethWorkIds),]$workId) == TRUE)


# Ogolnie o reprezentacji Finaly the method create a ambeded in lower space document representation so it may be consider in Dim reduction by feature projection, feature costruction aspects.
worksIndxs$grDocRep <- c("53 - Searching for discriminative words in multidimensional continuous feature space.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grDocRep] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grDocRep),]$workId) == TRUE)


worksIndxs$grWeightingWorkIds <- c("109 - A semantic term weighting scheme for text categorization.xlsx", 
                                   "236 - An improved term weighting scheme for text classification.xlsx",  
                                   "6 - Weighted Document Frequency for feature selection in text classification.xlsx", 
                                   "63 - Turning from TF-IDF to TF-IGM for term weighting in text.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grWeightingWorkIds] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grWeightingWorkIds),]$workId) == TRUE)


# Dim reduction by feature selection - zachowanie cech tylko ich selekcja bez zmiany przestrzeni
worksIndxs$grFsWorkIds <- c("10 - Self-Tuned Descriptive Document Clustering Using a Predictive Network.xlsx",
                            "14 - A Bayesian Classification Approach Using Class-Specific  Features for Text Categorization.xlsx", 
                            "18 - Toward Optimal Feature Selection in Naive bayes for text categorization.xlsx", 
                            "19 - Text Categorization Using Weighted Hyper Rectangular Keyword Extraction.xlsx", 
                            "218 - Relative discrimination criterion - A novel feature ranking method.xlsx", 
                            "238 - Extending the Single Words-Based Document Model.xlsx", 
                            "238 - Extending the Single Words-Based Document Model.xlsx", 
                            "28 - TOFA Trace Oriented Feature Analysis in Text Categorization.xlsx", 
                            "33 - Importance weighted feature selection strategy for text classification.xlsx", 
                            "48 - A discriminative and semantic feature selection method.xlsx", 
                            "61 - Feature subset selection using naive Bayes for text classification.xlsx", 
                            "63 - RFBoost An improve d multi-lab el boosting algorithm and its application to text categorisation.xlsx", 
                            "66 - Improved Document Feature Selection with Categorical Parameter for Text Classification.xlsx", 
                            "88 - Feature ranking for enhancing boosting-based multi-label text categorization.xlsx", 
                            "93 - Feature selection based on a normalized difference measure for text classification.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grFsWorkIds] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grFsWorkIds),]$workId) == TRUE)


# Dim reduction by feature projection, feature costruction, document representation i other feature space Przeksztalcenie czegos w cos innego
worksIndxs$grFetProjWorkIds <- c("202 - KATE K-Competitive Autoencoder for Text.xlsx", 
                                 "204 - Learning document representations using subspace multinomial model.xlsx", 
                                 "219 - Replicated Softmax an Undirected Topic Model.xlsx", 
                                 "22 - Learning distributed word representation with multi-contextual mixed embedding.xlsx", 
                                 "23 - Extending Embedding Representation by Incorporating Latent Relations.xlsx", 
                                 "28 - TOFA Trace Oriented Feature Analysis in Text Categorization.xlsx", 
                                 "29 - Manifold Adaptive Experimental Design for Text Categorization.xlsx", 
                                 "44 - Probabilistic Clustering and Classification for Textual Data An Online and Incremental Approach.xlsx", 
                                 "49 - Fast text categorization using concise semantic analysis.xlsx", 
                                 "5 - A Bidirectional Hierarchical Skip-Gram model for text topic embedding.xlsx", 
                                 "62 - Bag-of-Concepts representation for document classification based on automatic knowledge acquisition from probabilistic knowledge base.xlsx", 
                                 "73 - A new regularized restricted Boltzmann machine based on class preserving.xlsx")                                                             

all((schemaFileNames[schemaFileNames %in% worksIndxs$grFetProjWorkIds] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grFetProjWorkIds),]$workId) == TRUE)


worksIndxs$grClassMet <- c("1 - Classification using discriminative restricted Boltzmann machines.xlsx", 
                           "100 - Enhanced sparse representation classifier for text classification.xlsx", 
                           "121 - Improving Multiclass Text Classification with Error-Correcting Output Coding and Sub-class Partitions.xlsx", 
                           "13 - Using the Tsetlin Machine to Learn Human-Interpretables.xlsx", 
                           "138 - From Word Embeddings To Document Distances.xlsx", 
                           "140 - Overfitting Reduction of Text Classification Based on AdaBELM.xlsx", 
                           "17 - Probabilistic reasoning on background net An application to text categorization.xlsx", 
                           "199 - Investigating Unsupervised Learning.xlsx", 
                           "200 - Joint Verification-Identification in end-to-end Multi-Scale CNN Framework for Topic Identification.xlsx", 
                           "2005 - Text Classification with Kernels on the Multinom.xlsx", 
                           "212 - On The Value of Leave-One-Out.xlsx", 
                           "224 - Sparse Representations for Text Categorization.xlsx", 
                           "228 - Text Classification Using Combined Sparse Representation Classifiers and Support.xlsx", 
                           "229 - Text Classification using Hierarchical Sparse.xlsx", 
                           "235 - An Adaptive k-Nearest Neighbor Text Categorization Strategy.xlsx", 
                           "239 - Large-Scale Bayesian Logistic Regression for Text Categorization.xlsx", 
                           "32 - A Text Categorization Method Based on Local Document Frequency.xlsx", 
                           "54 - On the strength of hyperclique patterns for text categorization.xlsx", 
                           "55 - Improving scalability of ART neural networks.xlsx", 
                           "56 - Minimizer of the Reconstruction Error for multi-class document.xlsx", 
                           "58 - Regularized margin-based conditional log-likelihood loss for prototype learning.xlsx", 
                           "59 - Towards a Quantum-Inspired Binary Classifier.xlsx", 
                           "61 - Feature subset selection using naive Bayes for text classification.xlsx", 
                           "63 - RFBoost An improve d multi-lab el boosting algorithm and its application to text categorisation.xlsx", 
                           "64 - Bag-of-Embeddings for Text Classification.xlsx", 
                           "80 - CenKNN a scalable and effective text classifier.xlsx", 
                           "88 - Feature ranking for enhancing boosting-based multi-label text categorization.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grClassMet] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grClassMet),]$workId) == TRUE)


worksIndxs$grBenchmarkWorkIds <- c("50 - On strategies for imbalanced text classification using SVM A comparative study.xlsx",  
                                   "65 - Empirical Study to Evaluate the Performance of Classification Algorithms on Public Datasets.xlsx", 
                                   "77 -  A Comparative Study on Term Weighting Schemes for Text Classification.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grBenchmarkWorkIds] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grBenchmarkWorkIds),]$workId) == TRUE)


worksIndxs$grEvalWorkIds <- c("152 - What is relevant in a text document.xlsx")

all((schemaFileNames[schemaFileNames %in% worksIndxs$grEvalWorkIds] ==
       modelResultsDf[which(modelResultsDf$workId %in% worksIndxs$grEvalWorkIds),]$workId) == TRUE)

worksIndxsVec <- unique(unlist(worksIndxs))
if (length(worksIndxsVec) != 60) {
  stop("Wrong length")  
} else {
  message("A length is proper.")
}

#
#
#
generalDf <- modelResultsDf[,c("Q1", "Q2", "Q3", "Q4", "Q5")]
generalAggDf <- createAggDf(generalDf)

ttile <- "Distribution of answers to questions about general impresion of articles"
xlab <- "Question number"
ylab <- "Count/percentage of articles"
ggplotQc(generalAggDf, ttile, xlab, ylab)

# Generating of csv tables to article
cnames <- c("Reference", "Work goal", "Technical and algorythimical aspact of the work",
            "Findings/recommendations of the research", "Highlighted challenges or open problems")
tres <- commentsDf[which(commentsDf$workId %in% worksIndxs$grLearninMethWorkIds),c("workId", "Q1", "Q2", "Q3", "Q4")]
colnames(tres) <- cnames
write.xlsx(tres, "../tables/table-grLearninMeth.xlsx")

#
#
#
methRepDf <- modelResultsDf[,c("Q6.1", "Q6.2", "Q6.3", "Q6.4", "Q6.5", "Q6.6", "Q6.7")]
methRepAggDf <- createAggDf(methRepDf)

ttile <- "Distribution of answers to questions about reproducibility of methods/algorithms"
xlab <- "Question number"
ylab <- "Count/percentage of articles"
ggplotQc(methRepAggDf, ttile, xlab, ylab)

#
# Datasets analysis Assesment
# Datasets analysis Commensts - Remove datasets other then tm and standarized datasets names
#
datasetRepDf <- modelResultsDf[,c("Q7.1.1", "Q7.1.2", "Q7.1.3", "Q7.1.4", "Q7.1.5", "Q7.1.6", "Q7.1.7", "Q7.1.8")]
datasetRepAggDf <- createAggDf(datasetRepDf)

ttile <- "Distribution of answers to questions about reproducibility of datasets"
xlab <- "Question number"
ylab <- "Count/percentage of articles"
ggplotQc(datasetRepAggDf, ttile, xlab, ylab)

commentsDf$Q7.1.8 # Datasets

dataSetsList <- sapply(commentsDf$Q7.1.8, function(rec) {
  tsplit <- stringr::str_split(rec, "\r\n|\r|\n")
  
  if (length(tsplit) > 1) {
    stop("Wrong list length")
  }
  
  stringr::str_trim(stringr::str_replace_all(tsplit[[1]], "^-\\s{1,1}", ""))
})

unifiedDsNames <- data.frame("from" = "", "to" = "", stringsAsFactors = F)

unifiedDsNames <- rbind(unifiedDsNames, c("^20 Newsgroup$", NA))

unifiedDsNames <- rbind(unifiedDsNames, c("^Other 29 different datasets.$", NA))

unifiedDsNames <- rbind(unifiedDsNames, c("^MNIST \\(no text/document data set\\)$", NA))
unifiedDsNames <- rbind(unifiedDsNames, c("^MNIST$", NA))

unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578 \\- the authors used the seven most frequent classes in the ModApte split version of Reuters\\-21578 which provide a train and test split of data\\)$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-50\\-50$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^reuters$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters R8$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^ModApte subset of the Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-Small$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-Large$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters R8$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters R52$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters 21578 Apte 90 Cat$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters\\-21578$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuter\\-21,578$", "Reuters-21578"))

unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters Corpus Volume I \\(RCV1\\-v2\\)$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters \\(Reuters RCV1\\-v2\\)$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^RCV1\\-v2, a test categorization test collection of 804,414 newswire stories based on data released by Reuters$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Reuters Corpus Volume 1 \\(RCV1\\)$", "Reuters-21578"))
unifiedDsNames <- rbind(unifiedDsNames, c("^RCV1-v2$", "Reuters"))
unifiedDsNames <- rbind(unifiedDsNames, c("^RCV1-v2$", "Reuters"))

unifiedDsNames <- rbind(unifiedDsNames, c("^Ingredient lists from Yummly's recipe dataset$", "Recipe"))
unifiedDsNames <- rbind(unifiedDsNames, c("^recipe$", "Recipe"))

unifiedDsNames <- rbind(unifiedDsNames, c("^Sector$", "Sector"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Sector$", "Sector"))
unifiedDsNames <- rbind(unifiedDsNames, c("^7 Sectors$", "Sector"))

unifiedDsNames <- rbind(unifiedDsNames, c("^WIPO-alpha$", "WIPO"))
unifiedDsNames <- rbind(unifiedDsNames, c("^WIPO-de$", "WIPO"))

unifiedDsNames <- rbind(unifiedDsNames, c("^Tancorp$", "Tancorp"))
unifiedDsNames <- rbind(unifiedDsNames, c("^TanCorp$", "Tancorp"))
unifiedDsNames <- rbind(unifiedDsNames, c("^TanCorp$", "Tancorp"))

unifiedDsNames <- rbind(unifiedDsNames, c("^webKB$", "WebKB"))
unifiedDsNames <- rbind(unifiedDsNames, c("^WebKB$", "WebKB"))
unifiedDsNames <- rbind(unifiedDsNames, c("^WebKB Universities$", "WebKB"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Webkb$", "WebKB"))

unifiedDsNames <- rbind(unifiedDsNames, c("^Ohsumed$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^ohsumed$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Ohsumed \\(two types oh10 and oh23\\)$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^OHSUMED$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Ohsumed first 20000 docs$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^OHSUMED$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^OHSUMED$", "Ohsumed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Oshumed$", "Ohsumed"))

unifiedDsNames <- rbind(unifiedDsNames, c("^bbcsport$", "BBC"))
unifiedDsNames <- rbind(unifiedDsNames, c("^BBC$", "BBC"))
unifiedDsNames <- rbind(unifiedDsNames, c("^BBCSport$", "BBC"))

unifiedDsNames <- rbind(unifiedDsNames, c("^classic$", "Classic"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Classic$", "Classic"))

unifiedDsNames <- rbind(unifiedDsNames, c("^NSF research award abstracts 1990\\-2003 data set$", "NSF research award abstracts 1990-2003 data set"))
unifiedDsNames <- rbind(unifiedDsNames, c("^News articles provided by Antonio Gulli$", "News articles provided by Antonio Gulli"))
unifiedDsNames <- rbind(unifiedDsNames, c("^SemEval$", "SemEval"))
unifiedDsNames <- rbind(unifiedDsNames, c("^IMDb$", "IMDb"))
unifiedDsNames <- rbind(unifiedDsNames, c("^twitter$", "Twitter"))
unifiedDsNames <- rbind(unifiedDsNames, c("^amazon$", "Amazon"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Biomed$", "Biomed"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Topic detection$", "Topic detection"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Tracking \\(TDT2\\)$", "Tracking (TDT2)"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Fisher$", "Fisher"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Wiki10\\+$", "Wiki10+"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Movie review data \\(MRD\\) for regression task$", "Movie review data (MRD) for regression task"))
unifiedDsNames <- rbind(unifiedDsNames, c("^NIPS proceedings papers$", "NIPS proceedings papers"))
unifiedDsNames <- rbind(unifiedDsNames, c("^A clinical dataset with authentic EHRs from a hospital$", "A clinical dataset with authentic EHRs from a hospital"))
unifiedDsNames <- rbind(unifiedDsNames, c("^The Sogou Lab Data$", "The Sogou Lab Data"))
unifiedDsNames <- rbind(unifiedDsNames, c("^PKU WEB Page Dataset$", "PKU WEB Page Dataset"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Open Directory Project$", "Open Directory Project"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Nlpcc2014$", "Nlpcc2014"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Cade$", "Cade"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Google snippets$", "Google snippets"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Ohscal$", "Ohscal"))
unifiedDsNames <- rbind(unifiedDsNames, c("^EUR\\-Lex\\-EUROVOC \\(Eur-Lex\\)$", "EUR-Lex-EUROVOC (Eur-Lex)"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Nova$", "Nova"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Cnae\\-9$", "Cnae-9"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Farm Ads$", "Farm Ads"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Dexter$", "Dexter"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Mayor's public hotline \\(MPH\\)$", "Mayor's public hotline (MPH)"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Yahoo! Answers Topic \\(Yahoo\\)$", "Yahoo! Answers Topic (Yahoo)"))
unifiedDsNames <- rbind(unifiedDsNames, c("^The Fudan University text classification corpus$", "The Fudan University text classification corpus"))
unifiedDsNames <- rbind(unifiedDsNames, c("^The DMOZ dataset based on the ODP \\(Open Directory Project\\) web directory data$", "The DMOZ dataset based on the ODP (Open Directory Project) web directory data"))
unifiedDsNames <- rbind(unifiedDsNames, c("^Medical$", "Medical"))
unifiedDsNames <- rbind(unifiedDsNames, c("^TMC2007$", "TMC2007"))
unifiedDsNames <- rbind(unifiedDsNames, c("^WAP \\(downloaded from Karypis Lab, University of Minnesota, http://glaros.dtc.umn.edu/gkhome/cluto/cluto/download\\)$", "WAP"))
unifiedDsNames <- rbind(unifiedDsNames, c("^K1a \\(downloaded from Karypis Lab, University of Minnesota, http://glaros.dtc.umn.edu/gkhome/cluto/cluto/download\\)$", "K1a"))
unifiedDsNames <- rbind(unifiedDsNames, c("^K1b \\(downloaded from Karypis Lab, University of Minnesota, http://glaros.dtc.umn.edu/gkhome/cluto/cluto/download\\)$", "K1b"))
unifiedDsNames <- rbind(unifiedDsNames, c("^re0 \\(downloaded from Karypis Lab, University of Minnesota, http://glaros.dtc.umn.edu/gkhome/cluto/cluto/download\\)$", "re0"))
unifiedDsNames <- rbind(unifiedDsNames, c("^re1 \\(downloaded from Karypis Lab, University of Minnesota, http://glaros.dtc.umn.edu/gkhome/cluto/cluto/download\\)$", "re1"))

unifiedDsNames <- unifiedDsNames[-c(1), ]

for (i in 1:length(dataSetsList)) {
  for (j in 1:nrow(unifiedDsNames)) {
    dataSetsList[[i]] <- str_replace(dataSetsList[[i]], unifiedDsNames[j, "from"], unifiedDsNames[j, "to"])
    dataSetsList[[i]]  <- unique(dataSetsList[[i]])
  }  
}

dataSetsCountVec <- sapply(dataSetsList, function(rec) {
  count <- 0
  
  if (is.na(rec[[1]])) {
    count <- 1
  } else {
    count <- length(rec) + 1
  }
  
  count
})

dataSetsCountDf <- as.data.frame(table(dataSetsCountVec))
colnames(dataSetsCountDf) <- c("count", "frequ")

ttile <- "Distribution of the number of datasets in publications"
xlab <- "Number of datasets"
ylab <- "Count of publications"
ggplotCf(dataSetsCountDf, ttile, xlab, ylab)

dataSetsVec <- unlist(dataSetsList)
names(dataSetsVec) <- NULL
dataSetsVec <- dataSetsVec[!is.na(dataSetsVec)]
dataSetsVec <- dataSetsVec[!dataSetsVec == ""]
datSetRankDf <- as.data.frame(sort(table(dataSetsVec), decreasing = T))

datSetRankDf

#
# Evaluation analysis Assesment
# Evaluation analysis Commensts
#
datasetRepDf <- modelResultsDf[,c("Q7.2.1", "Q7.2.2", "Q7.2.3", "Q7.2.4", "Q7.2.5", "Q7.2.6", "Q7.2.7")]
evalRepAggDf <- createAggDf(datasetRepDf)

ttile <- "Distribution of answers to questions about reproducibility of evaluation procedure/study design and results"
xlab <- "Question number"
ylab <- "Count/percentage of articles"
ggplotQc(evalRepAggDf, ttile, xlab, ylab)

commentsDf$Q7.2.1 # Indicators

indicatorsList <- sapply(commentsDf$Q7.2.1, function(rec) {
  tsplit <- stringr::str_split(rec, "\r\n|\r|\n")
  
  if (length(tsplit) > 1) {
    stop("Wrong list length")
  }
  
  stringr::str_trim(stringr::str_replace_all(tsplit[[1]], "^-\\s{1,1}", ""))
})

unifiedIndiNames <- data.frame("from" = "", "to" = "", stringsAsFactors = F)

unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no citations or explanation of indicators\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no full of explanation of indicators\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Accuracy for comparison with other works\\~\\\\citep\\{Sainath2010,Sharma2016\\}$", "Accuracy"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no equations, but we have citation\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no citations or explanation of Accuracy\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Accuracy for comparison with other works$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no citations or explanation of Error\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Results reported for each one-vs-all strategy for each category\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no full of explanation of the indicator\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no citations\\.$", NA))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^There is no citations or fully explanation of indicators\\.$", NA))

unifiedIndiNames <- rbind(unifiedIndiNames, c("^Micro-F1 score$", "Micro F1 score"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Macro-F1 score$", "Macro F1 score"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Binary classifiers are built for each individual class and a global F1 measure is obtained by averaging the F1 measure of each class weighted by the class prior\\.$", "Macro F1 score"))	

unifiedIndiNames <- rbind(unifiedIndiNames, c("^Classification time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Computation time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Computational cost$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^CPU runtime$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Documents conversion time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Execution time \\(trin and test time\\)$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Feature selection time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Test time\\(s\\)$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^The computational learning time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Time per Sample \\(TTpS\\)$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Training time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Training Time$", "Time"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Training time \\(CPU seconds\\)$", "Time"))

unifiedIndiNames <- rbind(unifiedIndiNames, c("^F1 score", "Unknow F score type"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^F1 score \\(the authors do not justify if it macro or micro type\\)", "Unknow F score type"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^The authors do not note which F1 score was used, i\\.e\\. Macro F1 score or Micro F1 score\\.$", "Unknow F score type"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^F\\-score but we do not know what type, i\\.e\\. micro\\-averaging or macro\\-averaging\\.$", "Unknow F score type"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^F\\-score$", "Unknow F score type"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Unknow F score type \\(the authors do not justify if it macro or micro type\\)$", "Unknow F score type"))

unifiedIndiNames <- rbind(unifiedIndiNames, c("^Precision$", "Unknow Precision type"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Recall$", "Unknow Recall type"))

unifiedIndiNames <- rbind(unifiedIndiNames, c("^Break even point \\(BEP\\)$", "Break even point (BEP)"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Break\\-even point of recall and precision$", "Break even point (BEP)"))

unifiedIndiNames <- rbind(unifiedIndiNames, c("^Area under the Receiver Operating Characteristic \\(ROC\\) curve \\(AUC\\)$", "Receiver Operating Characteristic (ROC)"))
unifiedIndiNames <- rbind(unifiedIndiNames, c("^Receiver Operating Characteristic \\(ROC\\)\\-curve$", "Receiver Operating Characteristic (ROC)"))

unifiedIndiNames <- unifiedIndiNames[-c(1), ]

tworks <- c()
for (i in 1:length(indicatorsList)) {
  for (j in 1:nrow(unifiedIndiNames)) {
    indicatorsList[[i]] <- str_replace(indicatorsList[[i]], unifiedIndiNames[j, "from"], unifiedIndiNames[j, "to"])
    indicatorsList[[i]]  <- unique(indicatorsList[[i]])
    indicatorsList[[i]] <- indicatorsList[[i]][!is.na(indicatorsList[[i]])]
    if("Macro F1 score" %in% indicatorsList[[i]]) tworks <- append(tworks,commentsDf$workId[i])
  }  
}

setdiff(unique(modelResultsDf$workId[!is.na(modelResultsDf$Macro.F1.score)]), unique(tworks))
setdiff(unique(tworks), unique(modelResultsDf$workId[!is.na(modelResultsDf$Macro.F1.score)]))

indicatorsCountVec <- sapply(indicatorsList, function(rec) {
  count <- 0
  
  if (is.na(rec[[1]])) {
    count <- 0
  } else {
    count <- length(rec)
  }
  
  count
})

indicatorsCountDf <- as.data.frame(table(indicatorsCountVec))
colnames(indicatorsCountDf) <- c("count", "frequ")

ttile <- "Distribution of the number of different indicators in publications"
xlab <- "Number of indicators"
ylab <- "Count of publications"
ggplotCf(indicatorsCountDf, ttile, xlab, ylab)

indicatorsCountVec <- unlist(indicatorsList)
names(indicatorsCountVec) <- NULL
indicatorsCountVec <- indicatorsCountVec[!is.na(indicatorsCountVec)]
indicatorsCountVec <- indicatorsCountVec[!indicatorsCountVec == ""]
indicatorsRankDf <- as.data.frame(sort(table(indicatorsCountVec), decreasing = T))

indicatorsRankDf


# Ranking of different methods
commentsDf$Q5 # Methods
  
#
# Generate raw datat to further analysis no. 2
#
colNames <- c("workId", "modelName", "Macro-averaged Precision", "Macro-averaged Recall", "Macro F1 score", "Micro-averaged Precision", 
              "Micro-averaged Recall", "Micro F1 score", "Accuracy", "Error")
propColNames <- make.names(colNames)

modelResultsDf <- data.frame("V1" = "", "V2" = NA, "V3" = NA, "V4" = NA,
                             "V5" = "", "V6" = NA, "V7" = NA, "V8" = NA, "V9" = NA, "V10" = NA)
modelResultsDf <- colnames(propColNames)

tmpFilePath <- "./tmp/" # temporary dir
do.call(file.remove, list(list.files(tmpFilePath, full.names = TRUE))) # Clear tmp dir

for (i in 1:length(schemaFileNames)) {
  tFileName <- schemaFileNames[i]
  message(paste0("Checking schema of question for file: ", tFileName))
  
  # Sometimes a file name is too long so we must transform it to shorter form and after that open
  tNewFileName <- renameFile(tFileName)
  pasteTo <- paste0(tmpFilePath, tNewFileName)
  copyFile(xmlsPath, tFileName, pasteTo)
  
  resScheet <- readxl::read_excel(pasteTo, sheet = "Quantity-1")
  
  ttDf <- t(resScheet[1:9,])
  ttDf <- cbind(data.frame("workId" = NA), ttDf)
  ttNames <- ttDf[1,]
  ttNames[1] <- "workId"
  ttNames[2] <- "modelName"
  ttNames <- as.character.numeric_version(ttNames[1,])
  
  if (all((ttNames == colNames) == TRUE)) {
    message(paste0("Schema is fine"))
    
    ttDf <- as.data.frame(ttDf[, ])
    ttDf <- ttDf[-c(1), ]
    
    ttDf[,1] <- tFileName
    ttDf[,2] <- as.character(ttDf[,2])
    ttDf[,3:10] <- apply(ttDf[,3:10], 2, function(x) as.numeric(x))
    ttDf <- cbind(ttDf, "Subgroup" = stringr::str_split(commentsDf$Q2[i], "\r\n")[[1]][1])
    colnames(ttDf) <- propColNames
    str(ttDf)
    
    modelResultsDf <- rbind(modelResultsDf, ttDf)
    
  } else {
    stop("Wrong schema")
  }
}

do.call(file.remove, list(list.files(tmpFilePath, full.names = TRUE))) # Clear tmp dir

rownames(modelResultsDf) <- 1:nrow(modelResultsDf)
modelResultsDf <- modelResultsDf[-c(1), ]

str(modelResultsDf)
head(indicatorsRankDf)

simpleStats(modelResultsDf, "Accuracy", unique(modelResultsDf$workId))
simpleStats(modelResultsDf, "Macro.F1.score", unique(modelResultsDf$workId))
simpleStats(modelResultsDf, "Micro.F1.score", unique(modelResultsDf$workId))

tmrDf <- subset(modelResultsDf, Accuracy > 70)
simpleStats(tmrDf, "Accuracy", unique(tmrDf$workId))
tmrDf <- subset(modelResultsDf, Macro.F1.score >= 65)
simpleStats(tmrDf, "Macro.F1.score", unique(tmrDf$workId))
simpleStats(tmrDf, "Micro.F1.score", unique(tmrDf$workId))


# Split by groups
simpleStats(modelResultsDf, "Accuracy", worksIndxs$grClassMet)
simpleStats(modelResultsDf, "Macro.F1.score", worksIndxs$grClassMet)
simpleStats(modelResultsDf, "Micro.F1.score", worksIndxs$grClassMet)

worksIndxs$grClassMet <- c(1, 3, 5, 7,  10, 11, 15, 16, 19, 23, 24, 25, 27, 30, 33, 42, 43, 44, 45, 46, 48, 50, 52, 57, 58)
schemaFileNames[worksIndxs$grClassMet]
# Outlayers from grClassMet
worksIndxs$grDistBasedClassMet <- c(8, 14)
schemaFileNames[worksIndxs$grDistBasedClassMet]

simpleStats(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grFsWorkIds])
simpleStats(modelResultsDf, "Macro.F1.score", schemaFileNames[worksIndxs$grFsWorkIds])
simpleStats(modelResultsDf, "Micro.F1.score", schemaFileNames[worksIndxs$grFsWorkIds])

simpleStats(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grFetProjWorkIds])
simpleStats(modelResultsDf, "Macro.F1.score", schemaFileNames[worksIndxs$grFetProjWorkIds])
simpleStats(modelResultsDf, "Micro.F1.score", schemaFileNames[worksIndxs$grFetProjWorkIds])

#simpleStats(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grWeightingWorkIds])
simpleStats(modelResultsDf, "Macro.F1.score", schemaFileNames[worksIndxs$grWeightingWorkIds])
simpleStats(modelResultsDf, "Micro.F1.score", schemaFileNames[worksIndxs$grWeightingWorkIds])

t.test(createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grClassMet])[,"Accuracy"], 
       createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grFsWorkIds])[,"Accuracy"])

t.test(createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grClassMet])[,"Accuracy"], 
       createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grFetProjWorkIds])[,"Accuracy"])

t.test(createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grFsWorkIds])[,"Accuracy"], 
       createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grFetProjWorkIds])[,"Accuracy"])

wilcox.test(createIndicatorValDf(modelResultsDf, "Accuracy", c(schemaFileNames[worksIndxs$grFetProjWorkIds], schemaFileNames[worksIndxs$grFetProjWorkIds] ))[,"Accuracy"])

t.test(createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames[worksIndxs$grClassMet])[,"Accuracy"], 
       createIndicatorValDf(modelResultsDf, "Accuracy", c(schemaFileNames[worksIndxs$grFetProjWorkIds], schemaFileNames[worksIndxs$grFetProjWorkIds] ))[,"Accuracy"])


groups <- c("grClassMet", "grFsWorkIds", "grFetProjWorkIds", "grWeightingWorkIds")
grIndx <- 2:3
tfns <- schemaFileNames[unique(unlist(worksIndxs[groups[grIndx]]))]

valuesVec <- createIndicatorValDf(modelResultsDf, "Accuracy", tfns)[,"Accuracy"]
valuesVec <- createIndicatorValDf(modelResultsDf, "Macro.F1.score", tfns)[,"Macro.F1.score"]
valuesVec <- createIndicatorValDf(modelResultsDf, "Micro.F1.score", tfns)[,"Micro.F1.score"]

valuesVec <- createIndicatorValDf(modelResultsDf, "Accuracy", schemaFileNames)[,"Accuracy"]

fitdistrplus::plotdist(valuesVec, histo = TRUE, demp = TRUE)
fitdistrplus::descdist(valuesVec, discrete = FALSE, boot = 500)

fit_n  <- fitdist(valuesVec, "norm")
summary(fit_n)

par(mfrow = c(2,2))
plot.legend <- c("norm")
fitdistrplus::denscomp(list(fit_n), legendtext = plot.legend)
fitdistrplus::cdfcomp(list(fit_n), legendtext = plot.legend)
fitdistrplus::qqcomp(list(fit_n), legendtext = plot.legend)
fitdistrplus::ppcomp(list(fit_n), legendtext = plot.legend)

rsq <- function(data, indices) {
  d <- data[indices]
  return(mean(d, na.rm = T))
} 

results <- boot::boot(data = valuesVec, statistic = rsq, R = 900000)
plot(results)
nortest::ad.test(results$t[sample(1:100000, 1000),1])

# get 95% confidence interval
boot::boot.ci(results, type = "bca")










#
# Old analysys to delete
#


modelAccDf <- subset(modelResultsDf, !is.na(Accuracy))
accMetaDf <- data.frame("Author" = modelAccDf$modelName, "Me" = modelAccDf$Accuracy/100,	
                        "Se" = rep("", nrow(modelAccDf)),	"Mc" = rep(1/20, nrow(modelAccDf)),	
                        "Sc" = rep(0.0025, nrow(modelAccDf)),	"Ne" = 7528,	"Nc" = 7528,	
                        "Subgroup" = rep(1, nrow(modelAccDf)))

modelFscoreMacroDf <- subset(modelResultsDf, !is.na(Macro.F1.score))
fscoreMacroMetaDf <- data.frame("Author" = modelFscoreMacroDf$modelName, "Me" = modelFscoreMacroDf$Macro.F1.score/100,	
                        "Se" = rep("", nrow(modelFscoreMacroDf)),	"Mc" = rep(1/20, nrow(modelFscoreMacroDf)),	
                        "Sc" = rep(0.0025, nrow(modelFscoreMacroDf)), "Ne" = 7528,	"Nc" = 7528,	
                        "Subgroup" = rep(1, nrow(modelFscoreMacroDf)))

modelFscoreMicroDf <- subset(modelResultsDf, !is.na(Micro.F1.score))
fscoreMicroMetaDf <- data.frame("Author" = modelFscoreMicroDf$modelName, "Me" = modelFscoreMicroDf$Micro.F1.score/100,	
                                "Se" = rep("", nrow(modelFscoreMicroDf)),	"Mc" = rep(1/20, nrow(modelFscoreMicroDf)),	
                                "Sc" = rep(0.0025, nrow(modelFscoreMicroDf)), "Ne" = 7528,	"Nc" = 7528,	
                                "Subgroup" = rep(1, nrow(modelFscoreMicroDf)))

#
# Save oryginal all data
#
xmlsPath <- "../search-results/5-quantitative-analysis/"

schemaFileName <- "accuracyMetaData.xlsx"
openxlsx::write.xlsx(accMetaDf, paste0(xmlsPath, schemaFileName))

schemaFileName <- "fscoreMacroMetaData.xlsx"
openxlsx::write.xlsx(fscoreMacroMetaDf, paste0(xmlsPath, schemaFileName))

schemaFileName <- "fscoreMicroMetaData.xlsx"
openxlsx::write.xlsx(fscoreMicroMetaDf, paste0(xmlsPath, schemaFileName))

#
# Simple exploratory statistics
#
plot(modelResultsDf$Accuracy)
hist(modelResultsDf$Accuracy[!is.na(modelResultsDf$Accuracy)])
boxplot(modelResultsDf$Accuracy[!is.na(modelResultsDf$Accuracy)])
shapiro.test(modelResultsDf$Accuracy)

mrAccDf <- modelResultsDf[-which(is.na(modelResultsDf$Accuracy)),]

tsplit <- split(mrAccDf, mrAccDf$workId)

res <- lapply( X = split(mrAccDf, mrAccDf$workId),
       FUN = function(record) record[which.max(record$Accuracy),])

maxValDf <- as.data.frame(res[[1]][c("workId", "modelName", "Accuracy")])
for (i in 2:length(res)) {
  maxValDf <- rbind(maxValDf, as.data.frame(res[[i]][c("workId", "modelName", "Accuracy")])) 
}

hist(maxValDf$Accuracy)
boxplot(maxValDf$Accuracy)
shapiro.test(maxValDf$Accuracy)

hist(maxValDf$Accuracy[-which.min(maxValDf$Accuracy)])
boxplot(maxValDf$Accuracy[-which.min(maxValDf$Accuracy)])
shapiro.test(maxValDf$Accuracy[-which.min(maxValDf$Accuracy)])

#
plot(modelResultsDf$Macro.F1.score)
hist(modelResultsDf$Macro.F1.score[!is.na(modelResultsDf$Macro.F1.score)])
boxplot(modelResultsDf$Macro.F1.score[!is.na(modelResultsDf$Macro.F1.score)])
shapiro.test(modelResultsDf$Macro.F1.score)

mrMacF1Df <- modelResultsDf[-which(is.na(modelResultsDf$Macro.F1.score)),]
maxs <- aggregate(Macro.F1.score ~ workId, data = mrMacF1Df, FUN = max)
hist(maxs$Macro.F1.score)
boxplot(maxs$Macro.F1.score)
shapiro.test(maxs$Macro.F1.score)

hist(maxs$Macro.F1.score[-which.min(maxs$Macro.F1.score)])
boxplot(maxs$Macro.F1.score[-which.min(maxs$Macro.F1.score)])
shapiro.test(maxs$Macro.F1.score[-which.min(maxs$Macro.F1.score)])

#
plot(modelResultsDf$Micro.F1.score)
hist(modelResultsDf$Micro.F1.score[!is.na(modelResultsDf$Micro.F1.score)])
boxplot(modelResultsDf$Micro.F1.score[!is.na(modelResultsDf$Micro.F1.score)])
shapiro.test(modelResultsDf$Micro.F1.score)

mrMicF1Df <- modelResultsDf[-which(is.na(modelResultsDf$Micro.F1.score)),]
maxs <- aggregate(Micro.F1.score ~ workId, data = mrMicF1Df, FUN = max)
hist(maxs$Micro.F1.score)
boxplot(maxs$Micro.F1.score)
shapiro.test(maxs$Micro.F1.score)

#
valuesVec <- modelFscoreMacroDf$Macro.F1.score / 100 # Rescale value
valuesVec <- maxs$Accuracy / 100 # Rescale value
valuesVec <- modelFscoreMicroDf$Micro.F1.score / 100 # Rescale value
summary(valuesVec)

par(mfrow = c(1,1))
#valuesCutVec <- valuesVec[valuesVec > threshold]
valuesCutVec <- valuesVec[-which.min(valuesVec)]
shapiro.test(valuesCutVec)
boxplot(valuesVec, valuesCutVec)
summary(valuesCutVec)

fit <- fitdistr(valuesCutVec, densfun = "normal")  # we assume my_data ~ Normal(?,?)
fit
hist(valuesCutVec, prob = TRUE, main = "")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col = "red", lwd = 2, add = T)

fit <- fitdistr(valuesCutVec, densfun = "gamma")  # we assume my_data ~ Normal(?,?)
fit
hist(valuesCutVec, prob = TRUE, main = "")
curve(dgamma(x, fit$estimate[1], fit$estimate[2]), col = "red", lwd = 2, add = T)


plotdist(valuesCutVec, histo = TRUE, demp = TRUE)
descdist(valuesCutVec, discrete = FALSE, boot = 500)

fit_n  <- fitdist(valuesCutVec, "norm")
fit_g  <- fitdist(valuesCutVec, "gamma")
fit_beta <- fitdist(valuesCutVec, "beta")
summary(fit_n)
summary(fit_g)
summary(fit_beta)

par(mfrow = c(2,2))
plot.legend <- c("norm", "gamma", "beta")
fitdistrplus::denscomp(list(fit_n, fit_g, fit_beta), legendtext = plot.legend)
fitdistrplus::cdfcomp(list(fit_n, fit_g, fit_beta), legendtext = plot.legend)
fitdistrplus::qqcomp(list(fit_n, fit_g, fit_beta), legendtext = plot.legend)
fitdistrplus::ppcomp(list(fit_n, fit_g, fit_beta), legendtext = plot.legend)

rsq <- function(data, indices) {
  d <- data[indices]
  return(mean(d, na.rm = T))
} 

results <- boot::boot(data = valuesCutVec, statistic = rsq, R = 900000)
plot(results)
nortest::ad.test(results$t[sample(1:100000, 1000),1])

# get 95% confidence interval
boot::boot.ci(results, type = "bca")

#
# Filter datasets by the given threshold
#
subModelFscoreMacroDf <- subset(modelResultsDf, !is.na(Macro.F1.score) & Macro.F1.score > threshold * 100)
subFscoreMacroMetaDf <- data.frame("Author" = subModelFscoreMacroDf$modelName, "Me" = subModelFscoreMacroDf$Macro.F1.score/100,	
                                "Se" = rep("", nrow(subModelFscoreMacroDf)),	"Mc" = rep(1/20, nrow(subModelFscoreMacroDf)),	
                                "Sc" = rep(0.0025, nrow(subModelFscoreMacroDf)), "Ne" = 7528,	"Nc" = 7528,	
                                "Subgroup" = rep(1, nrow(subModelFscoreMacroDf)))

#
# Save transformed data, i.e. only given subset
#
xmlsPath <- "../search-results/5-quantitative-analysis/"

schemaFileName <- "subFscoreMacroMetaData.xlsx"
openxlsx::write.xlsx(subFscoreMacroMetaDf, paste0(xmlsPath, schemaFileName))

#
# Article groups
#




