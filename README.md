# The outcomes and publication standards of research descriptions in document classification: a systematic review of publications reporting the use of 20 Newsgroups bydate dataset

The main goals of this study are to provide an original systematic review of document and text classification methods (machine and deep learning methods), and to provide the reader with the techniques and progress achieved in the field of document classification. For this purpose, we have evaluated the collective effort of various document classification methods through the prism of 20 Newsgroups – a well-known and well-established dataset that researchers frequently and broadly use to evaluate the proposed classification solutions alongside other datasets. To achieve our goals, we performed qualitative and quantitative analysis on a set of recent studies that used the same dataset. The qualitative analysis has shown eight key domains of research interest: (1) learning methods in the manipulation of input training data; (2) pre-processing methods; (3) feature weighting methods; (4) feature selection methods; (5) feature projection methods; (6) classification methods; (7) evaluation methods; and (8) benchmarking. The quantitative research has allowed us to demonstrate the performance of the methods, characterize research patterns, and evaluate the possibility of reproducibility. Based on the quantitative analyses, we have obtained state-of-the-art results. We have found no evidence that the year of publication explains the variation in accuracy and macro F-score results. Moreover, we have discovered that there are no significant differences between the approaches to document classification evaluated (the goals of these articles can differ; they are assessed here, however, in the context of document classification results). We conclude that the reproducibility of many of the analyzed studies might prove problematic for the scientific community. The resulting overview covers the entire spectrum of document classification methods, and will contribute to a better understanding of various aspects of the document classification field. Furthermore, the proposed systematic way of the conducted review helped us to avoid drawbacks of the traditional surveys or reviews and reduced systematic error reference to such works. 

The repository contains supplementary materials for the article titled "The outcomes and publication standards of research descriptions in document classification: a systematic review". We share research data, i.e.:
1. Raw data, i.e. bibliographies, and filled questionnaires for each article are publicly available.
2. A code allows creating a technical report based on the raw data is publicly available.

The project is structured as follow:
* *resources\biblio catalogue* contains bibliography files, i.e.:
  * *biblio-helper-article.bib* - the articles other than reviewed articles that were used in the article
  * *biblio-reviewed-article.bib* - the reviewed artilces
  * *biblio-reviewed-article-prop-form.bib* - the reviewed articles formatted thanks to Madely software
* *resources\img* catalogue contains all saved images created during analysis performed thanks to R markdown (https://rmarkdown.rstudio.com/) which provides a unified format for writing reproducible, dynamic reports with R.
* *resources\r-code* catalogue includes files, such as:
  * *QuestionsAnalysis.Rmd* - it is an R markdown file which combining code, its results, and commentary.
  * *QuestionsAnalysis.html* - it is a final pre-compiled technical report created thanks to *QuestionsAnalysis.Rmd*, i.e. it is an output of the *QuestionsAnalysis.Rmd*
  * *QuestionsAnalysis.pdf* - it is a final pre-compiled technical report created thanks to *QuestionsAnalysis.Rmd*, i.e. it is an output of the *QuestionsAnalysis.Rmd*
  * *QuestionsAnalysis.md* - it is a final technical pre-compiled report created thanks to *QuestionsAnalysis.Rmd*, i.e. it is an output of the *QuestionsAnalysis.Rmd*
* *resources\search-results\4-qq-assesments* folder includes files, such as:
  * *v0 - article-assesment-schema.xlsx* and *v1 - article-assesment-schema.xlsx* - the questionnaire schema, we use schema version v1 to assesment each article in our review
  * *[ID] - [Work title].xlsx* - the filled questionaires for each reviewed work (62 questionaires)
* *resources\tables\latex* catalogue contains the created tables during analysis:
  * tables without suffix, for example, *table-grLearninMeth.tex* contain a little processing comments about (1) Technical, and algorithmic aspect of the work, (2) Findings/recommendations of the research, and (3) Highlighted challenges or open problems from the questionnaires
  * tables with suffix 1, for example *table-grLearninMeth-1.tex* contain summarisation of descriptions from tables mentioned above
  * tables with suffix 2, for example *table-grLearninMeth-2.tex* contain summarisation of descriptions from tables with suffix 1
  * tables with suffix 3, for example *table-grLearninMeth-3.tex* contain revised, improved and corrected summarisation of descriptions from tables with suffix 2
  * table-data-to-review-1.tex (*table-data-to-review-1-1.pdf*, *table-data-to-review-1-1.png*) - shows information about number of articles collected during different review stages

We recommend use R Studio (https://rstudio.com/) to compile/build the *QuestionsAnalysis.Rmd* for creating the technical report (*QuestionsAnalysis.html*) from scratch. However, the created report is available and ready to read.
