#
# Data analysis for spiking neural network output
#

#
# Clear workspace
#
rm(list = ls())

#
# Required libraryies
#
toLoadLibraries <-
  c(
    "meta",
    "dmetar"
  )
loadedLibraries <- (.packages())
unloadedLibraries <- setdiff(toLoadLibraries, loadedLibraries)
if (length(unloadedLibraries) != 0) {
  sapply(unloadedLibraries, function(x)
    library(x, character.only = TRUE))
}

#
#
#
xmlsPath <- "../search-results/5-quantitative-analysis/"
fileName <- "accuracyMaxMetaData.xlsx"

metaDataXmls <- readxl::read_excel(paste0(xmlsPath, fileName))

str(metaDataXmls)

#metaDataXmls <- metaDataXmls[-c(which(metaDataXmls$Me == min(metaDataXmls$Me))), ] 
#metaDataXmls <- metaDataXmls[-c(which(metaDataXmls$Me == min(metaDataXmls$Me))), ] 

m.raw <- meta::metacont(
                  Ne, Me, Se,
                  Nc, Mc, Sc,
                  data = metaDataXmls,
                  studlab = paste(stringr::str_sub(metaDataXmls$Author, 1, 10)),
                  comb.fixed = F,
                  comb.random = T,
                  hakn = FALSE,
                  prediction = TRUE,
                  sm = "SMD",
                  method.smd = "Glass",
                  sd.glass = "control")

m.raw
meta::forest(m.raw)

res2 <- m.raw
res2$vi.f <- res2$vi.f[1:10]
meta::forest(res2, xlim = c(-3, 4))

find.outliers(m.raw)
meta::forest(find.outliers(m.raw))

df <- data.frame(x = 1:length(m.raw$upper),
                 F = m.raw$TE,
                 L = m.raw$lower,
                 U = m.raw$upper)
sDf <- df[order(df$F),]
sDf$x <- 1:nrow(sDf)

require(ggplot2)
ggplot(sDf, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))


