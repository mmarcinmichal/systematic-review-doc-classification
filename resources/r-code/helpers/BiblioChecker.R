bib1 <- "../biblio/biblio-reviewed-article.bib"
bib2 <- "../biblio/biblio-helper-article.bib"

options(encoding = "UTF-8")
tdf1 <- bib2df::bib2df(bib1)
options(encoding = "UTF-8")
tdf2 <- bib2df::bib2df(bib2)

plot(table(c(tdf1$BIBTEXKEY,tdf2$BIBTEXKEY)))

intersect(tdf1$BIBTEXKEY,tdf2$BIBTEXKEY)
