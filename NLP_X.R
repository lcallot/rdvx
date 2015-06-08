
library(NLP)
library(openNLP)
library(RWeka)
library(qdap)
library(magrittr)
library(openNLPmodels.it)


#install.packages("openNLPmodels.es",
#                 repos = "http://datacube.wu.ac.at/",
#                 type = "source")



#

# Loading the data
load('rawdata_rdvX')
#raw_data <- raw_data[1:50,]

# CLEAN THE DATA!

raw_txt <- paste(raw_data[,2],collapse = ' ')
raw_txt <- as.String(raw_txt)

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

txt_annotations <- annotate(raw_txt, list(sent_ann, word_ann))
txt_doc <- AnnotatedPlainTextDocument(raw_txt, txt_annotations)

sents(txt_doc) %>% head(2)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
txt_annotations <- annotate(raw_txt, pipeline)
txt_doc <- AnnotatedPlainTextDocument(raw_txt, txt_annotations)


entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(txt_doc, kind = "person")
entities(txt_doc, kind = "location")
entities(txt_doc, kind = "organization")

