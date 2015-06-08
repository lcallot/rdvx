# Some functions for the Rendez-vous avec X text mining.
# Author: Laurent Callot
# 08/06/2015

clean_corpus <- function(docs,dico=NULL,stem_complete=FALSE,stem=FALSE,rm_common=1000){
  
  docs <- tm_map(docs, content_transformer(tolower)) # Lower case
  docs <- tm_map(docs, removeNumbers)# Remove numbers
  docs_orig <- docs
  
  # Stemming
  if(stem)docs <- tm_map(docs, stemDocument,language='french')
  #Stemming reversion
  if(!stem) stem_complete=FALSE
  if(is.null(dico)) dico <- docs_orig
  if(stem_complete){
    docs <- tm_map(docs, content_transformer(function(x, d)
      paste(stemCompletion(strsplit(stemDocument(x), ' ')[[1]], d, 'shortest'), collapse = ' ')), docs_orig)
    docs <- tm_map(docs,PlainTextDocument)  
    docs <- docs %>% VectorSource %>% Corpus 
  }
  
  # stopword from a another list
  slw <- lapply(read.table('data/stop.txt',fileEncoding = 'latin1',stringsAsFactors = FALSE),enc2native)$V1
  docs <- tm_map(docs, removeWords, c(slw))
  # and yet another list
  slw <- lapply(read.table('data/stopwords_fr.txt',fileEncoding = 'latin1',stringsAsFactors = FALSE),enc2native)$V1
  docs <- tm_map(docs, removeWords, c(slw))
  docs <- tm_map(docs, removeWords, stopwords('french')) # Remove common stopwords
  # 10000 most common french words for stem reversion
  common_wd <- lapply(read.table('data/top10000fr.txt',fileEncoding = 'latin1',stringsAsFactors = FALSE),enc2native)$V1
  if(rm_common>0)docs <- tm_map(docs, removeWords, c(head(common_wd,rm_common)))# Removing common words
  
  # This cleaning does not work very well with french punctuation. Or it's just me. 
  docs <- tm_map(docs, removePunctuation,preserve_intra_word_dashes = FALSE) # Remove punctuations
  docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
  


  # and some manual word removal
  docs <- tm_map(docs, removeWords,  c('monsieur','aujourd','après','peutêtre'
                                       ,'abord','estil','affaire'))
  
  # Removing 100 and 1000 most common french words.
  # list from: http://wortschatz.uni-leipzig.de/Papers/top100fr.txt
  # Not working with 10000 words.
  #common_wd <- lapply(read.table('top1000fr.txt',fileEncoding = 'latin1',stringsAsFactors = FALSE),enc2native)$V1
  #docs <- tm_map(docs, removeWords, c(common_wd))
  #common_wd <- lapply(read.table('top100fr.txt',fileEncoding = 'latin1',stringsAsFactors = FALSE),enc2native)$V1
  #docs_cm100 <- tm_map(docs, removeWords, c(common_wd))
  
  return(docs)
}



wcloud <- function(docs,wlength=3,maxw=50,scale=c(4,.2)){
  # Term matrix construction
  tdmx <- TermDocumentMatrix(docs, control = list(wordLengths =c(wlength,Inf) ))
  
  # color palette
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:4)]
  #brewer.pal(8, "Dark2")
  
  # making word cloud.
  m <- t(as.matrix(tdmx))
  v = sort(colSums(m), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  wordcloud(d$word, max.words=maxw, d$freq, scale=scale, min.freq=1,colors=pal, rot.per=.25)
}




GetRDVXSynop <- function(){
  library('magrittr')
  library('rvest')
  
  # base url for the episods
  base_url <- 'http://rendezvousavecmrx.free.fr/page/detail_emission.php?cle_emission='
  
  # first show with description: 156
  # last documented show: 761
  show_smpl <- 1:761
  
  # storage
  raw_data <- matrix(NA,ncol=3,nrow=length(show_smpl))
  colnames(raw_data) <- c('date','title','synopsis')
  
  # Gathering the raw data.
  for(show in show_smpl){
    show_url <- paste0(base_url,show) 
    show_url %>% html -> show_content
    # extracting the relevant fields
    titre <- show_content %>% html_node(.,css='div#titre') %>% html_text
    synop <- show_content %>% html_node(.,css='div#emission') %>% html_text
    datediff <- show_content %>% html_node(.,css='strong') %>% html_text
      
    raw_data[show-156,] <- c(datediff,titre,synop)
    print(show)
    }
  save(raw_data,file='rawdata_rdvX_new')
}