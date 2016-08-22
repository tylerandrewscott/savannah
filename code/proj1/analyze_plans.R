library(RTextTools)
library(tm)


f1 = list.files('input/plantext/',pattern='*.txt')
f1
for(i in 1:length(f1)){
  dfile<-paste("input/plantext/",f1[i],sep="")
  rr <- tt(elem=list(uri=dfile), language="en")
  write.table(unlist(rr), file=paste("input/plantext",t1[i], ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}


dbprocess<-tm_map(f1, stripWhitespace)


Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    

library(tm) 
cname <- file.path('input','plantext')   
docs <- Corpus(DirSource(cname))   

summary(docs)

stopwords("english")  
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)
library(filehash)

f1 = read_data('input/plantext/',type='folder',index=gsub('_.*','',f1))

dbp<-PCorpus(URISource(t1,mode=""),readerControl=list(reader=tt),dbControl=list(dbName="win/project/bogachiel/baker/BakerPDFS/pdfcorpus.db",dbType="DB1"))
dbprocess<-tm_map(dbp, stripWhitespace)
dbp<-dbInit("win/project/bogachiel/baker/BakerPDFS/pdfcorpus.db")
dbprocess<-tm_map(dbprocess, content_transformer(tolower))
dbp<-dbInit("win/project/bogachiel/baker/BakerPDFS/pdfcorpus.db")
tm_map(dbprocess, removeWords, stopwords("english"))
dbp<-dbInit("win/project/bogachiel/baker/BakerPDFS/pdfcorpus.db")
dbp<-tm_map(dbp, stemDocument)
dbp<-dbInit("win/project/bogachiel/baker/BakerPDFS/pdfcorpus.db")




test = read_data('input/plantext/OutputALT_Adopted_RWP_000.pdf.txt')

?read_data
modst = train_models(test,'SVM')
