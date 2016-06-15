library(googlesheets)
ref_link = 'https://docs.google.com/spreadsheets/d/1O2m4-7dWPRmpE0N6xX2BS4qQSTJAzA6T3R0cu2YnwNQ/pubhtml'
refs = googlesheets::gs_read(gs_url(ref_link))

test_link = 'http://www.middlechattahoochee.org/documents/MCH_Adopted_RWP.pdf'
RSelenium::checkForServer()
RSelenium::startServer() #if required
require(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "firefox")

remDr$open()
remDr$navigate(test_link)
remDr$getTitle()[[1]] # [1] "Google"


#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)
t1<-list.files("win/project/bogachiel/baker/BakerPDFS/pdfs2")
t1[1]

test_link = 'http://www.georgiawaterplanning.org/documents/MCH_Adopted_RWP.pdf'


tt <- readPDF(engine=c("ghostscript"))
rr <- tt(elem=list(uri=getURL(test_link)),language='en')
tt

sed 's/\\0//g' MCH_Adopted_RWP.pdf > MCH_Adopted_RWP.pdf


for(i in 1:length(t1)){
  dfile2<-paste("win/project/bogachiel/baker/BakerPDFS/pdfs2/",t1[i],sep="")
  rr <- tt(elem=list(uri=dfile2), language="en")
  write.table(unlist(rr), file=paste("win/project/bogachiel/baker/BakerPDFS/txt2/",t1[i], ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}


require(RSelenium)
pJS <- phantom()



Sys.sleep(5) # give the binary a moment
remDr <- remoteDriver(browserName = 'phantomjs')

remDr$close
pJS$stop() # close the PhantomJS process, note we dont call remDr$closeServer()



remDr$open()
head(refs)
remDr$getStatus()
