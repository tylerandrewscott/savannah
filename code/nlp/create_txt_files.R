#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)
library(pdftools)
# library(googlesheets)
# library(dplyr)
# library(lubridate)
# att_link = 'https://docs.google.com/spreadsheets/d/15RrfB5JKJs74vaKqNFK43ucocQIV6HnwHlzNWDwtLL0/pubhtml'
# att_link = 'https://docs.google.com/spreadsheets/d/15RrfBy5JKJs74vaKqNFK43ucocQIV6HnwHlzNWDwtLL0/edit?usp=sharing'
# roster_link = 'https://docs.google.com/spreadsheets/d/1IiNvfbnyvDyJpUmRgoCtPpCUJjbgPIv1i-DkqeFdK_I/pubhtml'
# 
# temp = gs_read(gs_url(att_link)) %>% rename(Meeting = `Meeting #`,Meeting_Topic = `Meeting Topic`) %>%
#   mutate(Date = zoo::na.locf(Date),Meeting = zoo::na.locf(Meeting), Council = zoo::na.locf(Council))
# temp = temp %>% mutate(Date = mdy(Date)) %>% mutate(uq.event = paste(Date,Council),
#                                                     full_name = paste(First, Last,sep=' '))  %>% filter(!is.na(First))
# t1 = temp$Link[!is.na(temp$Link)]
# t1 = t1[!duplicated(t1)]
rm(temp)
#download.file(t1,paste0('input/meeting_pdf/',gsub(".*\\/","",t1)))
#missing = !(gsub(".*\\/","",t1) %in% list.files('input/meeting_pdf/'))
#download.file(t1[missing],paste0('input/meeting_pdf/',gsub(".*\\/","",t1[missing])))

pdf_files =list.files('input/meeting_pdf/',pattern='pdf')

tt_xpdf <- readPDF(engine=c("xpdf"))
tt_ghostscript <- readPDF(engine=c("ghostscript"))

for(i in 1:length(pdf_files)){
  print(i)
  dfile2<-paste0('input/meeting_pdf/',pdf_files[i])
  rr <- tt_xpdf(elem=list(uri=dfile2), language="en")
  write.table(unlist(rr), file=paste("input/meeting_text/xpdf/",pdf_files[i], ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
  rr <- tt_ghostscript(elem=list(uri=dfile2), language="en")
  write.table(unlist(rr), file=paste("input/meeting_text/ghostscript/",pdf_files[i], ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
  rr <- pdf_text(dfile2)
  write.table(unlist(rr), file=paste("input/meeting_text/pdftools/",pdf_files[i], ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}

