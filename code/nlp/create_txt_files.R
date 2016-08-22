#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)


library(googlesheets)
library(dplyr)
library(lubridate)
att_link = 'https://docs.google.com/spreadsheets/d/15RrfB5JKJs74vaKqNFK43ucocQIV6HnwHlzNWDwtLL0/edit?usp=sharing'
roster_link = 'https://docs.google.com/spreadsheets/d/1IiNvfbnyvDyJpUmRgoCtPpCUJjbgPIv1i-DkqeFdK_I/pubhtml'

temp = googlesheets::gs_read(gs_url(att_link)) %>% rename(Meeting = `Meeting #`,Meeting_Topic = `Meeting Topic`) %>%
  mutate(Date = zoo::na.locf(Date),Meeting = zoo::na.locf(Meeting), Council = zoo::na.locf(Council))
temp = temp %>% mutate(Date = mdy(Date)) %>% mutate(uq.event = paste(Date,Council),
                                                    full_name = paste(First, Last,sep=' '))  %>% filter(!is.na(First))

rm(temp)
t1 = temp$Link[!is.na(temp$Link)]
missing = !(gsub(".*\\/","",t1) %in% list.files('input/meeting_pdf/'))
download.file(t1[missing],paste0('input/meeting_pdf/',gsub(".*\\/","",t1[missing])))


download.file("http://www.middlechattahoochee.org/documents/111109_CM4_MCH_MEETINGMINUTESr2.pdf",'test.pdf')
table(missing)

t1[missing]

pdf_files = list.files('input/meeting_pdf/')

plyr::l_ply(pdf_fils,)


for(i in t1){
  rr <- tt(elem=list(uri=i), language="en")
  write.table(unlist(rr), file=paste('input/meeting_text/',gsub(".*\\/","",i), ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}




for(i in t1[!(gsub(".*\\/","",t1) %in% list.files('input/meeting_pdf/'))])
  {
  # dfile2<-paste("win/project/bogachiel/baker/BakerPDFS/pdfs2/",t1[i],sep="")
  rr <- tt(elem=list(uri=getURI(i)), language="en")
  write.table(unlist(rr), file=paste('input/meeting_text/',gsub(".*\\/","",i), ".txt",sep=""), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}


rr = tt(elem = list(uri = "http://www.flintochlockonee.org/documents/20110210_LFO_CM9_MEETING_SUMMARY_000.pdf"),language="en")


rr = tt(elem = list(uri = "http://www.middlechattahoochee.org/documents/MCH_CM3_Meeting_Minutes_091709g.pdf"),language="en")


getU
t1


length(list.files('input/meeting_pdf/'))
length(t1)
gsub(".*\\/","",t1[i])
