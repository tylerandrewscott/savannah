library(tidyverse)
library(data.table)
loc_string = 'input/extracted/named_entities_pdftools.csv'

talk = read_csv(loc_string) %>%
  mutate(Subject = gsub('^Dr\\. ','',Subject)) %>%
  mutate(Subject = gsub('^Mr\\. ','',Subject)) %>%
  mutate(Subject = gsub(', Jr.*','',Subject)) %>%
  mutate(Subject = gsub(' [A-Z]\\.','',Subject)) %>% mutate(Subject = gsub('Chiarman','Chairman',Subject)) %>%
  mutate(Council = str_extract(Meeting,'^[A-Z]{3}'),Meeting_Num = as.numeric(str_extract(Meeting,'(?!CM)[0-9]{1,2}')))


unique(talk$Council)


talk$Subject[talk$Subject=='Elmo'] = 'Elmo Richardson'
talk$Subject[talk$Subject=='Cliff Arnett'] = 'Clifford Arnett'
talk$Subject[talk$Subject=='James Hook'] = 'Jim Hook'
talk$Subject[talk$Subject=='Chairman Richardson'] = 'Elmo Richardson'
talk$Subject[talk$Subject=='Chairman Chase'] = 'Donald Chase'
talk$Subject[talk$Subject=='Chairman Lanier'] = 'Brinson Lanier'
talk$Subject[talk$Subject=='Chairman Royal'] = 'Richard Royal'
talk$Subject[talk$Subject=='Chairman Cross'] = 'Ron Cross'
talk$Subject[talk$Subject=="Commissioner Paris"] = 'Lamar Paris'
talk$Subject[talk$Subject=="Chair Windom"] = 'Matt Windom'
talk$Subject[talk$Subject=="Kevin Farrell"] = 'Kevin Ferrell'
talk$Subject[talk$Subject=="Steve Davis"] = 'Steven Davis'
talk$Subject[talk$Subject=="Jimmy Webb"] = 'James Webb'
talk$Subject = gsub('Udvardis','Udvardy',talk$Subject)
talk$Subject = gsub('Huddy','Harold',talk$Subject)
talk$Subject[talk$Subject=='Su'] = 'Tai-Yi Su'
talk$Subject[talk$Subject=="Chairman Bennett"&grepl('UOC',talk$Meeting)] = 'David Bennett'
talk$Subject[talk$Subject=="Chairman Bennett"&grepl('CNG',talk$Meeting)] = 'John Bennett'

talk = talk %>% filter(!Subject %in% c('Allen Barnes', 'Director Barnes','Carol Couch'))
                       
attendance = read_csv('https://docs.google.com/spreadsheets/d/15RrfB5JKJs74vaKqNFK43ucocQIV6HnwHlzNWDwtLL0/pub?output=csv')%>% 
  fill(Link,`Meeting #`,Date,Council,`Meeting Topic`) %>% mutate(Full_Name = paste(First, Last,sep=' ')) %>%
  mutate(Full_Name = gsub(' [A-Z]\\.|^[A-Z]\\. ','',Full_Name)) %>%
  mutate(Full_Name = gsub(',.*$','',Full_Name))




#as.data.frame(table(talk$Subject[!talk$Subject %in% attendance$Full_Name])) %>% arrange(-Freq) %>% .[1:10,]

library(stringr)

talk = talk %>% filter(paste(talk$Subject,talk$Council) %in% paste(attendance$Full_Name,attendance$Council))


temp = talk_filtered %>% group_by(Council,Meeting_Num) %>% filter(!Council %in% c('ALT','SSA')) %>% summarise(participation_count = n())
library(ggthemes)
ggplot(temp,aes(x=Meeting_Num,y=participation_count,group=Council,colour=Council)) + geom_line() + geom_point() + scale_color_tableau() +
  scale_x_continuous(breaks=c(1:11)) + scale_y_continuous(limits=c(0,75)) + theme_bw() + theme(legend.position = c(0.6,0.075),legend.direction='horizontal')

attendance_by_council = lapply(sort(unique(attendance$Council)),function(x) attendance %>% filter(Council==x))
names(attendance_by_council) = sort(unique(attendance$Council))
uq_attendees_by_council = lapply(attendance_by_council,function(x) sort(unique(x$Full_Name)))
base_council_matrices = lapply(uq_attendees_by_council,function(x) matrix(0,ncol=length(x),nrow=length(x),dimnames = list(x,x)))
names(base_council_matrices) = sort(unique(attendance$Council))



meeting_ref = expand.grid(sort(unique(attendance$Council)),1:11)
meeting_ref = meeting_ref[paste(meeting_ref$Var1,meeting_ref$Var2) %in% unique(paste(attendance$Council,attendance$`Meeting #`)),]

talk_summary = talk %>% group_by(Subject,Council,Meeting_Num)%>% summarise(participation_count = n())

base_mats = lapply(1:nrow(meeting_ref),function(x) base_council_matrices[[meeting_ref$Var1[x]]])

part_summary_by_meeting = lapply(1:nrow(meeting_ref),function(x) talk_summary %>% filter(Council == meeting_ref$Var1[x],Meeting_Num==meeting_ref$Var2[x]))

attendance_by_meeting = base_mats


for (i in (1:length(base_mats)))
{
  if (nrow(part_summary_by_meeting[[i]])!=0)
  {
    base_mats[[i]][match(part_summary_by_meeting[[i]]$Subject,rownames(base_mats[[i]])),colnames(base_mats[[i]]) %in% attendance$Full_Name[attendance$Council==meeting_ref$Var1[i]&attendance$`Meeting #`==meeting_ref$Var2[i]]] <- part_summary_by_meeting[[i]]$participation_count
  }
}

participation_networks_by_council  = lapply(names(base_council_matrices),function(x) base_mats[meeting_ref$Var1==x])
names(participation_networks_by_council) = names(base_council_matrices)
lapply(1:length(participation_networks_by_council),function(x) names(participation_networks_by_council[[x]]) <<- paste(names(participation_networks_by_council)[x],1:length(participation_networks_by_council[[x]]),sep='_'))


test = participation_networks_by_council[[3]]
talk %>% filter(Council == 'CGA')

library(statnet)
library(btergm)
lapply(test,sum)

btergm(test~edges + mutual,R=100)

























