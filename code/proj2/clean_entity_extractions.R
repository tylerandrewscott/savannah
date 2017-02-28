library(tidyverse)
library(data.table)
loc_string = 'input/extracted/named_entities.html'

talk = data.table::fread(loc_string) %>% 
  mutate(Subject = gsub('^Dr\\. ','',Subject)) %>%
  mutate(Subject = gsub('^Mr\\. ','',Subject)) %>%
  mutate(Subject = gsub(' [A-Z]\\.','',Subject)) 
talk$Subject[talk$Subject=='Elmo'] = 'Elmo Richardson'
talk$Subject[talk$Subject=='Chairman Richardson'] = 'Elmo Richardson'
talk$Subject[talk$Subject=='Chairman Bennett'&grepl('Coosa_')] = 'Elmo Richardson'



talk$Subject[talk$Subject=="Commissioner Paris"] = 'Lamar Paris'
talk$Subject = gsub('Udvardis','Udvardy',talk$Subject)


talk$Subject[talk$Subject=='Su'] = 'Tai-Yi Su'

attendance = read_csv('https://docs.google.com/spreadsheets/d/15RrfB5JKJs74vaKqNFK43ucocQIV6HnwHlzNWDwtLL0/pub?output=csv')%>% 
  fill(Link,`Meeting #`,Date,Council,`Meeting Topic`) %>% mutate(Full_Name = paste(First, Last,sep=' ')) %>%
  mutate(Full_Name = gsub(' [A-Z]\\.','',Full_Name)) %>%
  mutate(Full_Name = gsub(',.*$','',Full_Name))


as.data.frame(table(talk$Subject[!talk$Subject %in% attendance$Full_Name])) %>% arrange(-Freq) %>% .[1:10,]

talk[talk$Subject=='Chairman Bennett'&grepl('Oconnee|'),]

table(talk$Subject %in% attendance$Full_Name)

test_talk$Subject
test_talk$Subject
test_att$Full_Name


