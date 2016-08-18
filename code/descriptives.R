library(googlesheets)
library(dplyr)
library(lubridate)
att_link = 'https://docs.google.com/spreadsheets/d/15RrfB5JKJs74vaKqNFK43ucocQIV6HnwHlzNWDwtLL0/edit?usp=sharing'
roster_link = 'https://docs.google.com/spreadsheets/d/1IiNvfbnyvDyJpUmRgoCtPpCUJjbgPIv1i-DkqeFdK_I/pubhtml'

temp = googlesheets::gs_read(gs_url(att_link)) %>% rename(Meeting = `Meeting #`,Meeting_Topic = `Meeting Topic`) %>%
  mutate(Date = zoo::na.locf(Date),Meeting = zoo::na.locf(Meeting), Council = zoo::na.locf(Council))
temp = temp %>% mutate(Date = mdy(Date)) %>% mutate(uq.event = paste(Date,Council),
                       full_name = paste(First, Last,sep=' '))  %>% filter(!is.na(First))

roster = googlesheets::gs_read(gs_url(roster_link)) %>% mutate(member = paste(First,Last,sep=' '))

attendance = temp %>% filter(is.na(Organization)) %>% group_by(Council,Meeting) %>% summarize(n = n())
attendance = left_join(attendance,attendance %>% summarise(avg = mean(n)))
nmem = roster %>% group_by(Planning.Council) %>% summarize(num_members = n()) %>%
  rename(Council = Planning.Council)
attendance = left_join(attendance,nmem)
temp = temp %>% mutate(member =  !is.na(Council.position) & Council.position == 'Council Member')
meetings_attended = temp %>% #filter(is.na(Organization)) %>%
  group_by(Council,full_name,member) %>% summarize(meetings_attended = n())

mheld = temp %>% group_by(Council) %>% summarize(meetings_held = length(unique(uq.event)))
meetings_attended$meetings_held = mheld$meetings_held[match(meetings_attended$Council,mheld$Council)]
meetings_attended = meetings_attended %>% mutate(prop = meetings_attended/meetings_held)


name_ref = data.frame(region_name = 
                        c('Altamaha','Coastal Georgia','Coosa-North Georgia',
                          'Lower Flint-Ochlockonee','Middle Chattahoochee',
                          'Middle Ocmulgee','Suwanee-Satilla','Savannah-Upper Ogeechee',
                          'Upper Flint','Upper Oconee','Metro North Georgia'),
                      Council = c('ALT','CGA','CNG','LFO','MCH',
                                  'MOC','SSA','SUO','UFL','UOC','MNG'))



attendance = left_join(attendance,name_ref)


att_slopes = data.frame(slopes = sapply(unique(attendance$Council),function(x) 
coef(lm(n~Meeting,data = attendance %>% filter(Council==x)))['Meeting']),
Council = unique(attendance$Council))

attendance = left_join(attendance,att_slopes)

library(ggplot2)
library(ggthemes)
p2 = ggplot(attendance,aes(y=n,x=Meeting)) + 
  stat_smooth(method='lm',alpha=0.2,se=FALSE,colour='grey80') + 
  geom_line() + geom_point() + 
  #scale_colour_colorblind() + 
  theme_tufte(ticks=F) + 
  scale_x_continuous(breaks=seq(1,11,2),name='Meeting #') +
  scale_y_continuous(name='# Attendees') + 
  scale_colour_tableau() + guides(colour=FALSE)+
  #coord_flip() + 
  facet_wrap(~region_name) + geom_abline(aes(intercept=num_members,slope=0,lty='d')) +
  scale_linetype_manual(
    name='',values = 2,label='# total members') +
  theme(legend.position = c(0.8,.2),legend.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size=14),
        axis.text=element_text(size=14)) +
  annotate("text",x=9,y = 30,label = paste0('b = ',round(
    sapply(sort(unique(attendance$region_name)),function(x) 
      coef(lm(n~Meeting,data = attendance %>% filter(region_name==x)))['Meeting'])
    ,2))) 



group_attendance  = meetings_attended %>% filter(member) %>% group_by(Council) %>% 
  summarize(avg.prop = round(mean(prop),2),med.prop = round(median(prop),2),
            prophalf = round(mean(prop>.5),2),
            class = ifelse(prophalf>.6,'high','low'),
            avg.att = mean(meetings_attended),
            sd.att = sd(meetings_attended))

just_member_attendance = meetings_attended[meetings_attended$member,]

just_member_attendance = left_join(just_member_attendance,group_attendance)

library(scales)


#show_col(colorblind_pal()(8))
mean_col = '#56B4E9'
sd_col = '#E69F00'

(p3 = ggplot(just_member_attendance,
             aes(x=meetings_attended)) +
  geom_bar() + 
  theme_tufte(ticks=F) +
 # geom_density() +
  facet_wrap(~Council,scales = 'free_y') +  
  scale_x_continuous(name='# meetings attended by individual',
                     breaks=c(3,6,9)) +
 # scale_fill_colorblind(name='',labels=c('Board Member','Professional')) + 
  scale_y_continuous(limits = c(0,41),name = 'Council Members')+
  theme(legend.position = c(0.8,.2),legend.text=element_text(size=16),
        axis.title=element_text(size=16),axis.text.x=element_text(size=14),
        strip.text=element_text(size=14),
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  geom_errorbarh(aes(xmin = avg.att-sd.att,xmax = avg.att+sd.att,y=15,
                     col = 'SD', height = 4),lwd=1.25) +
  geom_point(aes(x=avg.att,y=15,col = 'Mean'),size=2) +
  scale_colour_manual(values = c(mean_col,sd_col),
                        labels=c('Mean','+/- 1 SD'))+
  guides(colour = guide_legend(override.aes = 
                                 list(shape = c(19,32),
                                      linetype = c(0,1),
                                      width = c(0,3))))
)

p4 =   ggplot(meetings_attended,aes(x=prop,fill=Council.position)) + 
geom_bar()+
  theme_tufte(ticks=F)+
  facet_wrap(~Council)+
  scale_y_continuous(name='Density') + 
  scale_x_continuous(name='Proportion of Meetings Attended by Member',breaks=c(0.25,0.75)) +
  #scale_fill_colorblind(name='',labels=c('Board Member','Professional')) + 
  theme(legend.position = c(0.8,.2),legend.text=element_text(size=16),
        axis.title=element_text(size=16),axis.text.x=element_text(size=14),
        axis.text.y=element_blank())


plan_goals_link = 'https://docs.google.com/spreadsheets/d/1xNbRL31keNT59yhr-43xUqLoaAkvdGT4rMAYCIKCS18/pub?gid=0&single=true&output=csv'



goals = read.csv(plan_goals_link)

goals = goals %>% mutate(Spec_Score = Defined_Direction + Measurable_Outcome + 
  Specified_Action)
goals = left_join(goals,name_ref)



forecast_link = 'https://docs.google.com/spreadsheets/d/14Xj5BtadVkx1wTYeK-7U_QivDLeau7Pm9PSa6F_IDms/pub?gid=233023918&single=true&output=csv'
forecasts = read.csv(forecast_link)
library(tidyr)
forecasts = forecasts %>% mutate(
  uq = paste(Council, Demand.Category),
  perc_change = {(X2050 - X2010)/X2010} * 100) %>%
  select(-contains('X'))  %>% select(-Source) %>%
  filter(grepl('Total',Demand.Category))
bmp_link = 'https://docs.google.com/spreadsheets/d/1FHPQYSghLryzkWtojWRNVY2K8oCPyFlIE0zK9mvBMfQ/pub?gid=0&single=true&output=csv'
bmps = read.csv(bmp_link)
bmp_count = 
  bmps %>% group_by(Council,Side) %>% 
  summarise(bmp_count = n()) %>% 
  complete(Council,Side,fill=list(bmp_count = 0))
bmp_count = left_join(bmp_count,name_ref)

cwa_sum = read.csv('Input/region_cwa_summary.csv',row.names=1)
cwa_sum = cwa_sum %>% select(-Total_Stream_Length,-Total_Length_M) %>%
  filter(Evaluation == 'Not Supporting'&Region!='MNG') %>%
  rename(Council = Region) %>%
  mutate(Council = gsub('UPF','UFL',Council)) %>%
  mutate(Council = gsub('UPO','UOC',Council)) %>%
  rename(Issue = Evaluation,Pressure = Proport_Status) %>%
  mutate(Pressure  = 100 * Pressure, Std_Pressure = scale(Pressure,center = F))

forecast_pressure = forecasts %>% select(-uq) %>% 
  rename(Issue = Demand.Category,Pressure = perc_change) %>%
  mutate(Std_Pressure = scale(Pressure,center = F))

issue_pressure = full_join(forecast_pressure,cwa_sum)
issue_pressure = full_join(issue_pressure,issue_pressure %>% filter(Issue == 'Total_Demand') %>% 
  mutate(Issue = gsub('Demand','Supply',Issue)))

issue_pressure$Issue = gsub('Total_','',issue_pressure$Issue)
issue_pressure$Issue = gsub('Wastewater','Returns',issue_pressure$Issue)
issue_pressure$Issue[issue_pressure$Issue=='Not Supporting'] = 'Quality'

issue_pressure$region_name = name_ref$region_name[match(issue_pressure$Council,name_ref$Council)]

response = left_join(issue_pressure,bmp_count %>% rename(Issue = Side))
response$Std_bmp = scale(response$bmp_count,center=F)

response_and_att = full_join(just_member_attendance,response[!duplicated(response),])


response_and_att = response_and_att %>% mutate(Overall_Part = ifelse(avg.att/sd.att>2.5,'High','Low'))

unique(response_and_att$avg.att/response_and_att$sd.att)

response_and_att = response_and_att %>% mutate(test_ovpart = ifelse(avg.att/sd.att >3,'1.high',ifelse(avg.att/sd.att<2,'3.low','2.med')))


response_and_att = left_join(response_and_att,att_slopes)
response_and_att$Attrition = ifelse(response_and_att$slopes>(-0.85),
                                    'Low attrition','High attrition')

response_and_att = response_and_att %>% mutate(attrit_class = ifelse(slopes<(-1.25),'1.high',ifelse(slopes>(-0.75),'3.low','2.med')))



library(viridis)

gg_fig6 = 
  ggplot(response_and_att,
         aes(x=Pressure,y=bmp_count,
           shape=test_ovpart,
           #size = avg.att/sd.att,
           fill = test_ovpart)) +
  geom_smooth(aes(x=Pressure,y=bmp_count,group='black'),
              method='lm',lty=2,se=F,col='black')+
  geom_point(size=4)  +  facet_wrap(~Issue,scales='free')+
  theme_tufte(ticks=F) +
  scale_y_continuous(name = '# BMPs adopted')+
  scale_x_continuous(name = 'Problem Severity (% Projected Increase in Need or % Impaired [Quality])')+
  theme(#axis.text.y=element_blank(),
        strip.text = element_text(size=16),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14),
        legend.position = c(0.91,.30),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.background = element_rect(fill = 'grey80',colour = 'grey80')) +
  scale_fill_viridis(name = 'Overall \nParticipation',discrete=TRUE,
                      labels=c('High','Medium','Low')) +
scale_shape_manual(name = 'Overall \nParticipation',
                    labels=c('High','Medium','Low'),values=c(21:23)) +
  guides(fill = guide_legend(override.aes = 
                                 list(linetype = c(0,0,0))))


gg_fig7 = 
  ggplot(response_and_att,
         aes(x=Pressure,y=bmp_count)) + 
     facet_wrap(~Issue,scales='free_x')+
    theme_tufte(ticks=F)   +
    scale_x_continuous(name = 'Problem Severity (% Projected Increase in Need or % Impaired [Quality])')+
    scale_y_continuous(name = '# BMPs adopted')+
    geom_smooth(aes(x=Pressure,y=bmp_count,group='black'),
                method='lm',lty=2,se=F,col='black')+
    geom_point(aes(fill=Attrition),shape = 21,size=4) + 
    scale_fill_viridis(direction = -1,name = 'Attrition',discrete=T)+#,labels=c('High','Medium','Low')) +
    theme(#axis.text.y=element_blank(),
      strip.text = element_text(size=16),
      axis.title = element_text(size=16),
      axis.text = element_text(size=14),
      legend.position = c(0.91,.35),
      legend.text = element_text(size=14),
      legend.title = element_text(size=14),
      legend.background = element_rect(fill = 'grey80',colour = 'grey80'))
               
          
goal_spec = goals %>% group_by(Council) %>% summarise(spec_mean = mean(Spec_Score))

response_and_att = left_join(response_and_att,goal_spec)
goals$Overall_Part = response_and_att$Overall_Part[match(goals$Council,response_and_att$Council)]
goals$test_ovpart = response_and_att$test_ovpart[match(goals$Council,response_and_att$Council)]
goals$Attrition = response_and_att$Attrition[match(goals$Council,response_and_att$Council)]

gg_fig8 = 
  ggplot(goals) + 
  geom_bar(aes(x=as.factor(Spec_Score),fill = paste(Overall_Part,Attrition)))  +
  facet_wrap(~region_name)+
  theme_tufte(ticks=F) +
  scale_x_discrete(name = 'Goal Quality Score (0 = low quality, 3 = satisifies all 3 criterion)',labels=c('Poor','1','2','Strong')) +
  scale_y_continuous(name = '# Goals') +
  scale_fill_colorblind(name = 'Participation - Attrition',
                     labels=c('High - High','High - Low','Low - High','Low - Low'))+
            #         labels=c('High','Medium','Low')) +
  theme(#axis.text.y=element_blank(),
    strip.text = element_text(size=16),
    axis.title = element_text(size=16),
    axis.text = element_text(size=14),
    legend.position = c(0.7,.2),
    legend.text = element_text(size=14),
    legend.title = element_text(size=14),
    legend.background = element_rect(fill = 'grey80',colour = 'grey80'))

  
gg_fig9 = 
  ggplot(goals) + 
  geom_bar(aes(x=as.factor(Spec_Score),fill = Attrition))  +
  facet_wrap(~region_name)+
  theme_tufte(ticks=F) +
  scale_x_discrete(name = 'Goal Quality Score (0 = low quality, 3 = satisifies all 3 criterion)',labels=c('Poor','1','2','Strong')) +
  scale_y_continuous(name = '# Goals') +
  scale_fill_viridis(direction = -1,name = 'Attrition',discrete=TRUE) +
  theme(#axis.text.y=element_blank(),
    strip.text = element_text(size=16),
    axis.title = element_text(size=16),
    axis.text = element_text(size=14),
    legend.position = c(0.7,.16),
    legend.text = element_text(size=14),
    legend.title = element_text(size=14),
    legend.background = element_rect(fill = 'grey80',colour = 'grey80'))



?geom_jitter

ggplot(response_and_att,
       aes(x=slopes,y=spec_mean)) + 
  theme_tufte(ticks=F)  + geom_point()

  scale_x_continuous(name = 'Problem Severity (% Projected Increase in Need or % Impaired [Quality])')+
  scale_y_continuous(name = '# BMPs adopted')+
  geom_smooth(aes(x=Pressure,y=bmp_count,group='black'),
              method='lm',lty=2,se=F,col='black')+
  geom_point(aes(fill=Attrition),shape = 21,size=4) + 
  scale_fill_viridis(direction = -1,name = 'Attrition',discrete=T)+#,labels=c('High','Medium','Low')) +
  theme(#axis.text.y=element_blank(),
    strip.text = element_text(size=16),
    axis.title = element_text(size=16),
    axis.text = element_text(size=14),
    legend.position = c(0.91,.35),
    legend.text = element_text(size=14),
    legend.title = element_text(size=14),
    legend.background = element_rect(fill = 'grey80',colour = 'grey80'))






