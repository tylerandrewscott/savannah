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

#test = temp %>% filter(Council=='Upper Oconee'|Council=='Savannah - Upper Ogeechee')
library(statnet)
library(xergm)
inc_matrix = as.matrix(table(temp$full_name,temp$uq.event))
net_list = list()
  for (uq in sort(colnames(inc_matrix)))
  { net_list[[uq]] <- tcrossprod(inc_matrix[,uq])
  }

net_list = lapply(net_list,as.network,directed=F)


#m = btergm(net_list~edges+twopath+memory(type = "stability", lag = 1) ,R = 1000)


bip_net = as.network(inc_matrix,matrix.type='incidence',bipartite=T)
set.vertex.attribute(bip_net,'Meeting',value = temp$Meeting[match(network.vertex.names(bip_net),temp$uq.event)])
set.vertex.attribute(bip_net,'Council',value = temp$Council[match(network.vertex.names(bip_net),temp$uq.event)])
m2 = ergm(bip_net~edges+b2cov('Meeting')+ #b2factor('Council') + 
            gwb2degree(1,fixed=T) +
            gwb1degree(0.25,fixed=T))


dy_net = net_list[grepl('Lower Flint',names(net_list))]
dy_net = lapply(dy_net,as.network,bipartite=F,directed=F)
require(xergm)
model1 <- btergm(dy_net ~ edges + gwdegree(0.25,fixed=T)+ gwdsp(0.25,fixed=T) + gwesp(0.25,fixed=T), R = 1000)

dy_net[[2]]

m3 = ergm(bip_net~edges+edgecov(meet_mat)+ b2star(2))

summary(m2)
m1 = ergm(bip_net~edges+ gwb2degree(1,fixed=T))
library(stargazer)
library(texreg)
screenreg(list(m2))


attendance = temp %>% filter(is.na(Organization)) %>% group_by(Council,Meeting) %>% summarize(n = n())
attendance = left_join(attendance,attendance %>% summarise(avg = mean(n)))
nmem = roster %>% group_by(Planning.Council) %>% summarize(num_members = n()) %>%
  rename(Council = Planning.Council)
attendance = left_join(attendance,nmem)
temp = temp %>% mutate(member =  !is.na(Council.position) & Council.position == 'Council Member')
meetings_attended = temp %>% #filter(is.na(Organization)) %>%
  group_by(Council,full_name,member) %>% summarize(meetings_attended = n())

mheld = temp %>% group_by(Council) %>% summarize(meetings_held = length(unique(uq.event)))
meetings_attended = left_join(meetings_attended,mheld)%>% mutate(prop = meetings_attended/meetings_held)


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
  facet_wrap(~Council) + geom_abline(aes(intercept=num_members,slope=0,lty='d')) +
  scale_linetype_manual(name='',labels='# members',values = 2) +
  theme(legend.position = c(0.8,.2),legend.text=element_text(size=16),
        axis.title=element_text(size=16),axis.text=element_text(size=14))



test = temp %>% select(Council,full_name,Council.position)
unique(test$Council.position)


p3 = ggplot(meetings_attended,aes(x=meetings_attended,fill=)) + geom_bar() + 
  theme_tufte(ticks=F) +
facet_wrap(~Council) + scale_y_continuous(name='Individuals') + 
  scale_x_continuous(name='# meetings attended by individual') +
  scale_fill_colorblind(name='',labels=c('Board Member','Professional')) + 
  theme(legend.position = c(0.8,.2),legend.text=element_text(size=16),
        axis.title=element_text(size=16),axis.text=element_text(size=14))

p3b = ggplot(meetings_attended[!meetings_attended$prof,],
             aes(x=meetings_attended)) +
  geom_bar() + 
  theme_tufte(ticks=F) +
 # geom_density() +
  facet_wrap(~Council) + scale_y_continuous(name='Individuals') + 
  scale_x_continuous(name='# meetings attended by individual',breaks=c(1,4,7,10)) +
  scale_fill_colorblind(name='',labels=c('Board Member','Professional')) + 
  theme(legend.position = c(0.8,.2),legend.text=element_text(size=16),
        axis.title=element_text(size=16),axis.text=element_text(size=14),
        strip.text=element_text(size=14))


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
p4


meetings_attended %>% filter(member) %>% group_by(Council) %>% 
  summarize(avg.prop = round(mean(prop),2),med.prop = round(median(prop),2),
          prophalf = round(mean(prop>.5),2),class = ifelse(prophalf>.6,'high','low'))




meetings_attended %>% filter(member) %>% group_by(Council) %>% summarize(length(unique(full_name)))
test = meetings_attended %>% filter(Council=='Coosa - North Georgia',member)

roster = roster %>% mutate(full_name = paste(First,Last,sep=' '))

test$full_name

roster$full_name = gsub(' [A-Z]\\. ',' ',roster$full_name)
roster$full_name = gsub('\\,.*','',roster$full_name)

temp$full_name = gsub(' [A-Z]\\. ',' ',temp$full_name)
temp$full_name = gsub('\\,.*','',temp$full_name)



test = temp[!paste(temp$Last,temp$Council,sep='_') %in% 
paste(roster$Last,roster$Planning.Council,sep='_'),]


test %>% filter(Council.position=='Council Member')

grep('Hendrick',roster$Last,value=T)

table(just_members$Council,just_members$prop>.5)

20/26


roster = roster %>% mutate(full_name = paste(First,Last,sep=' ')) + 
temp = temp %>% mutate(uq.person.council = paste0(full_name,Council))

council_matrix = as.matrix(table(temp$full_name[!duplicated(temp$uq.person.council)],
                                 temp$Council[!duplicated(temp$uq.person.council)]))

council_shared = crossprod(council_matrix)

council_network = as.network(council_shared,directed=F,ignore.eval = F,names.eval = 'person',matrix.type='adjacency')
  library(igraph)
l <- layout.circle(intergraph::asIgraph(council_network))

p2

test = intergraph::asIgraph(council_network)
summary(test)


plot(test, vertex.label=
       network.vertex.names(council_network), vertex.size=3, edge.width=sqrt(E(test)$person))

library(ggnetwork)
council_network
ggnetwork::ggnetwork(council_network)
council_network

ggplot(data = ggnetwork(n), aes(x = x, y = y, xend = xend, yend = yend))
ggplot2::fortify(council_network)

library(ggplot2)
library(ggnetwork)

# Let’s define a small random graph to illustrate each component of ggnetwork:
library(network)
library(sna)


is.bipartite(intergraph::asIgraph(council_network))
ggnetwork::ggnetwork(council_network)
ggplot(data = ggnetwork(council_network), aes(x, y, xend = xend, yend = yend))+
  geom_edges() +
  geom_nodes() +
  theme_tufte(ticks=F) +
  theme(axis.text=element_blank(),axis.title=element_blank())

library(reshape2)
council_heat = council_shared %>% as.data.frame(.) %>%
  mutate(id = rownames(council_shared)) %>% melt(.) %>%
  mutate(value = ifelse(id==variable,NA,value))

library(viridis)
ggplot(council_heat,aes(x=id,y=variable,fill=value)) + geom_tile() +
  scale_fill_viridis(name='Common \nAttendees') +
  theme_tufte(ticks=F) +
  ylab('') + xlab('')


table(temp$Council,temp$Meeting)

names(council_heat)

%>% mel(.,id=id)

%>% tidyr::gather()


head(ggnetwork(council_network))
netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(council_shared[,10:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )


E(test)$person

plot(test,edge.width='person')
    yout=l,edge.width=3)

council_network



roster$full_name = gsub(' [A-Z]\\. ',' ',roster$full_name)
temp$full_name = gsub(' [A-Z]\\. ',' ',temp$full_name)


unique(temp$full_name[!temp$full_name %in% roster$full_name])


paste(roster$City[is.na(roster$Organization)],roster$County[is.na(roster$Organization)],sep=',')

roste

roster$Organization[is.na(roster$Organization)]

= roster$City[is.na(roster$Organization)]


temp %>% filter(is.na(Organization)) %>% mutate(uq_member = paste(Council,full_name)) %>% 
  filter(!duplicated(uq_member)) %>% group_by(Council) %>% summarize(n())



  
  group_by(Council) %>% summarise(n(unique(full_name)))




          +gwb2degree(0.25,fixed=T))
  b2mindegree(min(table(temp$uq.event))))


get.vertex.attribute(bip_net,'Meeting')


meet_mat = matrix(
  rep(temp$Meeting[match(network.vertex.names(bip_net),temp$uq.event,nomatch = 0)],each=nrow(inc_matrix)),ncol=ncol(inc_matrix),byrow=F)


nrow(inc_matrix)*ncol(inc_matrix)

rep(temp$Meeting[match(network.vertex.names(bip_net),temp$uq.event)],each=nrow(inc_matrix)))





rep(gsub('^ ','',gsub('[0-9]|-','',colnames(inc_matrix))),each=nrow(inc_matrix))



network(5, bipartite=10, density=0.1)

bip_net
inc_matrix
            
           
          

summary(m2)
mcmc.diagnostics(m2)
get.vertex.attribute(bip_net,'Council')
mults = temp %>% group_by(full_name) %>% summarize(app = n()) %>% filter(app>1)
xergm.common::gof(m2)
temp$Organization[temp$full_name=='Alan Saxon']
search.ergmTerms('bipartite')
for (uq in mults$full_name)
{
  
}
ergm::search.ergmTerms('bipartite')



summary(m2)

get.vertex.attribute(bip_net,'Meeting')

network.vertex.names(bip_net)
mcmc.diagnostics(m2)
  dim(inc_matrix)

summary(model1)
require(xergm)
model1 <- btergm(observed_network ~ edges + b2star(2:3), R = 1000)
summary(model1)


warnings()

?btergm::btergm
xergm::
table(Arow)

sum(inc_matrix[,uq])



temp[temp$full_name=='Deatre Denion',][1:4,]
unique(temp$Date)
table(temp$uq.event)


rm(list=ls())
