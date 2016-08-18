library(sp)
library(Hmisc)
library(rvest)
library(ggthemes)
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
library(dplyr)


cwa_2010 = readOGR('shape/Y2010_305b303d/','Y2010_305b303d_Streams_CoastalStreams_Beaches')
counties = readOGR('shape/nrcs_county','county_nrcs_a_ga')
base = 'http://ga.water.usgs.gov/waterplanning/regions/'
regions = c('ALT','CGA','CNG','LFO','MCH','MOC','SSA','SUO','UPF','UPO')
suffix1 = rep('/data/barbycounty.html',10)
suffix2 = rep('/data/barbycountynopt.html',10)
pages = lapply(as.list(paste0(base,tolower(regions),suffix1)),
               function(i) try(read_html(i),TRUE))
page_tables = lapply(pages,function(i)
  i %>% html_node('table') %>% html_node('table') %>%
    html_table())

county_list = sapply(page_tables,function(x) x[,1][x[,1]!='Total'],
                     simplify = TRUE)

region_county$Region = gsub('UPF','UFL',region_county$Region)
region_county$Region = gsub('UPC','UOC',region_county$Region)

region_county = lapply(1:length(regions),function(x)
  data.frame(County = county_list[[x]],Region = regions[x]))
region_county = do.call('rbind',region_county)
mng_counties = data.frame(County = c(
  'Bartow', 'Cherokee', 'Clayton', 'Cobb', 'Coweta', 'DeKalb', 'Douglas', 'Fayette', 'Forsyth',
  'Fulton', 'Gwinnett', 'Hall', 'Henry', 'Paulding', 'Rockdale'),Region = 'MNG')
region_county = rbind(region_county,mng_counties)
counties@data$Planning_Region = region_county$Region[match(counties@data$COUNTYNAME,region_county$County)]

# Now the dissolve
region <- gUnaryUnion(counties, id = counties@data$Planning_Region)
cwa_df = fortify(cwa_2010)
cwa_2010@data$id = as.character(0:(nrow(cwa_2010)-1))
cwa_df = dplyr::left_join(cwa_df,cwa_2010@data)
region_df = fortify(region)



#'#009E73','#D55E00','#F0E442'


river_map = ggplot() + 
#  geom_path(aes(x=long,y=lat,group=group),data=cwa_df,col='grey') +
  geom_polygon(aes(x=long,y=lat,group = group),
               data=region_df,fill='grey90',colour = 'grey10') +
  geom_path(aes(x=long,y=lat,group=group,fill=Evaluation,colour=Evaluation),
            data=cwa_df,alpha = 0.9,lwd=2)+
  theme_tufte(ticks=F) +
  scale_colour_manual(name = 'Status',values = 
                        c('#F0E442','#D55E00','#009E73'),
                      labels = c('Pending','Not Supporting','Supporting')) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill=alpha('grey70',0.7)),
        legend.position = c(0.8,0.8),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

region_crop = lapply(1:length(region),function(x) crop(cwa_2010,region[x,]))


sum_by_region = sapply(region_crop, 
                       function(x) x@data %>% group_by(Evaluation) %>% 
                         summarise(Total_Length_M = sum(Stream_Ext)),simplify=F)

sum_by_region = lapply(1:length(sum_by_region),function(x) 
  sum_by_region[[x]] %>% mutate(Region = names(region)[x]))

region_sums = do.call('rbind',sum_by_region)

total_length_by_reg = region_sums %>% group_by(Region) %>% 
  summarise(Total_Stream_Length = sum(Total_Length_M))

region_sums = left_join(region_sums,total_length_by_reg)

region_sums = region_sums %>% mutate(Proport_Status = Total_Length_M/Total_Stream_Length)

# ggplot(region_sums,aes(x=Region,y = Proport_Status,fill=Evaluation)) + 
#   geom_bar(stat = 'identity',position = 'dodge') + 
#   scale_y_continuous(name = 'Proportion of overall stream length',limits=c(0,1)) + 
#   theme_tufte(ticks=F)  +
#   scale_fill_manual(name = 'Status',values = 
#                         c('#F0E442','#D55E00','#009E73'),
#                       labels = c('Pending','Not Supporting','Supporting')) +
#   theme(legend.position = c(0.8,0.2))+
#   coord_flip()

region_df$Region = region_df$id
region_df = left_join(region_df,region_sums)

library(viridis)

require(gridExtra)



choro_support = ggplot(data=region_df %>% filter(Evaluation == 'Supporting')) + 
  #  geom_path(aes(x=long,y=lat,group=group),data=cwa_df,col='grey') +
  geom_polygon(aes(x=long,y=lat,group = group,
                   fill = Proport_Status)) + theme_tufte(ticks=F) +
  scale_fill_viridis('Supporting',option = 'viridis')+
  theme(
    axis.text=element_blank(),
    axis.title=element_blank())

choro_notsupport = ggplot(data=region_df %>% filter(Evaluation == 'Not Supporting')) + 
  #  geom_path(aes(x=long,y=lat,group=group),data=cwa_df,col='grey') +
  geom_polygon(aes(x=long,y=lat,group = group,
                   fill = Proport_Status))+ theme_tufte(ticks=F) +
  scale_fill_viridis(
    name = 'Proportion \nNot Supporting')+
  theme(
    axis.text=element_blank(),
    axis.title=element_blank(),
    legend.position = c(0.8,0.75))

choro_pending = ggplot(data=region_df %>% filter(Evaluation == 'Assessment Pending For')) + 
  #  geom_path(aes(x=long,y=lat,group=group),data=cwa_df,col='grey') +
  geom_polygon(aes(x=long,y=lat,group = group,
                   fill = Proport_Status)) + theme_tufte(ticks=F) +
  scale_fill_viridis('Status Pending',option = 'inferno') +
  theme(
    axis.text=element_blank(),
    axis.title=element_blank())
    
name_ref = data.frame(region_name = 
                        c('Altamaha','Coastal Georgia','Coosa-North Georgia',
                          'Lower Flint-Ochlockonee','Middle Chattahoochee',
                          'Middle Ocmulgee','Suwanee-Satilla','Savannah-Upper Ogeechee',
                          'Upper Flint','Upper Oconee','Metro North Georgia'),
                      Region = unique(region_sums$Region))

region_sums = left_join(region_sums,name_ref)


temp = region_sums %>% filter(Region != 'MNG')


ggplot(data=temp) +
  geom_bar(aes(x=region_name,y = Proport_Status,fill=Evaluation),
           stat='identity',position = 'dodge') +
 theme_tufte(ticks=F)  +
  scale_y_continuous(limits=c(0,1),expand=c(0,0),
                     labels=c(0,0.25,0.50,0.75,''),
         name = 'Proportion of stream length in impaired status')+
  coord_flip()+
theme(axis.title.y = element_blank(),
      legend.position = c(0.8,0.25),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      axis.text = element_text(size=14),
      axis.title = element_text(size=16)) +
    scale_fill_manual(name = 'Status',values = 
       c('#F0E442','#D55E00','#009E73'),
   labels = c('Pending','Not Supporting','Supporting'))
  

not_sup = temp %>% filter(Evaluation == 'Not Supporting') 
sup = temp %>% filter(Evaluation == 'Supporting') 

not_sup$Evaluation = as.character(not_sup$Evaluation)
not_sup$Evaluation = ifelse(not_sup$Evaluation=='Not Supporting','1.Not','2.Pending')

ggplot() + 
  geom_bar(stat='identity',
           data=not_sup, aes(x=region_name,
                                 y=-(Proport_Status),
                             fill = 'Not Supporting')) +
  geom_bar(stat='identity',
           data=sup, aes(x=region_name,
                             y=Proport_Status,fill = 'Supporting'))+
  coord_flip()  + theme_tufte() +
  scale_y_continuous(
    name = 'Proportion of total stream length with status',
    breaks = c(-0.5,0.0,0.5),limits = c(-0.8,.8),
    minor_breaks = seq(-0.5,.5,0.25),
    expand=c(0,0)) +
  theme(axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text = element_text(size=14),
        axis.title.x = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.position = c(0.88,0.11),
        panel.grid.minor = element_line(size=2)) +
  scale_fill_manual(values = c('#D55E00','#009E73'),
                    name = 'Supporting',labels=c('No','Yes'))


#write.csv(region_sums,'Input/region_cwa_summary.csv')






