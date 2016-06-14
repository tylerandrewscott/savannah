getwd()

library(foreign)
library(sp)
library(rgdal)
huc8 = readOGR("../duckabush","WBDHU8")


head(huc8@data)


huc8.se = huc8[grep('TN|KY|GA|MS|FL|SC|NC|AL',huc8@data$States),]


dlist = read.csv('input/reg4.303d.list.csv')
head(dlist)

table(data.frame(table(dlist$Listed.Water.ID))[,2])


test =
table(dlist$Cycle)
rm(test)



