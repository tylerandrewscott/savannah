rm(list=ls())
source('code/customBulkCreateFromHitLayout.R')
library(devtools)
#install_github('cloudyr/MTurkR',force=T)
require('MTurkR')


library(dplyr)

id_from_gui = '3I9SL54PYX9Z0MU1JADJ857RRF9K95'




Sys.setenv(AWS_ACCESS_KEY_ID = read.csv('../../../TScott/mturk/credentials.csv',stringsAsFactors = F)[,2],
           AWS_SECRET_ACCESS_KEY = read.csv('../../../TScott/mturk/credentials.csv',stringsAsFactors = F)[,3])
AccountBalance()

dat <- read.csv('code/plan_goals.csv',stringsAsFactors=F) %>% select(Wording)
#dat = head(dat,2)

past_workers = read.csv('../../Downloads/User_389369_workers.csv')

setnewqual1 <-
  CreateQualificationType(
    name="Auto-Granted Qualification to Prevent Worker Retakes",
    description="This qualification is for people who have worked for me on this task before. It is granted automatically to new workers.",
    status = 'Active',
    keywords="Worked for me before",
    auto = TRUE,
    auto.value = 100)

w <- past_workers$Worker.ID # a vector containing WorkerIds
AssignQualification(
  qual = setnewqual1$QualificationTypeId,
  workers = w,
  value = "50")

qual_req <- GenerateQualificationRequirement(c("Locale","Approved",
                            as.character(setnewqual1$QualificationTypeId)),
                            c("In",">","=="),
                            c("US-GA,US-FL,US-NC,US-AL,US-MS,US-SC,US-TN","90","100"),
                                         preview = TRUE)

qual_req_nb <- GenerateQualificationRequirement(c("Locale","Approved"),
                                             c("In",">"),
                                             c("US-GA,US-FL,US-NC,US-AL,US-MS,US-SC,US-TN","90"),
                                             preview = TRUE)

a <- data.frame(Wording = dat$Wording)
aa = tail(a,2)


h_test = customBulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('Batch1wqual',2),
  title = 'Categorize a goal statement',
  input = a,
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.',assignments="9",
  expiration = seconds(days = 4),
  qual.req = qual_req)
# HITType Registered: 3TKAYVZVPJM5DY4FE9LDA4LFYBH82K

MTurkR::status(hit.type = '3TKAYVZVPJM5DY4FE9LDA4LFYBH82K')
stat = status(hit.type = '3TKAYVZVPJM5DY4FE9LDA4LFYBH82K')


stat$NumberOfAssignmentsAvailable
head(stat)
dim(stat)
tt = stat %>% filter(RequesterAnnotation=='Batch1wqual')
rr = stat %>% filter(RequesterAnnotation!='Batch1wqual')
tt$HITStatus
rr$HITStatus




stat$RequesterAnnotation
stat$HITStatus

h_test_2 = customBulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('Test11_12',nrow(aa)),
  title = 'Categorize a goal statement',
  input = aa,
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.10',assignments="9",
  expiration = seconds(days = 4),
  qual.req = qual_req)
#HITType Registered: 3XMQ3UBY137QYGRAX53V2KHS3KFLZR
status(hit.type = '3XMQ3UBY137QYGRAX53V2KHS3KFLZR')


h_test_3 = customBulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('Test11_12_nb',nrow(aa)),
  title = 'Categorize a goal statement',
  input = aa,
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.08',assignments="9",
  expiration = seconds(days = 4),
  qual.req = qual_req_nb)

#HITType Registered: 33GCHQ2CVOZP2A8G0GPA8YJNP81ORI
status(hit.type='33GCHQ2CVOZP2A8G0GPA8YJNP81ORI')



#expire(hit.type = '3TKAYVZVPJM5DY4FE9LDA4LFYBH82K')

MTurkR::
