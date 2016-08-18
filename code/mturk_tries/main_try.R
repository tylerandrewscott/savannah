rm(list=ls())
source('code/customBulkCreateFromHitLayout.R')
library(devtools)
install_github('cloudyr/MTurkR',force=T)
require('MTurkR')


library(dplyr)

id_from_gui = '3I9SL54PYX9Z0MU1JADJ857RRF9K95'




Sys.setenv(AWS_ACCESS_KEY_ID = read.csv('../../../TScott/mturk/credentials.csv',stringsAsFactors = F)[,2],
           AWS_SECRET_ACCESS_KEY = read.csv('../../../TScott/mturk/credentials.csv',stringsAsFactors = F)[,3])
AccountBalance()

dat <- read.csv('code/plan_goals.csv',stringsAsFactors=F) %>% select(Wording)
#dat = head(dat,2)

past_workers = read.csv('../../Downloads/User_389369_workers.csv')


#setnewqual1
#3AATNUS1413EUVZMN79L1CUFPQX9J9
setnewqual1 <-
  CreateQualificationType(
    name="Auto-Granted Qualification to Prevent Worker Retakes",
    description="This qualification is for people who have worked for me on this task before. It is granted automatically to new workers.",
    status = 'Active',
    keywords="Worked for me before",
    auto = TRUE,
    auto.value = 100)

#> setnewqual1$QualificationTypeId
#[1] 3AATNUS1413EUVZMN79L1CUFPQX9J9

#> setnewqual2$QualificationTypeId
#[1] 38T2WS02O0B6SQJUXCIGZE420DYTUP

setnewqual3 <-
  CreateQualificationType(
    name="Auto-Granted Qualification",
    description="This is granted automatically.",
    status = 'Active',
    keywords="auto-granted",
    auto = TRUE,
    auto.value = 100)

#> setnewqual3$QualificationTypeId
#[1] 36MND4DYOMHH71SNB07CRPI1CTZ6YQ

#QualificationType Created: 36MND4DYOMHH71SNB07CRPI1CTZ6YQ

#QualificationType Created: 38T2WS02O0B6SQJUXCIGZE420DYTUP

w <- past_workers$Worker.ID # a vector containing WorkerIds
w50 <- unique(first11_results$WorkerId)

AssignQualification(
  qual = setnewqual1$QualificationTypeId,
  workers = w,
  value = "50")

AssignQualification(
  qual = setnewqual2$QualificationTypeId,
  workers = w50,
  value = "50")

AssignQualification(
  qual = setnewqual3$QualificationTypeId,
  workers = w50,
  value = "50")

AssignQualification(
  qual = setnewqual3$QualificationTypeId,
  workers = w50,
  value = "50")

w50_andAlready = c(
as.character(w50),as.character(unique(results_df$WorkerId)))


AssignQualification(
  qual = setnewqual3$QualificationTypeId,
  workers = w50,
  value = "50")


AssignQualification(
  qual = setnewqual3$QualificationTypeId,
  workers = w50_andAlready,
  value = "50")


qual_req <- GenerateQualificationRequirement(c("Locale","Approved",
                            as.character("3AATNUS1413EUVZMN79L1CUFPQX9J9")),
                            c("In",">","=="),
                            c("US-GA,US-FL,US-NC,US-AL,US-MS,US-SC,US-TN","90","100"),
                                         preview = TRUE)

qual_req_no50 <- GenerateQualificationRequirement(c("Locale","Approved",
             as.character(setnewqual3$QualificationTypeId)),
               c("In",">","=="),
             c("US","90","100"),
            preview = TRUE)


qual_req_no50_noAgain <- GenerateQualificationRequirement(c("Locale","Approved",
                     as.character(setnewqual3$QualificationTypeId)),
                                                          c("In",">","=="),
                                                          c("US","90","100"),
                                                          preview = TRUE)



qual_req_nb <- GenerateQualificationRequirement(c("Locale","Approved"),
                                             c("In",">"),
                                             c("US-GA,US-FL,US-NC,US-AL,US-MS,US-SC,US-TN","90"),
                                             preview = TRUE)



a <- data.frame(Wording = dat$Wording,stringsAsFactors = F)
aa = tail(a,4)
BulkCreateFromHITLayout
h_test = BulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('test_update',2),
  title = 'Categorize a goal statement',
  input = a,
  keywords = "categorization, category, statements",
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.05',assignments="2",
  expiration = seconds(days = 4),
  qual.req = qual_req)

status(hit.type = h_test[[1]]$HITTypeId)

h_full_v2 = customBulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('full_nb_2',2),
  title = 'Categorize a goal statement',
  input = a,
  keywords = "categorization, category, statements",
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.07',assignments="9",
  expiration = seconds(days = 4),
  qual.req = qual_req_no50)

#"35WEVPXY1V0MWCJLNWK2X1X5YW25J2"

h_full_v2_last4 = customBulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('full_nb_2',2),
  title = 'Categorize a goal statement',
  input = tail(a,4),
  keywords = "categorization, category, statements",
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.07',assignments="9",
  expiration = seconds(days = 4),
  qual.req = qual_req_no50)

temp = status(hit.type = '35WEVPXY1V0MWCJLNWK2X1X5YW25J2')
do.call('rbind',h_full_v2_last4)$HITId
h_full_v2
h_full_add6 = customBulkCreateFromHITLayout(
  hitlayoutid = id_from_gui,
  annotation = rep('full_nb_add6',2),
  title = 'Categorize a goal statement',
  input = a,
  keywords = "categorization, category, statements",
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  auto.approval.delay = seconds(days = 2),
  reward = '0.07',assignments="6",
  expiration = seconds(days = 4),
  qual.req = qual_req_no50_noAgain)


status('35WEVPXY1V0MWCJLNWK2X1X5YW25J2')
      '35WEVPXY1V0MWCJLNWK2X1X5YW25J2'
unique(results_df$WorkerId)

accountbalance()


AccountBalance()

GetAssignment(annotation = 'full_nb_2')
GetAssignments(annotation = 'full_nb_add6')

add6 = do.call('rbind',h_full_add6)
get6 = GetAssignments(hit = add6$HITId)
add6$Question = a[,1]

assi9 = rbind(do.call('rbind',h_full_v2),
do.call('rbind',h_full_v2_last4))
assi9$Question = a[,1]
results_df = GetAssignments(hit.type = '35WEVPXY1V0MWCJLNWK2X1X5YW25J2')
all15_HITs = rbind(assi9,add6)


test = full_join(results_df,get6)
test$Question = all15_HITs$Question[match(test$HITId,all15_HITs$HITId)]
all15_HITs$Order = rep(1:78,2)
goal_df = read.csv('code/plan_goals.csv',stringsAsFactors=F)
goal_df$Order = 1:78
test$Order = all15_HITs$Order[match(test$HITId,all15_HITs$HITId)]



sums = test %>% group_by(Order,Council) %>% summarise(YES_Answers = sum(Category=='YES it does'))

head(sums)






library(ggplot2)
ggplot(test,aes(x=))

tem = test %>% group_by(Question,Council)
test$Question
t1 = tem %>% summarise(sum(Category=='YES it does'))
t2 = tem %>% summarise(sum(Category=='NO it does not'))

t1$`sum(Category == "YES it does")` + t2$`sum(Category == "NO it does not")`




test[test$HITId=='302OLP89DZ8C5DGDE8PGYT73DJXCAX',]

summarize(Category,n())

?summarize
head(goal_df)

library(dplyr)




table(test$HITId)
as.data.frame(table(paste(test$WorkerId,test$HITId)))

test$HITTypeId
table(test$WorkerId)



test[duplicated(paste(test$WorkerId,test$HITId)),]



table(paste(get6$WorkerId,get6$HITId) %in% 
        paste(results_df$WorkerId,get6$HITId))

MTurkR::ExtendHIT(annotation = 'full_nb_2', #hit.type = '35WEVPXY1V0MWCJLNWK2X1X5YW25J2',
                  add.assignments = "1")

head(results_df)

h_full_v2[[1]]$HITTypeId

0.07 * 78 * 9
0.07 * 78 * 1

5 * 0.07 * 9



h_full_1_11 = h_full

status(hit.type = "35WEVPXY1V0MWCJLNWK2X1X5YW25J2")

h_full_v2

#h_full 
#3VU3J1KUFBZEQC4AGMHX34B3M77GT0

# HITType Registered: 3TKAYVZVPJM5DY4FE9LDA4LFYBH82K
expire(hit.type = '3VU3J1KUFBZEQC4AGMHX34B3M77GT0')

tt = status(hit.type = '3VU3J1KUFBZEQC4AGMHX34B3M77GT0')


first11_results = GetAssignments(hit = tt$HITId)
head(tt)
ApproveAllAssignments(hit.type = '3VU3J1KUFBZEQC4AGMHX34B3M77GT0')
table(first11_results$HITId,first11_results$Category)


res = GetAssignments(hit.type = '35WEVPXY1V0MWCJLNWK2X1X5YW25J2')






first11_results$WorkerId %in% as.character(past_workers$Worker.ID)

done_already = GetReviewableHITs(hit.type = '3VU3J1KUFBZEQC4AGMHX34B3M77GT0')


ApproveAllAssignments(hit = done_already$HITId)


done_already$HITId

original_hit = do.call('rbind',h_full)

original_hit$HITId %in% done_already$HITId

reviewresults(hit = tt$HITId)

reviewresults(hit = tt$HITId)


ExpireHIT(hit.type = '3TKAYVZVPJM5DY4FE9LDA4LFYBH82K')
ExpireHIT(annotation = 'full_nb_7-18')


status


MTurkR::status(hit.type = '3TKAYVZVPJM5DY4FE9LDA4LFYBH82K')
stat = status(hit.type = '3TKAYVZVPJM5DY4FE9LDA4LFYBH82K')
tt = stat %>% filter(RequesterAnnotation=='Batch1wqual')
table(tt$NumberOfAssignmentsAvailable)
table(tt$NumberOfAssignmentsCompleted)

extend(annotation = 'Batch1wqual',add.seconds = seconds(days=3))

head(tt)
stat$NumberOfAssignmentsAvailable
head(stat)
dim(stat)

rr = stat %>% filter(RequesterAnnotation!='Batch1wqual')
tt$HITStatus
rr$HITStatus


keywords = "categorization, category, "

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

GetAssignments(hit.type = '3XMQ3UBY137QYGRAX53V2KHS3KFLZR')




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
temp = status(hit.type='33GCHQ2CVOZP2A8G0GPA8YJNP81ORI')





