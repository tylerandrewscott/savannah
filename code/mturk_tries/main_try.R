require('MTurkR')
library(devtools)
#install_github('MTurkR','cloudyr')
library(MTurkR)

Sys.setenv(AWS_ACCESS_KEY_ID = 'AKIAJFKJQHZX3FZCOOJA',
           AWS_SECRET_ACCESS_KEY = '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')
AccountBalance()


layoutid = '38HEUFBIL3DG7BWBDN8IGLTU7O70TG'
hittype = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D'
CreateHIT(question=NULL,hitlayoutid = layoutid)

u = 14
l = 19


q1 <- GenerateQualificationRequirement(c("Locale","Approved"),
                                       c("==",">"),
                                       c("US",90),
                                       preview = TRUE)
## CreateHIT using HITLayout from MTurk Requester User Interface ##
hit4 <- CreateHIT(hit.type ='3940TVN5B550T63BEA5A9RK4AJ8OE2',
               hitlayoutid = '3I9SL54PYX9Z0MU1JADJ857RRF9K95',
               assignments='10',
               qual.req = q1,
                expiration = seconds(days = .15),
               hitlayoutparameters = a)     

aa = data.frame(names = rep('Wording',4),Wording = dat$Wording[1:4])
inputvalues <- 
  data.frame(Wording = dat$Wording[1:4],stringsAsFactors = F)

inputvalues[] <- lapply(inputvalues, as.character)

for (i in 1:nrow(inputvalues)){print(i)}
BulkCreateFromHITLayout
a <- GenerateHITLayoutParameter(rep("Wording",length(dat$Wording[1:4])), 
                                dat$Wording[1:4])
inputvalues
unlist(inputvalues[3, , drop = TRUE])
unlist(inputvalues[2, , drop = TRUE])
# initialize a HITId variable:
#inputvalues$HITId <- NA
inputvalues <- 
  data.frame(hitvar1 = c("Input for HIT 1 for var1","Input for HIT 2 for var1","Input for HIT 3 for var1"),
             hitvar2 = c("Input for HIT 1 for var2","Input for HIT 2 for var2","Input for HIT 3 for var2"),
             hitvar3 = c("Input for HIT 1 for var3","Input for HIT 2 for var3","Input for HIT 3 for var3"))

h = BulkCreateFromHITLayout( 
  hitlayoutid = '3I9SL54PYX9Z0MU1JADJ857RRF9K95',
  annotation = paste("Bulk From Layout", Sys.Date()),
  title = 'Categorize a goal statement',
  input = a,
  description = 'Categorize this goal statement',
  duration = seconds(minutes = 5),
  reward = '0.04',
  expiration = seconds(days = 4)
  )
BulkCreateFromHITLayout
GenerateHITLayoutParameter
temp <- system.file("templates/htmlquestion2.xml", package = "MTurkR")

names(inputvalues)

GenerateHITLayoutParameter(rep('Wording',4),dat$Wording[1:4])
inputvalues$HITId <- do.call(rbind, h)$HITId

b <- data.frame(hittitle = c("HIT title 1", "HIT title 2", "HIT title 3"),
                hitvariable = c("HIT text 1", "HIT text 2", "HIT text 3"), 
                stringsAsFactors = FALSE)
                         
                         verbose = T,
                         
                        
                        # hittype = '3940TVN5B550T63BEA5A9RK4AJ8OE2',
                         input = aa,
                         assignments='6',
                       # hitlayoutparameters = a,
                         expiration = seconds(days = .25)
                          )

aa


HITStatus(hit.type = "3H5SGK4YKNZK6DAXX7X12H2DMUT2GT")
HITStatus(BatchId:2448383)
reviewresults(hit = hit4$HITId)
HITStatus(hit = hit3$HITId)
GetAssignments(hit = hit4$HITId) 
GetReviewResultsForHIT(hit = hit4$HITId)

GetAssignments(hit.type = hit4$HITTypeId,annotation = "BatchId:2448383;")


GetAssignment(hit.type ='3940TVN5B550T63BEA5A9RK4AJ8OE2' )

HITStatus(hit='3HJ1EVZS2OKGRU6BNQ010GSJ7Y6R3W')
hit3$HITId
 HITStatus(hit.type='3940TVN5B550T63BEA5A9RK4AJ8OE2')
GetReviewableHITs(hit.type = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D' )
GetReviewResultsForHIT("3SBX2M1TKDO1XPAFAPGK5VAKPCX4QQ")
hit1$HITId
hit2$HITId
HITStatus(hit = hit1$HITId)
HITStatus(hit.type = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D')
GetAssignment(hit = hit1$HITId)             
GetAssignment(hit = hit2$HITId) 


GetAssignment(assignment = NULL, hit = NULL, hit.type = NULL,
              annotation = NULL, status = NULL,
              return.all = FALSE, pagenumber = "1", pagesize = "10",
              sortproperty = "SubmitTime", sortdirection = "Ascending",
              response.group = NULL, return.assignment.dataframe = TRUE,
              verbose = getOption('MTurkR.verbose', TRUE), ...)


#1. Develop an HTML form template to display and record data

#2. Optionally, develop a Qualification Test to select high-quality workers
q1 <- GenerateQualificationRequirement(c("Locale","Approved"),
                                       c("==",">"),
                                       c("US",90),
                                       preview = TRUE)
dat <- read.csv('Google Drive/savannah/code/plan_goals.csv',stringsAsFactors=F) %>% select(Wording)
dat = head(dat,7)
lp = GenerateHITLayoutParameter(names=rep("Wording",7),values= dat$Wording)
# create/load data.frame of HITLayout variable values


# first load credentials with `credentials()`
# create a dataframe of HITLayout parameters:
inputvalues <- 
  data.frame(Wording = dat$Wording)
# initialize a HITId variable:
#inputvalues$HITId <- NA
Sys.Date()
# Create each HIT:
h <- 
  BulkCreateFromHITLayout(hitlayoutid = layoutid,
                          input = inputvalues,
                          annotation = paste("Bulk From Layout", Sys.Date()),
                          title = "Categorize a statement",
                          description = "Categorize this statement",
                          reward = ".04",
                          expiration = seconds(days = 4),
                          duration = seconds(minutes = 5),
                          keywords = "categorization, moderation, category")

inputvalues$HITId <- do.call(rbind, h)$HITId


# Save the `inputvalues` dataframe:
save(inputvalues, file='inputvalues.RData')










b <- data.frame(Wording = dat$Wording, 
                stringsAsFactors = FALSE)

BulkCreateFromHITLayout(hitlayoutid = layoutid,
                        input = b,
                        annotation = paste("Bulk From Layout", Sys.Date()),
                        title = "Categorize a a statement",
                        description = "Categorize this goal statement",
                        reward = ".04",
                        expiration = seconds(days = 4),
                        duration = seconds(minutes = 5),
                        keywords = "categorization, moderation, category")


b
BulkCreateFromHITLayout(hitlayoutid = ,
          
                        expiration = seconds(days=4),verbose=T,
                        qual.req=q1,input=lp,
                        title = "Categorize A Goal Statement",
                        annotation = paste("Bulk From Layout", Sys.Date()),
                        validate.question=T,reward='0.04')

3YBKD8VAPND3U02YV1TBZSJW8DTR5D
q1 <- GenerateQualificationRequirement(c("Locale","Approved"),
                                       c("==",">"),
                                       c("US",90),
                                       preview = TRUE)
hit1 <- 
  CreateHIT( hitlayoutid = '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX',
             hit.type = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D',
             expiration = seconds(days=4),
             validate.question=T,
             reward = "0.10",
             qual.req=q1,verbose=T,
             hitlayoutparameters=lp)
status(hit1$HITId)

ExpireHIT(hit1)
  test = 
    GetAssignment(hit = hit1$HITId)
  
dim(test)

         
              
read_lines(system.file("templates/categorization1.xml", package = "MTurkR"))
#1. Develop an HTML form template to display and record data

#2. Optionally, develop a Qualification Test to select high-quality workers
q1 <- GenerateQualificationRequirement(c("Locale","Approved"),
                                       c("==",">"),
                                       c("US",90),
                                       preview = TRUE)
#3. Create a HITType (a display group) to organize the HITs you will produce
newhittype <-
  RegisterHITType(
    
                  description = "Categorize an goal statement according to its content",
                  reward = ".04",
                  duration = seconds(hours=1),
                  auto.approval.delay = seconds(days = 1),
                  qual.req = q1,
                  keywords = "categorization, coding, rating, sorting")

#4. Create the HITs from the template and an input data.frame

# template file
f <- 'Google Drive/savannah/code/statement_categorization.xml'

# input data.frame
library(dplyr)


# create the HITs (this is time consuming)
# create HIT using layout parameter
lp = GenerateHITLayoutParameter(names="Wording",values= dat$Wording)

q1 <- GenerateQualificationRequirement(c("Locale","Approved"),
                                       c("==",">"),
                                       c("US",90),
                                       preview = TRUE)
hit1 <- 
  CreateHIT( hitlayoutid = '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX',
             hit.type = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D',
             expiration = seconds(days=4),
             validate.question=T,
             reward = "0.10",
             qual.req=q1,verbose=T,
             hitlayoutparameters=lp)
hit1

dat
status(hit1)
status(hit = hit1$HITId)
ExpireHIT(hit1$HITId)
DisposeHIT(hit1$HITId)

DisposeHIT("3YBKD8VAPND3U02YV1TBZSJW8DTR5D")
DisposeHIT("3SBX2M1TKDO1XPAFAPGK5VAKPCX4QQ")
# create three HITs from the template
reviewing(hit1)

# retrieve HITLayoutID from Requester User Interface
layoutid <- '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX'

?CreateHIT()

# create/load data.frame of HITLayout variable values
b <- data.frame(hittitle = c("HIT title 1", "HIT title 2", "HIT title 3"),
                hitvariable = c("HIT text 1", "HIT text 2", "HIT text 3"), 
                stringsAsFactors = FALSE)
hits4 <- 
  BulkCreateFromHITLayout(hitlayoutid = layoutid,
                          input = b,
                          annotation = paste("Bulk From Layout", Sys.Date()),
                          title = "Categorize an image",
                          description = "Categorize this image",
                          reward = ".01",
                          expiration = seconds(days = 4),
                          duration = seconds(minutes = 5),
                          keywords = "categorization, image, moderation, category")


b <- data.frame(
                Wording = c("HIT text 1", "HIT text 2", "HIT text 3"), 
                stringsAsFactors = FALSE)

hits2 = BulkCreateFromHITLayout(hitlayoutid = '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX', input, annotation, verbose = FALSE, ...)
hits2 <- 
  BulkCreateFromHITLayout(hitlayoutid = '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX',
                     hit.type = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D',
                     expiration = seconds(days=4),reward = ".01",
                     duration = seconds(hours = 1),
                     hitlayoutparameters=lp,
                     annotation = paste("Bulk from file", Sys.Date()))

dat
?BulkCreate()
# Create HIT
hit2 <-
  CreateHIT(title = "Survey",
            description = "5 question survey",
            reward = ".10",
            expiration = seconds(days = 4),
            duration = seconds(hours = 1),
            keywords = "survey, questionnaire",
            assignment.review.policy = policyd,
            hit.review.policy = policye,
            question = GenerateHTMLQuestion(file = f))


# GetReviewResults
GetReviewResultsForHIT(hit1$HITId)
# cleanup
ExpireHIT(hit1$HITId)
DisposeHIT(hit1$HITId)


HITStatus(hit.type =  hit1$HITTypeId)



    title = "Survey",
            description = "5 question survey",
            reward = ".10",
            expiration = seconds(days=4),
            duration = seconds(hours = 1),
            keywords = "survey, questionnaire",
            # retrieved from MTurk web interface:
            hitlayoutid = "23ZGOOGQSCM61T1H5H9U0U00OQWFFU", 
            hitlayoutparameters = a)

a <- GenerateHITLayoutParameter(names = "Wording", 
                                values = "Text for HIT 1")


CreateHIT(hit.type=)
bulk <-
  BulkCreateFromHITLayout(#template = f,
                          input = dat,
                          hitlayoutid = '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX',
                          hitlayoutparameters  = lp,
                          hit.type = '3YBKD8VAPND3U02YV1TBZSJW8DTR5D',
                          assignments = 3,
                          annotation = paste("Statement Categorization", Sys.Date()),
                          expiration = seconds(days = 7))

#5. Monitor and retrieve results