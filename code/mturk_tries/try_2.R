require("MTurkR")

Sys.setenv(AWS_ACCESS_KEY_ID = 'AKIAJFKJQHZX3FZCOOJA',
           AWS_SECRET_ACCESS_KEY = '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')
AccountBalance()

q1 <- GenerateQualificationRequirement(c("Locale","Approved"),
                                       c("==",">"),
                                       c("US",90),
                                       preview = TRUE)

qf <- paste0(readLines('Google Drive/savannah/code/qualtest_statements.xml'), collapse="")
qa <- paste0(readLines('Google Drive/savannah/code/qualtest_answers.xml'), collapse="")

statement_qual <- CreateQualificationType(name = "Goal Statement Qualification",
                                 description = "pretest for categorization",
                                 test = qf,test.duration = 60,
                                 answerkey = qa,
                                 status = "Active",
                                 keywords = "test, autogranted")

q2 <- GenerateQualificationRequirement(c("Locale","Approved",statement_qual$QualificationTypeId),
                                       c("==", ">", "=="),
                                       c("US", 90, 100),
                                       preview = TRUE)

newhittype <-
  RegisterHITType(title = "Categorize an Image",
                  description = "Categorize an image according to its content",
                  reward = ".05",
                  duration = seconds(hours=1),
                  auto.approval.delay = seconds(days = 1),
                  qual.req = q2,
                  keywords = "categorization, image, coding, rating, sorting")

bulk <-
  BulkCreateFromHITLayout(template = f,
                          input = dat,
                          hit.type = newhittype$HITTypeId,
                          assignments = 3,
                          annotation = paste("Image Categorization", Sys.Date()),
                          expiration = seconds(days = 7))


a <- GenerateHITLayoutParameter(names = "hitvariable", 
                                values = "Text for HIT 1")
AccountBalance()

system.file("templates/qualificationtest1.xml", package = "MTurkR")

qa <- paste0(readLines(system.file("templates/answerkey1.xml", package = "MTurkR")), collapse="")
qual1 <- CreateQualificationType(test.duration = seconds(60),
                                 name = "Qualification with Test",
                                 description = "This qualification is a demo",
                                 test = qf,
                                 answerkey = qa,
                                 status = "Active",
                                 keywords = "test, autogranted")
q2 <- GenerateQualificationRequirement(c("Locale","Approved",qual1$QualificationTypeId),
                                       c("==", ">", ">"),
                                       c("US", 90, 50),
                                       preview = TRUE)

HITType ID: 3OT7QPGUOMZ7ZVEUB5HFPYC4Y5CIV7
             
Layout ID: 3VGQXUV9AET6FDN3QJIDV7TKRBKPRX

newhittype <- 
  RegisterHITType(title = "Categorize a goal statement",
                  description = "Categorize the specificity of a goal statement",
                  reward = ".05",
                  duration = seconds(hours=1),
                  auto.approval.delay = seconds(days = 1),
                 qual.req = q2,
                  keywords = "categorization, goal statement, specificity")

dat = read.csv('Google Drive/savannah/code/plan_goals.csv')
dat$Wording = as.character(dat$Wording)
library(dplyr)
dat = dat %>% select(Wording) %>% head(.,3)

dat = dat[1:3,]
# template file
f <- system.file("templates/categorization1.xml", package = "MTurkR")
# input data.frame
dat <- data.frame(imageurl = c("http://example.com/image1.png", "http://example.com/image1.png", "http://example.com/image1.png"),
             stringsAsFactors = FALSE)
AccountBalance()
GenerateHITLayoutParameter()
# retrieve HITLayoutID from Requester User Interface
layoutid <-  '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX'

# create/load data.frame of HITLayout variable values
b <- data.frame(
                Wording = dat$Wording,
                stringsAsFactors = FALSE)
b 
# create three HITs from the template

temp <- read.system.file("Google Drive/savannah/code/template.html", package = "MTurkR")
class(temp)
a <- data.frame(hittitle = c("HIT title 1", "HIT title 2", "HIT title 3"),
                hitvariable = c("HIT text 1", "HIT text 2", "HIT text 3"), 
                stringsAsFactors = FALSE)
BulkCreateFromTemplate(template = temp,
                       input = a,
                       annotation = paste("Bulk From Template", Sys.Date()),
                       title = "Categorize an image",
                       description = "Categorize this image",
                       reward = ".05",
                       expiration = seconds(days = 4),
                       duration = seconds(minutes = 5),
                       auto.approval.delay = seconds(days = 1),
                       keywords = "categorization, image, moderation, category")



hits4 <- 
  BulkCreateFromHITLayout(hitlayoutid = layoutid,
                          input = b,
                          annotation = paste("Bulk From Layout", Sys.Date()),
                          title = "Categorize Goal Statements",
                          description = "Assess this goal statement",
                          reward = ".05",
                          expiration = seconds(days = 4),
                          duration = seconds(minutes = 5),
                          keywords = "categorization, goal statements, category")

# cleanup
ExpireHIT(annotation = paste("Bulk From Layout", Sys.Date()))
DisposeHIT(annotation = paste("Bulk From Layout", Sys.Date()))


bulk <- BulkCreateFromHITLayout(hitlayoutid = '3VGQXUV9AET6FDN3QJIDV7TKRBKPRX',
                                input = dat,
                                title - 'Categorize Goal Statements',
                                expiration = seconds(days = 7),
                                annotation = paste("Goal Categorization", Sys.Date()))


                          input = dat,
                          hit.type = newhittype$HITTypeId,
                          assignments = 3,
                          annotation = paste("Goal Categorization", Sys.Date()),
                          expiration = seconds(days = 7))



gdf = read.csv('Downloads/plan_goals - Sheet1 (1).csv')


bulk <-
  BulkCreateFromHITLayout(hitlayoutid = "3VGQXUV9AET6FDN3QJIDV7TKRBKPRX",
                          input = gdf$Wording[1:2],
                          assignments = 10,
                          annotation = paste("Statement Categorization", Sys.Date()),
                          hit.type ="3OT7QPGUOMZ7ZVEUB5HFPYC4Y5CIV7",
                          expiration = seconds(days = 7))


                          
                          sandbox=T,
                        
     )

newhittype$HITTypeId


newHIT = CreateHIT(
  # layoutid in sandbox:
  hitlayoutid="3ERO2AY6W7MHAQP5TC10ZO4Q9575PC",
  #wording = 'Improve water quality',
 hit.type="37BTIVOV5AQPEV4OUJKY69WO5OEKJ0",
 question=NULL,
  sandbox=T,expiration=seconds(days=7))
  
If using method 4 (HITLayoutId), question should be NULL in
CreateHIT and the hitlayoutid argument should be specified instead.

?credentials()
qualReqs <- GenerateQualificationRequirement(c("Locale","Approved"),
                                             c("==",">"),
                                             c("US",90),
                                             preview = TRUE)

# Create new batch of hits:
newHIT = CreateHIT(
  # layoutid in sandbox:
  hitlayoutid="3ERO2AY6W7MHAQP5TC10ZO4Q9575PC",
  sandbox=T,
  # layoutid in production:
  # hitlayoutid="2C9X7H57DZKPHWJIU98DS25L8N41BW",
  annotation = "HET Experiment with Pre-Screen",
  assignments = "1200",
  title="Rate this hypothetical representative",
  description="It's easy, just rate this
  hypothetical representative on how well
  she delivers funds to his district",
  reward=".50",
  duration=seconds(hours=4),
  expiration=seconds(days=7),
  auto.approval.delay=seconds(days=15),
  qual.reqs=qualReqs
)




system.file("templates/qualificationtest1.xml", package = "MTurkR")
qf <- paste0(readLines(system.file("template/qualificationtest1.xml", package = "MTurkR")), collapse="")
qa <- paste0(readLines(system.file("template/answerkey1.xml", package = "MTurkR")), collapse="")
qual1 <- CreateQualificationType(name = "Qualification with Test",
                                 description = "This qualification is a demo",
                                 test = qf,
                                 answerkey = qa,
                                 status = "Active",
                                 keywords = "test, autogranted")

q2 <- GenerateQualificationRequirement(c("Locale","Approved",qual1$QualificationTypeId),
                                       c("==", ">", ">"),
                                       c("US", 90, 50),
                                       preview = TRUE)
