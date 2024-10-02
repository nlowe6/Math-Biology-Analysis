#install these packages if you do not already have them!

library(tidyverse)
library(janitor)
library(HH)
library(colorspace)
library(janitor)

## Career stats
dataFile <- readxl::read_excel("MBVI_Brown_Weigel_Aikens_Historic_ GTSubset.xlsx")
dataFilePre <- filter(dataFile, pre.post == 'Pre')
dataFilePost <- filter(dataFile, pre.post == 'Post')
dataFile <- clean_names(dataFile)
cleanGPA <- str_remove_all(deframe(dataFile['gpa']), '[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]')
cleanGPA <- as.numeric(cleanGPA)
cleanGPA <- round_to_fraction(cleanGPA, 4)
dataFile['gpa'] <- cleanGPA
dataFile['gender'] <- str_replace_all(deframe(dataFile["gender"]), c("1" = 'male', '2' = 'female', '3' = 'other'))
career = dataFilePre[,33]
career[career == 'NA'] = NA
career[career < 6] <-  '1'
career[career > 5] <-  '0'
dataFilePre <- dataFilePre[12:21]
dataFilePost  <- dataFilePost[12:21]
dataFilePre[dataFilePre == "NA"] = NA
dataFilePre[dataFilePre == "IDK"] = NA
dataFilePre[dataFilePre == "PNA"] = NA
dataFilePre[dataFilePre == 11] = NA
dataFilePre[dataFilePre == 8] = NA
dataFilePre[dataFilePre == 4.5] = NA
dataFilePost[dataFilePost == "NA"] = NA
dataFilePost[dataFilePost == "IDK"] = NA
dataFilePost[dataFilePost == "PNA"] = NA
dataFilePre <- cbind(dataFilePre, career)
dataFilePre <- drop_na(dataFilePre)
dataFilePre <- as.tibble(dataFilePre)
pre_med <- filter(dataFilePre, pre.prof.1 == 1)
pre_prof <- filter(dataFilePre, pre.prof.1 == 0)
dataFilePre <- as.data.frame(dataFilePre)
dataFilePre <- apply(dataFilePre, c(1,2), as.numeric)
pre_med <- apply(pre_med, c(1,2), as.numeric)
pre_prof <- apply(pre_prof, c(1,2), as.numeric)
pre_med_cost <- pre_med[,8:10]
pre_prof_cost <- pre_prof[,8:10]
t.test(pre_med_cost,pre_prof_cost)
wilcox.test(pre_med_cost, pre_prof_cost)
pre_med_int <- pre_med[,1:3]
pre_prof_int <- pre_prof[,1:3]
t.test(pre_med_int, pre_prof_int)
wilcox.test(pre_med_int, pre_prof_int)
pre_med_uty <- pre_med[,4:7]
pre_prof_uty <- pre_prof[,4:7]
t.test(pre_med_uty, pre_prof_uty)
wilcox.test(pre_med_uty, pre_prof_uty)

#Gender Stats
dataFile <- readxl::read_excel("MBVI_Brown_Weigel_Aikens_Historic_ GTSubset.xlsx")
responses <- dataFile[12:21]
studentID <- dataFile[,5]
gender <- dataFile[,48]
combine <- cbind(gender, studentID)
combine[combine == 'NA'] = NA
cleanData <- as.data.frame(table(combine[,2]))
responses[responses == "NA"] = NA
responses[responses == "IDK"] = NA
responses[responses == "PNA"] = NA
responses[responses == 11] = NA
responses[responses == 8] = NA
responses[responses == 4.5] = NA
cleanData <- cbind(responses, combine)
cleanData <- drop_na(cleanData)
cleanData <- distinct(cleanData, student, .keep_all = T)
cleanData <- cleanData[1:11]
cleanData <- as.tibble(cleanData)
male <- filter(cleanData, gender == 1)
female <- filter(cleanData, gender == 2)
cleanData <- as.data.frame(cleanData)
cleanData <- apply(cleanData, c(1,2), as.numeric)
male <- apply(male, c(1,2), as.numeric)
female <- apply(female, c(1,2), as.numeric)
male_cost <- male[,8:10]
female_cost <- female[,8:10]
t.test(male_cost,female_cost)
wilcox.test(male_cost, female_cost)
male_int <- male[,1:3]
female_int <- female[,1:3]
t.test(male_int, female_int)
wilcox.test(male_int, female_int)
male_uty <- male[,4:7]
female_uty <- female[,4:7]
t.test(male_uty, female_uty)
wilcox.test(male_uty, female_uty)

##Year Stats

dataFile <- readxl::read_excel("MBVI_Brown_Weigel_Aikens_Historic_ GTSubset.xlsx")
responses <- dataFile[12:21]
studentID <- dataFile[,5]
year <- dataFile[,39]
combine <- cbind(year, studentID)
combine[combine == 'NA'] = NA
cleanData <- as.data.frame(table(combine[,2]))
responses[responses == "NA"] = NA
responses[responses == "IDK"] = NA
responses[responses == "PNA"] = NA
responses[responses == 11] = NA
responses[responses == 8] = NA
responses[responses == 4.5] = NA
cleanData <- cbind(responses, combine)
cleanData <- drop_na(cleanData)
cleanData <- distinct(cleanData, student, .keep_all = T)
cleanData <- cleanData[1:11]
cleanData <- as.tibble(cleanData)
first <- filter(cleanData, year == 1)
returning <- filter(cleanData, year != 1)
cleanData <- as.data.frame(cleanData)
cleanData <- apply(cleanData, c(1,2), as.numeric)
first <- apply(first, c(1,2), as.numeric)
returning <- apply(returning, c(1,2), as.numeric)
first_cost <- first[,8:10]
returning_cost <- returning[,8:10]
t.test(first_cost,returning_cost)
wilcox.test(first_cost, returning_cost)
first_int <- first[,1:3]
returning_int <- returning[,1:3]
t.test(first_int, returning_int)
wilcox.test(first_int, returning_int)
first_uty <- first[,4:7]
returning_uty <- returning[,4:7]
t.test(first_uty, returning_uty)
wilcox.test(first_uty, returning_uty)

## career vis
pre_med_int <- as.data.frame(table(pre_med[,1:3]))
pre_med_uty <- as.data.frame(table(pre_med[,4:7]))
pre_med_cost <- as.data.frame(table(pre_med[,8:10]))
int <- pre_med_int[,2]
uty <- pre_med_uty[,2]
cost <- pre_med_cost[,2]
len <-  length(pre_med[,1])
int <- as.tibble(t(int))/(len*3)
uty <- as.tibble(t(uty))/(len*4)
cost <- as.tibble(t(cost))/(len*3)
#numbers above are based on sample size * question count
test <- c('int', 'uty', 'cost')
test <- as.tibble(t(test))
test <- t(test)
new <- rbind(int,uty,cost)
new <- cbind(test,new)
pre_prof_int <- as.data.frame(table(pre_prof[,1:3]))
pre_prof_uty <- as.data.frame(table(pre_prof[,4:7]))
pre_prof_cost <- as.data.frame(table(pre_prof[,8:10]))
int1 <- pre_prof_int[,2]
uty1 <- pre_prof_uty[,2]
cost1 <- pre_prof_cost[,2]
len <- length(pre_prof[,1])
int1 <- as.tibble(t(int1))/(len*3)
uty1 <- as.tibble(t(uty1))/(len*4)
cost1 <- as.tibble(t(cost1))/(len*3)
#numbers above are based on sample size * question count
test <- c('int', 'uty', 'cost')
test <- as.tibble(t(test))
test <- t(test)
new <- rbind(int1,uty1,cost1)
new <- cbind(test,new)
test1 <- rbind(int, int1)
test2 <- rbind(uty, uty1)
test3 <- rbind(cost, cost1)
career <- c('pre_med', 'pre_prof')
career <- as.tibble(t(career))
career <- t(career)
test1 <- cbind(career, test1)
test2 <- cbind(career, test2)
test3 <- cbind(career, test3)
test4 <- rbind(test1,test2,test3)
test4 <- remove_rownames(test4)
construct <- t(t(c('Interest', 'Interest', 'Utility', 'Utility', 'Cost', 'Cost')))
test4 <- cbind(test4, construct)
colnames(test4) <- c('item', 'Strongly Agree', 'Agree', 'Slightly Agree', 'neutral', 'slightly Disagree', 'Disagree', 'Strongly Disagree', 'type')
test4 <- rev(test4)
likert(item~. | type, test4, as.percent = T, col = diverging_hcl(7, rev = T, palette = 'Blue-Red 2'), main = F, rightAxis = F, layout=c(1,3), ylab = 'Demographic', xlim = c(-50,100), scales = list(x=list(at=seq(-50,100,20), labels=c(seq(50,0,-20),seq(10,100,20)))), borders = list(col = 'WHITE'))


##gender vis

male_int <- as.data.frame(table(male[,1:3]))
male_uty <- as.data.frame(table(male[,4:7]))
male_cost <- as.data.frame(table(male[,8:10]))
int <- male_int[,2]
uty <- male_uty[,2]
cost <- male_cost[,2]
len <-  length(male[,1])
int <- as.tibble(t(int))/(len*3)
uty <- as.tibble(t(uty))/(len*4)
cost <- as.tibble(t(cost))/(len*3)
#numbers above are based on sample size * question count
test <- c('int', 'uty', 'cost')
test <- as.tibble(t(test))
test <- t(test)
new <- rbind(int,uty,cost)
new <- cbind(test,new)
female_int <- as.data.frame(table(female[,1:3]))
female_uty <- as.data.frame(table(female[,4:7]))
female_cost <- as.data.frame(table(female[,8:10]))
int1 <- female_int[,2]
uty1 <- female_uty[,2]
cost1 <- female_cost[,2]
len <- length(female[,1])
int1 <- as.tibble(t(int1))/(len*3)
uty1 <- as.tibble(t(uty1))/(len*4)
cost1 <- as.tibble(t(cost1))/(len*3)
#numbers above are based on sample size * question count
test <- c('int', 'uty', 'cost')
test <- as.tibble(t(test))
test <- t(test)
new <- rbind(int1,uty1,cost1)
new <- cbind(test,new)
test1 <- rbind(int, int1)
test2 <- rbind(uty, uty1)
test3 <- rbind(cost, cost1)
gender <- c('male', 'female')
gender <- as.tibble(t(gender))
gender <- t(gender)
test1 <- cbind(gender, test1)
test2 <- cbind(gender, test2)
test3 <- cbind(gender, test3)
test4 <- rbind(test1,test2,test3)
test4 <- remove_rownames(test4)
construct <- t(t(c('Interest', 'Interest', 'Utility', 'Utility', 'Cost', 'Cost')))
test4 <- cbind(test4, construct)
colnames(test4) <- c('item', 'Strongly Agree', 'Agree', 'Slightly Agree', 'neutral', 'slightly Disagree', 'Disagree', 'Strongly Disagree', 'type')
test4 <- rev(test4)
likert(item~. | type, test4, as.percent = T, col = diverging_hcl(7, rev = T, palette = 'Blue-Red 2'), main = F, rightAxis = F, layout=c(1,3), ylab = 'Demographic', xlim = c(-50,100), scales = list(x=list(at=seq(-50,100,20), labels=c(seq(50,0,-20),seq(10,100,20)))), borders = list(col = 'WHITE'))

##year vis

first_int <- as.data.frame(table(first[,1:3]))
first_uty <- as.data.frame(table(first[,4:7]))
first_cost <- as.data.frame(table(first[,8:10]))
int <- first_int[,2]
uty <- first_uty[,2]
uty <- c(uty, 0)
cost <- first_cost[,2]
len <-  length(first[,1])
int <- as.tibble(t(int))/(len*3)
uty <- as.tibble(t(uty))/(len*4)
cost <- as.tibble(t(cost))/(len*3)
#numbers above are based on sample size * question count
test <- c('int', 'uty', 'cost')
test <- as.tibble(t(test))
test <- t(test)
new <- rbind(int,uty,cost)
new <- cbind(test,new)
returning_int <- as.data.frame(table(returning[,1:3]))
returning_uty <- as.data.frame(table(returning[,4:7]))
returning_cost <- as.data.frame(table(returning[,8:10]))
int1 <- returning_int[,2]
uty1 <- returning_uty[,2]
cost1 <- returning_cost[,2]
len <- length(returning[,1])
int1 <- as.tibble(t(int1))/(len*3)
uty1 <- as.tibble(t(uty1))/(len*4)
cost1 <- as.tibble(t(cost1))/(len*3)
#numbers above are based on sample size * question count
test <- c('int', 'uty', 'cost')
test <- as.tibble(t(test))
test <- t(test)
new <- rbind(int1,uty1,cost1)
new <- cbind(test,new)
test1 <- rbind(int, int1)
test2 <- rbind(uty, uty1)
test3 <- rbind(cost, cost1)
year <- c('first', 'returning')
year <- as.tibble(t(year))
year <- t(year)
test1 <- cbind(year, test1)
test2 <- cbind(year, test2)
test3 <- cbind(year, test3)
test4 <- rbind(test1,test2,test3)
test4 <- remove_rownames(test4)
construct <- t(t(c('Interest', 'Interest', 'Utility', 'Utility', 'Cost', 'Cost')))
test4 <- cbind(test4, construct)
colnames(test4) <- c('item', 'Strongly Agree', 'Agree', 'Slightly Agree', 'neutral', 'slightly Disagree', 'Disagree', 'Strongly Disagree', 'type')
test4 <- rev(test4)
likert(item~. | type, test4, as.percent = T, col = diverging_hcl(7, rev = T, palette = 'Blue-Red 2'), main = F, rightAxis = F, layout=c(1,3), ylab = 'Demographic', xlim = c(-50,100), scales = list(x=list(at=seq(-50,100,20), labels=c(seq(50,0,-20),seq(10,100,20)))), borders = list(col = 'WHITE'))
