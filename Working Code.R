######### First, I will load the appropriate libraries in #########
library(dplyr)
library(data.table)
library(ggplot2)
library(mfx)
library(texreg)
library(stargazer)
library(foreign)

######### open SAV file, then converting to CSV file  ######### 

# library(haven)
# Kenya_R6 <- read_sav("~/Desktop/ECON 499 - DATASET/Kenya_R6.sav")
# View(Kenya_R6)
# write.csv(Kenya_R6, "Kenya_r6data.csv")

###Next, I load my data first (pulling a CSV from my github) ###

####---The data is from the Afrobarometer Household survey (see afrobarometer.org for more info) -----####
## reading in Round 3 and Round 6 ##
Kenya_R6 <- read.csv("https://github.com/sayinde/ECON499/blob/master/Kenya_r6data2015.csv?raw=true",header = TRUE,stringsAsFactors = FALSE)

Kenya_R3 <- read.csv("https://github.com/sayinde/ECON499/blob/master/Kenya_r3data.csv?raw=true",header = TRUE,stringsAsFactors = FALSE)

names(Kenya_R3)
names(Kenya_R6)

# Round 6 data is what is more valuable because it has the occupation variable and can be used for this #

#############-------Using data from Afrobarometer Data from 2015 round 6-----------##########
### I named my dataset "WORKINGkenyaround6" to distinguish it from the larger Afrobarometer Dataset ###

WORKINGkenyaround6 <- dplyr::select(
  Kenya_R6,
  URBRUR,
  #this means URBAN/RURAL
  REGION,
  #region or province the reponsdent lives in
  AGE = Q1,
  edu_attainment = Q97,
  occupation = Q96A,
  gender = Q101,
  length_withoutfood = Q8A,
  length_withoutcleanh20 = Q8B,
  length_withoutmeds = Q8C,
  length_withoutfuel = Q8D,
  length_withoutcash = Q8E,
  frequency_sansfood = Q8F,
  livingconditions = Q4B,
  remittances = Q9,
  freemovement = Q76
#  access_medcare = Q67E
)

## round 3 
WORKINGkenyaround3 <- dplyr::select(
  Kenya_R3,
  URBRUR,
  #this means URBAN/RURAL
  AGE = Q1,
  edu_attainment = Q90,
  occupation = Q95,
  gender = Q101,
  length_withoutfood = Q8A,
  length_withoutcleanh20 = Q8B,
  length_withoutmeds = Q8C,
  length_withoutfuel = Q8D,
  length_withoutcash = Q8E,
  school_expenses = Q8F,
  live_conditions = Q4B
#  econ_conditions = Q7A,
#  remittances = Q9,
#  freemovement = Q76
)


#I need to transform these values into 2 seperate categories in the "occupation" row in the Afrobarometer data
#formal sector jobs = 1
#informal sector jobs = 0
str(WORKINGkenyaround6$occupation)
str(WORKINGkenyaround3$occupation)

#I'm using a filter because I'm only paying attention to the occupations that I'm coding for informal and formal

WorkingData6v2 <-
  dplyr::filter(
    WORKINGkenyaround6,
    (occupation == 4) |
      (occupation == 6) | (occupation == 7) | (occupation == 9) |
      (occupation == 4) | (occupation == 11) | (occupation == 12) |
      (occupation == 8) | (occupation == 10) | (occupation == 5) | (occupation == 2) | (occupation == 3)
  )

WORKINGkenyaround3 <-
  dplyr::filter(
    WORKINGkenyaround3,
    (occupation == 6) |
      (occupation == 1) | (occupation == 2) | (occupation == 3) |
      (occupation == 4) |
      (occupation == 5) | (occupation == 6) | (occupation == 7) |
      (occupation == 8) |
      (occupation == 9) | (occupation == 10) | (occupation == 11) |
      (occupation == 12) |
      (occupation == 13) |
      (occupation == 14) | (occupation == 15) | (occupation == 16) |
      (occupation == 17) |
      (occupation == 18) | (occupation == 19) | (occupation == 20) |
      (occupation == 21) |
      (occupation == 22) | (occupation == 24) | (occupation == 201)
  )

# I made a data frome called Occupation Filter because I didn't want to override my previous dataset, 
# called "Working Data 2015" (personal preference)
## now, with this code (below), I have reclassified certain occupation responses into "informal"-- which equals 0, and "formal" -- which equals 1

WorkingData6v2 <- WorkingData6v2 %>% mutate(
    occupation =
      ifelse(occupation == 99, NA, occupation),
    occupation =  ifelse(occupation == 4, 0, occupation),
    occupation =  ifelse(occupation == 6, 0, occupation),
    occupation = ifelse(occupation == 7, 1, occupation),
    occupation =  ifelse(occupation == 2, 0, occupation),
    occupation =  ifelse(occupation == 3, 0, occupation),
    occupation = ifelse(occupation == 9, 1, occupation),
    occupation = ifelse(occupation == 11, 1, occupation),
    occupation = ifelse(occupation == 12, 1, occupation),
    occupation =  ifelse(occupation == 8, 1, occupation),
    occupation = ifelse(occupation == 10, 1, occupation),
    occupation =  ifelse(occupation == 5, 1, occupation)
  )

## informal jobs = 0 + 1-5, 6,8,11,14,16,24,201
## formal jobs = 1 + 7,9, 10, 12,13,15,17,18,19,20,21,22

WORKINGkenyaround3 <- WORKINGkenyaround3 %>% mutate(
  occupation =
    ifelse(occupation == 99, NA, occupation),
  occupation =  ifelse(occupation == 1, 0, occupation),
  occupation =  ifelse(occupation == 2, 0, occupation),
  occupation = ifelse(occupation == 3, 0, occupation),
  occupation =  ifelse(occupation == 4, 0, occupation),
  occupation =  ifelse(occupation == 5, 0, occupation),
  occupation = ifelse(occupation == 6, 0, occupation),
  occupation = ifelse(occupation == 8, 0, occupation),
  occupation = ifelse(occupation == 11, 0, occupation),
  occupation =  ifelse(occupation == 14, 0, occupation),
  occupation = ifelse(occupation == 16, 0, occupation),
  occupation =  ifelse(occupation == 24, 0, occupation),
  occupation = ifelse(occupation == 201, 0, occupation),
  occupation = ifelse(occupation == 7, 1, occupation),
  occupation =  ifelse(occupation == 9, 1, occupation),
  occupation = ifelse(occupation == 10, 1, occupation),
  occupation = ifelse(occupation == 12, 1, occupation),
  occupation =  ifelse(occupation == 13, 1, occupation),
  occupation = ifelse(occupation == 15, 1, occupation),
  occupation = ifelse(occupation == 17, 1, occupation),
  occupation =  ifelse(occupation == 18, 1, occupation),
  occupation = ifelse(occupation == 19, 1, occupation),
  occupation = ifelse(occupation == 20, 1, occupation),
  occupation = ifelse(occupation == 21, 1, occupation),
  occupation =  ifelse(occupation == 22, 1, occupation)
)

#########recoding the "99's" in the educational attainment column as NAs (because that's what they are)
#also recoding the grading values so that they are a bit more intuitive to the reader
###Meaning >> Instead of "completing secondary school" being equivalent to 5, it'll be equivalent to 12

WorkingData6v2 <- WorkingData6v2 %>%
  mutate( edu_attainment =  ifelse(edu_attainment == 9, 18, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 6, 14, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 7, 15, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 8, 16, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 5, 12, edu_attainment),
          edu_attainment = ifelse(edu_attainment == 99, NA, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 4, 8, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 3, 5, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 2, 3, edu_attainment))

unique(WorkingData6v2$edu_attainment)

WORKINGkenyaround3 <- WORKINGkenyaround3 %>%
  mutate( edu_attainment =  ifelse(edu_attainment == 9, 18, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 6, 14, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 7, 15, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 8, 16, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 5, 12, edu_attainment),
          edu_attainment = ifelse(edu_attainment == 99, NA, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 4, 8, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 3, 5, edu_attainment),
          edu_attainment =  ifelse(edu_attainment == 2, 3, edu_attainment))
unique(WORKINGkenyaround3$edu_attainment)

OccupationfilterR3 <- WORKINGkenyaround3
OccupationfilterR6 <- WorkingData6v2


##### Here, i'm recoding all the "dont knows"/aka 9's as NA's, because it'll make things easier when I'm conducting my analyses ####
OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    length_withoutfood =  ifelse(length_withoutfood == 9, NA, length_withoutfood))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    length_withoutcleanh20 =  ifelse(length_withoutcleanh20 == 9, NA, length_withoutcleanh20))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    length_withoutmeds =  ifelse(length_withoutmeds == 9, NA, length_withoutmeds))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    length_withoutfuel =  ifelse(length_withoutfuel == 9, NA, length_withoutfuel))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    length_withoutcash =  ifelse(length_withoutcash == 9, NA, length_withoutcash))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    livingconditions =  ifelse(livingconditions == 9, NA, livingconditions))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    freemovement =  ifelse(freemovement == 9, NA, freemovement))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    remittances =  ifelse(remittances == 9, NA, remittances))

# ------------------------------- occupation  NA removal for Round 3 ---------------------- #

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    length_withoutfood =  ifelse(length_withoutfood == 9, NA, length_withoutfood))

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    length_withoutcleanh20 =  ifelse(length_withoutcleanh20 == 9, NA, length_withoutcleanh20))

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    length_withoutmeds =  ifelse(length_withoutmeds == 9, NA, length_withoutmeds))

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    length_withoutfuel =  ifelse(length_withoutfuel == 9, NA, length_withoutfuel))

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    length_withoutcash =  ifelse(length_withoutcash == 9, NA, length_withoutcash))

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    livingconditions =  ifelse(livingconditions == 9, NA, livingconditions))

OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    edu_attainment = ifelse(edu_attainment == 99, NA, edu_attainment)
  )
OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    edu_attainment = ifelse(edu_attainment == 99, NA, edu_attainment)
  )


## gender and urban/rural location ##
OccupationfilterR6 <- OccupationfilterR6 %>%
  mutate(
    gender = ifelse(gender == 2, 0, gender),
    URBRUR = ifelse(URBRUR == 2, 0, URBRUR))

OccupationfilterR3 <- OccupationfilterR3 %>%
  mutate(
    gender = ifelse(gender == 2, 0, gender),
    URBRUR = ifelse(URBRUR == 2, 0, URBRUR))

# 1 is male
# 0 is female
# 1 is urban
# 0 is rural

##### made a new dataset without NA values
FinalDATASET_R6  = OccupationfilterR6[complete.cases(OccupationfilterR6),]

FinalDATASET_R3  = OccupationfilterR3[complete.cases(OccupationfilterR3),]

OccupationfilterR3$year <- 2007

OccupationfilterR6$year <- 2012

#----------------###################################################--------------------####
####                             END OF CLEANING                                        ####
####                                                                                    ####
####-------------###################################################--------------------####

OccupationfilterR3 
OccupationfilterR6 

#### Now, for some summary statistics #####

### statistics about percentages of informal/formal based on years of schooling 
edu_workR6 <- OccupationfilterR6 %>%
  group_by(occupation, edu_attainment) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))%>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type))

edu_workR3 <- OccupationfilterR3 %>%
  group_by(occupation, edu_attainment) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))%>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type))

##### general summary table
summary(OccupationfilterR6)
summary(OccupationfilterR3)

##### GGPLOT graphs 1-3, below provide a better view of summary statistics for the data set #######

##########################################################################################
###                             GG PLOT GRAPHS                                        ####
###                                                                                   ####
##########################################################################################


###------ GG PLOT GRAPH 1 -----## 
## Here, I'm grouping informal-formal participation by avg. education
#-- I want to see the distribution of educational attainment by gender and occupation --#

occu_genderR6 <- OccupationfilterR6 %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_edu = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    min_edu = min(edu_attainment, na.rm = T),
    max_edu = max(edu_attainment,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 0 & gender == 0 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 0, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )

occu_genderR3 <- OccupationfilterR3 %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_edu = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    min_edu = min(edu_attainment, na.rm = T),
    max_edu = max(edu_attainment,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 0 & gender == 0 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 0, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )


## ## ## ##

#  ## --------- Error bars represent standard error of the mean ROUND 6 ------------ ##
occu_genderggR6 <- ggplot(occu_genderR6, aes(x=type, y=avg_edu, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_edu-sd_edu/sqrt(count), ymax=avg_edu+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type and Gender - Round 6 (2015)")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE) 

occu_genderggR6 + theme(axis.text=element_text(size=12),
                      axis.title=element_text(face="bold"))


# ## ---------- Error bars represent standard error of the mean ROUND 3 ------------ ### 
occu_genderggR3 <- ggplot(occu_genderR3, aes(x=type, y=avg_edu, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_edu-sd_edu/sqrt(count), ymax=avg_edu+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type and Gender - Round 3 (2007)")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE) 

occu_genderggR3 + theme(axis.text=element_text(size=12),
                      axis.title=element_text(face="bold"))



####### ----- GGPLOT with just occupation ----###

######### GRAPH 2: What average education look like between the two sectors? ######

OccupationfilterR3

occupation_only <- OccupationfilterR6 %>%
  group_by(occupation) %>% 
  summarise(
    avg_edu = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    min_edu = min(edu_attainment, na.rm = T),
    max_edu = max(edu_attainment,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 , "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

# Error bars represent standard error of the mean
avg_edgg <- ggplot(occupation_only, aes(x=type, y=avg_edu, fill=as.factor(type))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_edu-sd_edu/sqrt(count), ymax=avg_edu+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type - ROUND 6 - 2015")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE) 

avg_edgg + theme(axis.text=element_text(size=12),
                 axis.title=element_text(face="bold"))



#### AVG ED for Round 3 ## 

occupation_only <- OccupationfilterR3 %>%
  group_by(occupation) %>% 
  summarise(
    avg_edu = mean(edu_attainment, na.rm = T),
    sd_edu = sd(edu_attainment, na.rm = T),
    min_edu = min(edu_attainment, na.rm = T),
    max_edu = max(edu_attainment,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 , "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

# Error bars represent standard error of the mean
avg_edgg <- ggplot(occupation_only, aes(x=type, y=avg_edu, fill=as.factor(type))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_edu-sd_edu/sqrt(count), ymax=avg_edu+sd_edu/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Education by Employment Type - ROUND 3 - 2007")+
  labs(x = "", y="Average Education", fill= "") + 
  guides(fill=FALSE) 

avg_edgg + theme(axis.text=element_text(size=12),
                 axis.title=element_text(face="bold"))



#############



######------------GRAPH 3: What happens when we add age, coupled with gender + occupation? ------------######
# I'm interested in seeing average age among two sectors #

Occup_gen_age <- OccupationfilterR6 %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_age = mean(AGE, na.rm = T),
    sd_age = sd(AGE, na.rm = T),
    min_age = min(AGE, na.rm = T),
    max_age = max(AGE,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 0 & gender == 0 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 0, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )

# Error bars represent standard error of the mean
avg_agegg <- ggplot(Occup_gen_age, aes(x=type, y=avg_age, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_age-sd_age/sqrt(count), ymax=avg_age+sd_age/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Age by Employment Type - Round 6 (2015)")+
  labs(x = "", y="Average Age", fill= "") + 
  guides(fill=FALSE)

avg_agegg + theme(axis.text=element_text(size=12),
                  axis.title=element_text(face="bold"))

#### Round 3 ### 

Occup_gen_age <- OccupationfilterR3 %>%
  group_by(occupation, gender) %>% 
  summarise(
    avg_age = mean(AGE, na.rm = T),
    sd_age = sd(AGE, na.rm = T),
    min_age = min(AGE, na.rm = T),
    max_age = max(AGE,na.rm = T),
    count = n()
  ) %>%
  mutate(type = ifelse(occupation == 0 & gender == 1, "Informal Male", NA),
         type = ifelse(occupation == 0 & gender == 0 , "Informal Female", type),
         type = ifelse(occupation == 1 & gender == 0, "Formal Female", type),
         type = ifelse(occupation == 1 & gender == 1, "Formal Male", type)
  )

# Error bars represent standard error of the mean
avg_agegg <- ggplot(Occup_gen_age, aes(x=type, y=avg_age, fill=as.factor(gender))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_age-sd_age/sqrt(count), ymax=avg_age+sd_age/sqrt(count)),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(title = "Average Age by Employment Type - Round 3 (2007)")+
  labs(x = "", y="Average Age", fill= "") + 
  guides(fill=FALSE)

avg_agegg + theme(axis.text=element_text(size=12),
                  axis.title=element_text(face="bold"))

############## ------------- GRAPH 4: Food Insecurity---------------- #####################




Food_insecure6r  <- OccupationfilterR6 %>%
  group_by(occupation, length_withoutfood) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

Fsecurity_ggplotr6 <- ggplot(Food_insecure6r, aes(x=length_withoutfood,
                                            y=freq,
                                            fill=as.factor(type)
)) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity") +
  labs(title = "Food Insecurity by Employment Type - Round 6 (2015)")+
  labs(x = "Frequency without Food (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

colors <- colorRampPalette(brewer.pal(5, "Accent"))


Fsecurity_ggplotr6 + theme(axis.text=element_text(size=12),
                         axis.title=element_text(face="bold")) + scale_fill_manual(values = colors(6))


############## - 2007 --- ##

Food_insecure3r  <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutfood) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

Fsecurity_ggplotr3 <- ggplot(Food_insecure3r, aes(x=length_withoutfood,
                                            y=freq,
                                            fill=as.factor(type)
)) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity") +
  labs(title = "Food Insecurity by Employment Type - Round 3 (2007)")+
  labs(x = "Frequency without Food (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))


Fsecurity_ggplotr3 + theme(axis.text=element_text(size=12),
                         axis.title=element_text(face="bold")) + scale_fill_manual(values = colors(6))




############## ------------- GRAPH 5: Cash Volatility---------------- #####################

cash_volatiler6 <- OccupationfilterR6 %>%
  group_by(occupation, length_withoutcash) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

Cvolatileggplotr6 <- ggplot(cash_volatiler6, aes(x=length_withoutcash, y=freq, fill=as.factor(type))) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Cash Volitality by Employment Type - Round 3 (2015)")+
  labs(x = "Frequency without Cash (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

colors1 <- colorRampPalette(brewer.pal(5, "Set2"))


Cvolatileggplotr6 + theme(axis.text=element_text(size=12),
                        axis.title=element_text(face="bold")) +
  xlim(-.5,4.5) + scale_fill_manual(values = colors1(6))


############## ----------- Round 3 2007 --

cash_volatiler3 <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutcash) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

Cvolatileggplotr3 <- ggplot(cash_volatiler3, aes(x=length_withoutcash, y=freq, fill=as.factor(type))) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Cash Volitality by Employment Type - Round 3 (2007)")+
  labs(x = "Frequency without Cash (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))


Cvolatileggplotr3 + theme(axis.text=element_text(size=12),
                        axis.title=element_text(face="bold")) +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors1(6))


############## ------------- GRAPH 5: Medicine inconsistency --------------- #####################

medsr6 <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutmeds) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

medsggplotr6 <- ggplot(medsr6, aes(x=length_withoutmeds, y=freq, fill=as.factor(type))
) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Inconsistency of Medicine by Employment Type - Round 6 (2015)")+
  labs(x = "Frequency without Medicine (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

medsggplotr6 + theme(axis.text=element_text(size=12),
                   axis.title=element_text(face="bold")) +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors2(6))


colors2 <- colorRampPalette(brewer.pal(5, "Set3"))


############## ----------- Round 3 2007 --

medsr3 <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutmeds) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

medsggplotr3 <- ggplot(medsr3, aes(x=length_withoutmeds, y=freq, fill=as.factor(type))
) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Inconsistency of Medicine by Employment Type - Round 3 (2007)")+
  labs(x = "Frequency without Medicine (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

medsggplotr3 + theme(axis.text=element_text(size=12),
                   axis.title=element_text(face="bold")) +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors2(6))

############## ------------- GRAPH 6: Fuel Inconsistency --------------- #####################

fuelr6 <- OccupationfilterR6 %>%
  group_by(occupation, length_withoutfuel) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

fuelggplotr6 <- ggplot(fuelr6, aes(x=length_withoutfuel, y=freq, fill=as.factor(type))) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Inconsistency of Cooking Fuel by Employment Type")+
  labs(x = "Frequency without Cooking Fuel (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

fuelggplotr6 +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors3(2))


colors3 <- colorRampPalette(brewer.pal(5, "Accent"))

############## ----------- Round 3 2007 --

fuelr3 <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutfuel) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

fuelggplotr3 <- ggplot(fuelr3, aes(x=length_withoutfuel, y=freq, fill=as.factor(type))) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Inconsistency of Cooking Fuel by Employment Type")+
  labs(x = "Frequency without Cooking Fuel (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))

fuelggplotr3 +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors3(2))

############## ------------- GRAPH 7: Clean Water Consistency --------------- #####################

h20r6 <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutcleanh20) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

h20ggplotr6 <- ggplot(h20r3, aes(x=length_withoutcleanh20, y=freq, fill=as.factor(type))) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Inconsistency of Clean Water by Employment Type")+
  labs(x = "Frequency without Clean Water (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))


h20ggplotr6 +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors3(8))


colors3 <- colorRampPalette(brewer.pal(5, "Paired"))

############## ----------- Round 3 2007 --

h20r3 <- OccupationfilterR3 %>%
  group_by(occupation, length_withoutcleanh20) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100 ) %>%
  mutate(type = ifelse(occupation == 0, "Informal", NA),
         type = ifelse(occupation == 1, "Formal", type)
  )

h20ggplotr3 <- ggplot(h20r3, aes(x=length_withoutcleanh20, y=freq, fill=as.factor(type))) + 
  geom_histogram(position=position_dodge(),
                 stat = "identity")+
  labs(title = "Inconsistency of Clean Water by Employment Type")+
  labs(x = "Frequency without Clean Water (annual)", y="Percentage of sector", fill= "Employment Types") +
  scale_fill_manual(values=c("#79c36a", "#599ad3"))


h20ggplotr3 +
  xlim(-.5,4.5)+ scale_fill_manual(values = colors3(8))

######## ------------- ##


#################-------------------GRAPHS DONE --------------------#########################

############################################################################################



##############################################################################################
######                           REGRESSIONS                                           #######
#####                                                                                   ######
##############################################################################################


########--------------------------- LOGIT MODEL REGRESSIONS --------------------------########
##############################################################################################

### I will be performing a logit model regression because my outcome/dependent variable 
### (which will be occupation) is binary. Because I am using a logit model regression model,
### I will have to seperate the educational variable into binary code, by seperating the categories
### based on years of schooling.


###########       Step 1: Transforming education values into binary code     #########


## primary school completed
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ1 = edu_attainment)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ1 =  ifelse(edu_attainment < 5, 0, 1))

### greater than primary school but less than high school
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ2 = edu_attainment)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ2 =  ifelse(edu_attainment < 9, 0, 1))

### college completed
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ3 = edu_attainment)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ3 =  ifelse(edu_attainment < 16, 0, 1))

### post-graduate
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ4 = edu_attainment)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Educ4 =  ifelse(edu_attainment < 18, 0, 1))


##### ROUND 5/6 ------- ###

## primary school completed
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ1 = edu_attainment)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ1 =  ifelse(edu_attainment < 5, 0, 1))

### greater than primary school but less than high school
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ2 = edu_attainment)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ2 =  ifelse(edu_attainment < 9, 0, 1))

### college completed
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ3 = edu_attainment)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ3 =  ifelse(edu_attainment < 16, 0, 1))

### post-graduate
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ4 = edu_attainment)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Educ4 =  ifelse(edu_attainment < 18, 0, 1))


###########       Step 2: Transforming AGE values into binary code     ###########


## Ages 18-25
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age1 = AGE)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age1 =  ifelse(AGE <= 25 & AGE >= 18, 0, 1))

## Ages 26-34
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age2 = AGE)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age2 =  ifelse(AGE <= 34 & AGE >= 26, 0, 1))

#Ages 35-45
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age3 = AGE)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age3 =  ifelse(AGE <= 45 & AGE >= 35, 0, 1))

#Ages 46-55
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age4 = AGE)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age4 =  ifelse(AGE >= 46 & AGE <= 55, 0, 1))

## Those greater than 56
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age5 = AGE)
OccupationfilterR3 <- mutate(OccupationfilterR3,
                              Age5 =  ifelse(AGE >= 56, 0, 1))

#### Round 6 ###

###########       Step 2: Transforming AGE values into binary code     ###########


## Ages 18-25
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age1 = AGE)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age1 =  ifelse(AGE <= 25 & AGE >= 18, 0, 1))

## Ages 26-34
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age2 = AGE)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age2 =  ifelse(AGE <= 34 & AGE >= 26, 0, 1))

#Ages 35-45
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age3 = AGE)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age3 =  ifelse(AGE <= 45 & AGE >= 35, 0, 1))

#Ages 46-55
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age4 = AGE)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age4 =  ifelse(AGE >= 46 & AGE <= 55, 0, 1))

## Those greater than 56
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age5 = AGE)
OccupationfilterR6 <- mutate(OccupationfilterR6,
                              Age5 =  ifelse(AGE >= 56, 0, 1))

########---------------------------  REGRESSION TIME----------------------------------########
##############################################################################################

### regression 1: Gender as an independent variable, and occupation as a dependent variable
regression1 <- logitmfx(occupation ~ gender,
                        data = OccupationfilterR6)

## regression 2: Age(s) as independent vaiables, and occupation as a dependent variable

regression2 <- logitmfx(occupation ~ gender + Age1 + Age2 + Age3 + Age4 + Age5,
                        data = OccupationfilterR6)

## regression 3: Education levels as independent variables, and occupation as a dependent variable

regression3 <-logitmfx(occupation ~ gender + Age1 + Age2 + Age3 + Age4 + Age5+ Educ1 + Educ2 + Educ3 + Educ4,
                       data = OccupationfilterR6)

## regression 4: Urban/rural location as an independent variable, and occupation as a dependent variable
regression4 <- logitmfx(occupation ~ gender + Age1 + Age2 + Age3 + Age4 + Age5 + Educ1 + Educ2 + Educ3 + Educ4 + URBRUR,
                        data = OccupationfilterR6)




############# ROUND 3 DATA ##############


### regression 1: Gender as an independent variable, and occupation as a dependent variable
r3egression1 <- logitmfx(occupation ~ gender,
                        data = OccupationfilterR3)

## regression 2: Age(s) as independent vables, and occupation as a dependent variable

r3egression2 <- logitmfx(occupation ~ gender + Age1 + Age2 + Age3 + Age4 + Age5,
                        data = OccupationfilterR3)

## regression 3: Education levels as independent variables, and occupation as a dependent variable

r3egression3 <-logitmfx(occupation ~ gender + Age1 + Age2 + Age3 + Age4 + Age5+ Educ1 + Educ2 + Educ3 + Educ4,
                       data = OccupationfilterR3)

## regression 4: Urban/rural location as an independent variable, and occupation as a dependent variable
r3egression4 <- logitmfx(occupation ~ gender + Age1 + Age2 + Age3 + Age4 + Age5 + Educ1 + Educ2 + Educ3 + Educ4 + URBRUR,
                        data = OccupationfilterR3)


########-------------------------------------------------#########


texreg(regression1)
texreg(regression2)
texreg(regression3)
texreg(regression4)



### Round 3#### 

texreg(r3egression1)
texreg(r3egression2)
texreg(r3egression3)
texreg(r3egression4)