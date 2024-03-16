# read excel flat_file
df <- read.csv("flat_file.csv", header = TRUE, stringsAsFactors =
                 +                           FALSE, na.strings=c(""," ",NA))
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

# install packages
library(lubridate)
library(dplyr)
library(ggplot2)

#Count distinct row in each column
table(df$State)
table(df$X)
table(df$EmpID)
table(df$Zip)
table(df$EngagementSurvey)
table(df$EmpSatisfaction)
table(df$SpecialProjectsCount)
table(df$DaysLateLast30)
table(df$Absences)
table(df$Years_in_post)
table(df$Position)
table(df$Department)
table(df$ManagerName)
table(df$EmploymentStatus)
table(df$PerformanceScore)
table(df$Sex)
table(df$RaceDesc)
table(df$MaritalDesc)
table(df$RecruitmentSource)

summary(df)

# to see type of class in each column
sapply(df, class)

# Change these dates columns class to "Date"
df$DateofTermination <- as.Date(df$DateofTermination) 
df$DateofHire <- as.Date(df$DateofHire)
df$LastPerformanceReview_Date <- as.Date(df$LastPerformanceReview_Date)

# Replacing NA value in DateofTermination column with 2021-02-28
df$DateofTermination[is.na(df$DateofTermination)] <- "2021-02-28"

# Updating Years_in_post column
df$Years_in_post <- lubridate::time_length(difftime(df$DateofTermination, df$DateofHire), "years")

sapply(df, class)

# Create dfnumber that has only number for calculating correlation
dfnumber <- df[ -c(1, 2, 3, 4, 9, 10, 11, 15, 16, 17, 18, 19, 20, 21, 22, 23) ]

sapply(dfnumber, class)



###################Question 1###################################
# finding correlation between Absences column with the others
#SpecialProject and Absences has 0.47 in correlation
cor(dfnumber[-6], dfnumber$Absences)

# Salary&Years_in_post = 0.51, EngagementSurvey&DayslateLast30 = -0.59, SpecialProjectsCount&Absences = 0.47
# Years_in_post&SpecialProjectsCount = -0.37
cor(dfnumber)
#SpecialProjectsCount cor= 0.47, Salary cor = 0.19
#Salary and SpecialProjectsCount each tend to has high correlation with the Absences

#Plotting correlation of Salary vs Absences #1
ggplot(df, aes(x=Absences, y=Salary,)) + geom_point() + geom_smooth(method='lm')

#Plotting correlation of SpecialProjectsCount vs Absences#1
ggplot(df, aes(x=Absences, y=SpecialProjectsCount,)) + geom_point() + geom_smooth(method='lm')

#correlation between these two is 0.5
cor(df$SpecialProjectsCount, df$Salary)

# LM SpecialProjectsCount also significant 
model1 <-lm(formula = dfnumber$Absences ~ ., data = dfnumber)
summary(model1)

#SpecialProjectsCount
ggplot(df, aes(x=SpecialProjectsCount , y=Absences , color = Absences > 10)) + geom_point() + theme(legend.position = "top")


#Position
ggplot(df, aes(x=Position , y=Absences , color = Absences > 10)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 1.14e-11 ***
test11 <- aov(Absences~Position, data = df)
summary(test11)


#Department
ggplot(df, aes(x=Department , y=Absences , color = Absences > 10)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 6.11e-15 ***
test12 <- aov(Absences~Department, data = df)
summary(test12)


#Manger, Consider Manager who gave more than 15 absences days. These names show up in Production, IT/IS, Software Engineering
ggplot(df, aes(x=ManagerName , y=Absences , color = Absences)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 1.75e-10 ***
test13 <- aov(Absences~ManagerName, data = df)
summary(test13)

## Chi test for Department, Position, ManagerName

#Department and Position, p-value < 2.2e-16
cont_tableq11 <- table(df$Department, df$Position)
print(cont_tableq11)
chisq.test(cont_tableq11)

#Department and ManagerName, p-value < 2.2e-16
cont_tableq12 <- table(df$Department, df$ManagerName)
print(cont_tableq12)
chisq.test(cont_tableq12)

#Position and ManagerName, p-value < 2.2e-16
cont_tableq13 <- table(df$Position, df$ManagerName)
print(cont_tableq13)
chisq.test(cont_tableq13)

## Question 1 Combination ##

#Department and SpecialProjectsCount
ggplot(df, aes(x=Department , y=Absences , color = SpecialProjectsCount)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Table
table(df$Department, df$Absences)
table(df$Department, df$SpecialProjectsCount)

#Box plot for SpecialProjectsCount, IT/IS, Admin Offices, and SW Engineering are top 3
ggplot(data=df, aes(y=SpecialProjectsCount, x=Department, fill=Department)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Box plot for Absences, IT/IS, Admin Offices, and SW Engineering are top 3
ggplot(data=df, aes(y=Absences, x=Department, fill=Department)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# med Absences = 12, med Special Projects = 6
summary(subset(df,Department == "IT/IS"))
# med Absences = 10, med Special Projects = 4
summary(subset(df,Department == "Software Engineering"))
# med Absences = 8, med Special Projects = 0, absences dates came up because they have many staff, and there might be tough (209 peoples)
summary(subset(df,Department == "Production"))
# med Absences = 11, med Special Projects = 4
summary(subset(df,Department == "Admin Offices"))
# med Absences = 7, med Special Projects = 0, (31 Peoples)
summary(subset(df,Department == "Sales"))
# med Absences = 9, med Special Projects = 0
summary(df)

#Manager and Department
ggplot(df, aes(x=ManagerName , y=Absences , color = Department)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Manager and Position
ggplot(df, aes(x=ManagerName , y=Absences , color = Position)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##############################################################################################################










##################################Question 2###############################
# Note EngagementSurvey is a set of questions designed to measure how engaged, 
# committed, and contented employees are with their jobs and organizations

#Summary of VT people
summary(subset(df,EmploymentStatus == "Voluntarily Terminated"))
#Summary of Active people
summary(subset(df,EmploymentStatus == "Active"))



#Salary, most of VT has low salary
ggplot(df, aes(x= Salary, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#p-value =  0.0253 *
test21 <- aov(Salary~EmploymentStatus  == "Voluntarily Terminated", data = df)
summary(test21)

#SpecialProjectsCount
ggplot(df, aes(x= SpecialProjectsCount, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# p-value = 0.000787 ***
test22 <- aov(SpecialProjectsCount~EmploymentStatus  == "Voluntarily Terminated", data = df)
summary(test22)


#Years_in_post, most of VT has low Years_in_Post
ggplot(df, aes(x=Years_in_post, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")
# p-value = 1.96e-12 ***
test23 <- aov(Years_in_post~EmploymentStatus  == "Voluntarily Terminated", data = df)
summary(test23)

# Department, Production Department has the most VT
ggplot(df, aes(x=Department, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# p-value = 0.001561
cont_tableq21 <- table(df$EmploymentStatus == "Voluntarily Terminated", df$Department)
print(cont_tableq21)
chisq.test(cont_tableq21)

#Position,
ggplot(df, aes(x=Position, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#p-value = 0.2091
cont_tableq22 <- table(df$EmploymentStatus == "Voluntarily Terminated", df$Position)
print(cont_tableq22)
chisq.test(cont_tableq22)
#Pro Tech I 45/137 are VT
table(df$EmploymentStatus == "Voluntarily Terminated", df$Position == "Production Technician I")
#Pro Tech II 26/57 are VT
table(df$EmploymentStatus == "Voluntarily Terminated", df$Position == "Production Technician II")

#ManagerName, Find manager name
ggplot(df, aes(x=ManagerName, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Table
table(df$ManagerName,df$EmploymentStatus == "Voluntarily Terminated")
table(df$ManagerName,df$Department)
table(df$ManagerName,df$EmploymentStatus == "Voluntarily Terminated" & df$Department == "Production")

#p-value = 0.0007328
cont_tableq23 <- table(df$EmploymentStatus == "Voluntarily Terminated", df$ManagerName)
print(cont_tableq23)
chisq.test(cont_tableq23)
#ManagerName and Department have high impact on each other, so we can about only one of them
#p-value < 2.2e-16
cont_tableq29 <- table(df$Department, df$ManagerName)
print(cont_tableq24)
chisq.test(cont_tableq24)

#MaritalDesc, not giving much
ggplot(df, aes(x=MaritalDesc, y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#p-value = 0.001858
cont_tableq25 <- table(df$EmploymentStatus == "Voluntarily Terminated", df$MaritalDesc)
print(cont_tableq25)
chisq.test(cont_tableq25)

table(df$MaritalDesc, df$EmploymentStatus == "Voluntarily Terminated")
table(df$MaritalDesc == "Divorced" & df$EmploymentStatus == "Voluntarily Terminated", df$Department)

#RecruitmentSource, almost no VT from Linkedin people
ggplot(df, aes(x= RecruitmentSource , y=EmpID , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#p-value = 4.678e-07
cont_tableq27 <- table(df$EmploymentStatus == "Voluntarily Terminated", df$ RecruitmentSource )
print(cont_tableq27)
chisq.test(cont_tableq27)

###Question 2 Combination###

ggplot(df, aes(x=Department, y=Salary , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 75/209 are VT in Production Department
table(df$EmploymentStatus == "Voluntarily Terminated", df$Department)
# med salary = 59954, med years_in_post = 5
summary(subset(df,Department == "Production"))
# med salary = 62810, med years_in_post = 4.6
summary(df)

# Position and Salary, Most VT are in Production Department. Also this Department has low Salary
ggplot(df, aes(x=Position, y=Salary , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Use table to be more clear
table(df$EmploymentStatus == "Voluntarily Terminated", df$Position)

# Years_in_post and Salary, VT people tend to have low Salary and Years_in_post
ggplot(df, aes(x=Years_in_post, y=Salary , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")

# Position and Years_in_post, most of VT are from Production Department and they have low Years_in_post
ggplot(df, aes(x=Position, y=Years_in_post , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Years_in_post and EngagementSurvey, maybe VT people tried their best about their job, but no room to grow so they left
ggplot(df, aes(x=Years_in_post, y=EngagementSurvey , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# RecruitmentScource and Position, the graph is not clear
ggplot(df, aes(x=RecruitmentSource, y=Position , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Using table instead, we can see that most of the production staff are not from LinkedIn
table(df$EmploymentStatus == "Voluntarily Terminated" & df$Department == "Production", df$RecruitmentSource)
table(df$Department == "Production", df$RecruitmentSource)
table(df$Department, df$RecruitmentSource)
table(df$RecruitmentSource, df$EmploymentStatus == "Voluntarily Terminated")


# Position and Absences, VT people in Production department also have high Absences
ggplot(df, aes(x=Position, y=Absences , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#SpecialProjectsCount and Position
ggplot(df, aes(x=Position, y=SpecialProjectsCount , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Table
table(df$EmploymentStatus == "Voluntarily Terminated" & df$SpecialProjectsCount == 0, df$Department)
table(df$EmploymentStatus == "Voluntarily Terminated" & df$SpecialProjectsCount == 0, df$Position)
#Most of people in production department dont have special projects
table(df$SpecialProjectsCount == 0, df$Position)

#SpecialProjectsCount and Salary
ggplot(df, aes(x=SpecialProjectsCount, y=Salary , color = EmploymentStatus == "Voluntarily Terminated")) + geom_point() + theme(legend.position = "top")

#############################################################################################################












############################Question 3##############################################################33

####Conclusion for Question 3####
# Engagement Score = Motivation, Loyalty, Commitment to their company

# DaysLateLast30 has -0.59 correlation with EngagementSurvey
cor(dfnumber[-2], dfnumber$EngagementSurvey)


#DaysLateLast30, people who have 2-6 late days tend to have low EngagementScore (Score Lower than 3)
ggplot(df, aes(x=DaysLateLast30 , y=EngagementSurvey , color = EngagementSurvey <= 3)) + geom_point() + theme(legend.position = "top")
# cor = -0.5852315
cor(df$EngagementSurvey, df$DaysLateLast30)


#PerformanceScore, Need Improvement and PIP have a lot of EngagementScore lower than 3
ggplot(df, aes(x=PerformanceScore , y=EngagementSurvey , color = EngagementSurvey <= 3)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# p-value  = 2e-16 ***
test35 <- aov(EngagementSurvey~PerformanceScore, data = df)
summary(test35)

#EmploymentStatus, the Active Staff have a lot of EngagementScore lower than 3
ggplot(df, aes(x=EmploymentStatus , y=EngagementSurvey , color = EngagementSurvey <= 3)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# p-value = 0.0233 *
test36 <- aov(EngagementSurvey~EmploymentStatus, data = df)
summary(test36)


###combination for Question 3 ###
table(df$EmploymentStatus)
table(df$EmploymentStatus, df$EngagementSurvey <= 3)
table(df$EmploymentStatus, df$DaysLateLast30)
table(df$PerformanceScore)
# PIP <=3 = 12, Needs improvement <= 3 = 11
table(df$PerformanceScore, df$EngagementSurvey <= 3)
table(df$PerformanceScore == "PIP", df$EngagementSurvey)
table(df$PerformanceScore == "Needs Improvement", df$EngagementSurvey)
table(df$DaysLateLast30, df$EngagementSurvey <= 3)
table(df$PerformanceScore == "PIP" & df$EngagementSurvey <= 3, df$EmploymentStatus)

table(df$PerformanceScore == "Needs Improvement" & df$EngagementSurvey <= 3, df$EmploymentStatus)
table(df$PerformanceScore == "Needs Improvement" & df$EngagementSurvey <= 3, df$DaysLateLast30)
table(df$PerformanceScore == "PIP" & df$EngagementSurvey <= 3, df$DaysLateLast30)


# DaysLatelast30 and PerformanceScore
ggplot(df, aes(x=PerformanceScore , y=DaysLateLast30, color = EngagementSurvey <= 3)) + geom_point() + theme(legend.position = "top")

# PerformanceScore and EmploymentStatus
ggplot(df, aes(x=PerformanceScore , y=EmploymentStatus, color = EngagementSurvey <= 3)) + geom_point() + theme(legend.position = "top")

# DaysLatelast30 and EmploymentStatus
ggplot(df, aes(x=EmploymentStatus, y=DaysLateLast30, color = EngagementSurvey <= 3)) + geom_point() + theme(legend.position = "top")

######################################################################################################################################











#########################Question 4 ######################################################

ggplot(df, aes(x=Position , y=Salary, color = Sex)) + geom_point() + theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  geom_smooth(method='lm')

ggplot(df, aes(x=Years_in_post , y=Salary, color = Sex)) + geom_point() + theme(legend.position = "top") + geom_smooth(method='lm')

boxplot(Salary~Sex, data=df,main="Different boxplots of Salary for each Sex",xlab="Salary",
ylab="Sex",col="orange",border="brown")


#Pay gap between position
ggplot(data=df, aes(y=Salary, x=Position, fill=Sex)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=df, aes(y=Years_in_post, x=Position, fill=Sex)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Pay gap between department
ggplot(data=df, aes(y=Salary, x=Department, fill=Sex)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#median of salary
ggplot(data=df, aes(y=Salary, x=Sex, fill=Sex)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#median of Years_in_post
ggplot(data=df, aes(y=Years_in_post, x=Sex, fill=Sex)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Men med exp = 4.83 years med Salary = 63353
summary(subset(df,Sex == "M"))
#Women med exp = 4.65 years, med Salary = 62067
summary(subset(df,Sex == "F"))

#Men Prod Tech I, med exp = 5.11 years, med salary = 53407 
summary(subset(df,Position == "Production Technician I" & Sex == "M"))
#Women Prod Tech I, med exp = 5.22 years, med salary = 56339
summary(subset(df,Position == "Production Technician I" & Sex == "F"))

#Men Prod Tech II, med exp = 4.47 years, med salary = 63848
summary(subset(df,Position == "Production Technician II" & Sex == "M"))
#Women Prod Tech II, med exp = 3.92 years, med salary = 65902 
summary(subset(df,Position == "Production Technician II" & Sex == "F"))
#How come Tech II has lower med exp than Tech I? and they have higher salary too

#Men Prod Manager, med exp = 4.33 years, med salary = 78535
summary(subset(df,Position == "Production Manager" & Sex == "M"))
#Women Prod Manager, med exp = 5.49 years, med salary = 74929
summary(subset(df,Position == "Production Manager" & Sex == "F"))

#Men Data Analyst, med exp = 3.9 years, med salary = 88527
summary(subset(df,Position == "Data Analyst" & Sex == "M"))
#Women Data Analyst, med exp = 4.1 years, med salary = 89883
summary(subset(df,Position == "Data Analyst" & Sex == "F"))

#Men Area Sales Manager, med exp = 4.8 years, med salary = 66808
summary(subset(df,Position == "Area Sales Manager" & Sex == "M"))
#Women Area Sales Manager, med exp = 5.5 years, med salary = 61700  
summary(subset(df,Position == "Area Sales Manager" & Sex == "F"))

#Men IT Support, med exp = 4.2 years, med salary = 68678
summary(subset(df,Position == "IT Support" & Sex == "M"))
#Women IT Support, med exp = 7.7 years, med salary =65707  
summary(subset(df,Position == "IT Support" & Sex == "F"))

#Men Software Engineer, med exp = 10.5 years, med salary = 96703
summary(subset(df,Position == "Software Engineer" & Sex == "M"))
#Women Software Engineer, med exp = 11.5 years, med salary =97470 
summary(subset(df,Position == "Software Engineer" & Sex == "F"))

table(df$Position,df$Sex)

##################################################################################################################






























































