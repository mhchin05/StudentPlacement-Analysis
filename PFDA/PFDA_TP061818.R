# CHIN_MIN_HWEI
# TP061818

#Import data
sampleData = read.csv("C:\\Users\\User\\Desktop\\APD PFDA\\PFDA\\Placement_Data_Full_Class.csv",header=TRUE)
sampleData

#Assign Headers
names(sampleData) = c("SerialNo","Gender","Age","Address","M_edu","F_edu","M_job","F_job","FamSup",
                      "Paid_Class","Activities","Internet","Ssc_p","Ssc_b","Hsc_p","Hsc_b","Hsc_s",
                      "Degree_p","Degree_sp","Work_ex","EmpTest_p","MBA_sp","MBA_p","Status","Salary")
View(sampleData)

install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotrix")
install.packages("Hmisc")
install.packages("scales")

library(ggplot2)
library(dplyr)
library(plotrix)
library(Hmisc)
library(scales)

#show status = not placed
View(sampleData[sampleData$Status=="Not Placed",])

#summary
summary(sampleData$EmpTest_p) 
summary(sampleData)

#DROP COLUMN
sampleData$Age_cat = NULL

#-----------------------------------------------------------------------------------------------------------------

# F = 8564 ; M = 8443 
gender_table = sampleData %>%
  select(Gender)%>%
  count(Gender)
gender_table



nlevels(factor(sampleData$MBA_sp))   #2 category




test = sampleData %>%
  select(Status, Gender,Address) %>%
  filter(Status == "Placed" & Gender == "M" & Address =="U") %>% 
  count(Status, Gender,Address)
test


sampleData %>%
  select(Status, Gender,Address) %>%
  filter(Status == "Placed" & Gender == "M" & Address =="R") %>% 
  count(Status, Gender,Address)

sampleData %>%
  select(Status, Gender,Address) %>%
  filter(Status == "Placed" & Gender == "F" & Address =="U") %>% 
  count(Status, Gender,Address)

sampleData %>%
  select(Status, Gender,Address) %>%
  filter(Status == "Placed" & Gender == "F" & Address =="R") %>% 
  count(Status, Gender,Address)


#**************** QUESTION 1 ***************************************************************************
#---Q1A1-------------------------------------------------------------------------
# PLACEMENT STATUS (Placed: 8742 ; Not Placed: 8265)
status_table = sampleData %>%
  select(Status)%>%
  count(Status)
status_table

pie(status_table$n, labels=paste(status_table$Status,":",status_table$n), 
    main="Placement Status",col=c("blue","purple"), clockwise = TRUE)

status_table %>%
  mutate(Percentage = n/sum(n) * 100)



#---Q1A2-------------------------------------------------------------------------
#Gender and placement status 
gen_stat = sampleData %>%
  select(Gender,Status)%>%
  count(Gender,Status)
gen_stat

ggplot(gen_stat, aes(x = Gender, y = Status, size = n, color = Gender)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(10, 20)) +
  geom_text(aes(label = n), size=4, color="black") +
  labs(title = "Relationship between Gender and Placement Status",
       x = "Gender", y = "Status") +
  theme_bw()



#---Q1A3-------------------------------------------------------------------------
#Age and placement status 
age_stat = sampleData %>%
  select(Age,Status)%>%
  count(Age,Status)
age_stat

ggplot(age_stat, aes(x = Age, y = n, fill = Status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), size=4, color="black") +
  labs(x = "Age", y = "Count", title = "Histogram of Placement Status by Age") +
  scale_fill_manual(values = c("#FFCDD2", "#A1887F")) +
  facet_wrap(~Status)



#---Q1A3-1------------------------------------------------------------------------
# Filter to compare age group (PLACED STATUS)
age_stat1 = sampleData %>%
  select(Age, Status) %>%
  filter(Status == "Placed" & Age >= 21) %>%
  count(Age, Status)
age_stat1

age_stat2 = sampleData %>%
  select(Age, Status) %>%
  filter(Status == "Placed" & Age < 21) %>%
  count(Age, Status)
age_stat2

total_above21 = sum(age_stat1$n)
total_above21    #4384
total_below21 = sum(age_stat2$n)
total_below21    #4358

pct_above21 = percent(total_above21/sum(c(total_above21, total_below21)),accuracy=0.01)
pct_above21     #50.15%
pct_below21 = percent(total_below21/sum(c(total_above21, total_below21)),accuracy=0.01)
pct_below21     #49.85%


pie(c(total_above21, total_below21), 
    labels = c(paste0("Age >= 21: ", pct_above21),
               paste0("Age < 21: ", pct_below21)), 
    main = "Placed status for student age above and below 21",
    col = c("#3F51B5", "#FCCB00"), cex=1.2, clockwise = TRUE)


# NOT PLACED STATUS
sampleData %>%
  select(Age, Status) %>%
  filter(Status == "Not Placed" & Age >= 21) %>%
  count(Age, Status)


sampleData %>%
  select(Age, Status) %>%
  filter(Status == "Not Placed" & Age < 21) %>%
  count(Age, Status)



#---Q1A4-------------------------------------------------------------------------
#Address and placement status 
add_stat = sampleData %>%
  select(Address,Status)%>%
  count(Address,Status)
add_stat

ggplot(add_stat,aes(x=Address, y=n, fill=Status)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.8)) + 
  labs(title = "Correlation Between Address and Status",
       x = "Address", y = "Frequency") + 
  scale_fill_manual(values = c("Placed" = "#7986CB", "Not Placed" = "#607D8B"))



#---Q1A5-------------------------------------------------------------------------
#MBA specialism and placement status 
data = sampleData %>%
  select(MBA_sp,Status)%>%
  count(MBA_sp,Status)
data

ggplot(data, aes(x=n, y=MBA_sp, fill=Status)) + 
  geom_col() + 
  geom_text(aes(label=n), hjust=-0.2) +
  labs(x='Number of Students', y='MBA Specialism', 
       title='MBA Specialism and Placement Status') +
  scale_fill_manual(values=c('#00BCD4', '#FF6F00')) +
  theme_minimal() +
  facet_wrap(~Status, ncol=1)



#---Q1A6-------------------------------------------------------------------------
#Degree course and placement status 
deg_stat = sampleData %>%
  select(Degree_sp,Status)%>%
  count(Degree_sp,Status)
deg_stat


ggplot(deg_stat, aes(x = Degree_sp, y = n)) +
  geom_segment(aes(x=Degree_sp, xend=Degree_sp, y=0, yend=n, color=Status), linewidth=3) +
  geom_point(size = 12, color = "darkred") +
  geom_text(aes(label = n), color = "white") +
  labs(title = "Lollipop Chart for Degree Course and Placement Status ",
       x = "Degree Course", y = "Frequency", color = "Status") + facet_wrap(~Status) +
  scale_color_manual(values = c("cornflowerblue", "coral")) + 
  theme_gray()



#---Q1A7-------------------------------------------------------------------------
#Higher Secondary Education Specialization and placement status 
hsc_stat = sampleData %>%
  select(Hsc_s,Status)%>%
  count(Hsc_s,Status)
hsc_stat

ggplot(hsc_stat, aes(x = Hsc_s, y = Status, size = n, color = Hsc_s)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(10, 20)) +
  geom_text(aes(label = n), size=4, color="black") +
  labs(title = "Higher Secondary Specialization and Placement Status",
       x = "Specialization", y = "Status") +
  scale_color_manual(values = c("#9C27B0", "#FF6F00","#E56F98")) 



#---Q1A8-------------------------------------------------------------------------
#HOW EMPLOYMENT TEST % AFFECTS PLACEMENT STATUS
EmpTest_p = sampleData %>%
  select(EmpTest_p,Status)%>%
  count(EmpTest_p,Status)
EmpTest_p

summary(sampleData$EmpTest_p[sampleData$Status=="Placed"])
summary(sampleData$EmpTest_p[sampleData$Status=="Not Placed"])

ggplot(sampleData, aes(x=Status, y=EmpTest_p, color=Status)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 4, color = "black", show.legend = FALSE, 
               aes(label = round(..y.., digits = 2))) +
  labs(x='Placement Status', y='Employment Test Score',
       title='Distribution Of Employment Test Scores by Placement Status') 



#---Q1A8-1------------------------------------------------------------------------
#CORRELATION BETWEEN EMPLOYEE TEST >= 75% AND STATUS 
# - create a new column to check if marks is higher than 75%
sampleData$eTest_status = ifelse(sampleData$EmpTest_p >= 75, "High Etest", "Low Etest")

eTest_status = sampleData %>%
  select(eTest_status,Status)%>%
  count(eTest_status,Status)
eTest_status  

ggplot(eTest_status,aes(x=Status, y=n, fill=eTest_status)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(title = "Correlation Between Employee Test Rate and Status",
       x = "Job Status", y = "Frequency")



#---Q1A9-------------------------------------------------------------------------
#Father's job and placement status 
fjob = sampleData %>%
  select(F_job,Status)%>%
  count(F_job,Status)
fjob

ggplot(fjob, aes(x = F_job, y = n, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n)) +
  labs(title = "Relationship between Father Job and Placement Status",
       x = "Father Job", y = "Frequency", fill = "Placement Status") +
  scale_fill_manual(values = c("#B2DFDB", "#4DB6AC"))



#---Q1A10-------------------------------------------------------------------------
#Mother's job and placement status 
mjob = sampleData %>%
  select(M_job,Status)%>%
  count(M_job,Status)
mjob

ggplot(mjob, aes(x = M_job, y = n, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n)) +
  labs(title = "Relationship between Mother Job and Placement Status",
       x = "Mother Job", y = "Frequency", fill = "Placement Status") +
  scale_fill_manual(values = c("#D1C4E9", "#9575CD"))


# --Find the frequency either momjob=placed or dadjob=placed is high--
fjob_placed = fjob%>%
  filter(Status == "Placed")
fjob_placed

totalF = sum(fjob_placed$n)
totalF    #8742

mjob_placed = mjob%>%
  filter(Status == "Placed")
mjob_placed

totalM = sum(mjob_placed$n)
totalM    #8742



#---Q1A11------------------------------------------------------------------------
#Family support and placement status 
fam_stat = sampleData %>%
  select(FamSup,Status)%>%
  count(FamSup,Status)
fam_stat

ggplot(fam_stat, aes(x = FamSup, y = n)) +
  geom_segment(aes(x=FamSup, xend=FamSup, y=0, yend=n, color=Status), linewidth=4) +
  geom_point(size = 13, color = "black") +
  geom_text(aes(label = n),color="white") +
  labs(title = "Connection between Family Support and Placement Status",
       x = "Family Support", y = "Frequency", color = "Status") + 
  scale_color_manual(values = c("#303F9F", "#0097A7")) + 
  facet_wrap(~Status) +
  theme_minimal()



#---Q1A12------------------------------------------------------------------------
#RELATIONSHIP BETWEEN WORK EXPERIENCE AND JOB PLACEMENT
Work_ex = sampleData %>%
  select(Work_ex,Status)%>%
  count(Work_ex,Status)
Work_ex 

ggplot(Work_ex, aes(x=Work_ex, y=n, fill=Status)) +
  geom_bar(position="dodge", stat="identity") + geom_label(aes(label=n)) +
  labs(title = "Relationship between Work Experience and Job Placement",
       x = "Working Experience", y = "Count") +
  scale_fill_manual(values = c("#FFB74D", "#E65100"))




#**************** QUESTION 2 *******************************************************************************
#---Q2A1------------------------------------------------------------------------
# PARENTS JOB DISTRIBUTION 
Fjob = sampleData %>%
  select(F_job)%>%
  count(F_job)
Fjob 

pie3D(Fjob$n, labels=paste(Fjob$F_job,":",Fjob$n), 
    main="3D Pie Chart for Dad's Job", explode=.3)


Mjob = sampleData %>%
  select(M_job)%>%
  count(M_job)
Mjob 

pie(Mjob$n, labels=paste(Mjob$M_job,":",Mjob$n), 
    main="Pie Chart for Mom's Job", clockwise = TRUE)



#---Q2A2------------------------------------------------------------------------
#FIND OUT WHETHER BOTH PARENTS AT_HOME AFFECTS HSC_P%
Pathome = sampleData %>%
  select(F_job,M_job,Hsc_p)%>%
  mutate(Pathome = ifelse(M_job=="at_home" & F_job=="at_home", "p_AtHome","Other"))%>%
  count(Pathome,Hsc_p)
Pathome

#Filter row 
p_home = Pathome%>%
  filter(Pathome == "p_AtHome")
p_home

#Compare marks>= 75% and <75 
total = sum(p_home$n)
total        #623

above_75 = sum(p_home$n[p_home$Hsc_p >= 75])
above_75     #272

below_75 = sum(p_home$n[p_home$Hsc_p < 75])
below_75     #351 


#create a horizontal bar chart  
hsc_athome = data.frame(
  HSC = c("above(>=75)", "below(<75)"),
  count = c(above_75, below_75)
)

ggplot(hsc_athome, aes(x = HSC, y = count, fill = HSC)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=count), size=5) + 
  scale_fill_manual(values = c("#9C27B0", "#03A9F4")) + 
  labs(x = "HSC percentage", y = "Frequency", 
       title = "Parents at_home affect Hsc_p marks") + 
  coord_flip() + theme_minimal()



#---Q2A3------------------------------------------------------------------------
# FAMILY SUPPORT BY ADDRESS
FamSup = sampleData %>%
  select(FamSup,Address)%>%
  count(FamSup,Address)
FamSup 

ggplot(FamSup, aes(x = FamSup, y = n)) +
  geom_segment(aes(x=FamSup, xend=FamSup, y=0, yend=n, color=Address), linewidth=2) +
  geom_point(size = 10, color = "gray") +
  geom_text(aes(label = n)) +
  labs(title = "Lollipop Chart for Family Support by Address",
       x = "Family Support", y = "Count", color = "Address") + facet_wrap(~Address) +
  theme_minimal()



#---Q2A4------------------------------------------------------------------------
#BOARD OF HIGHER SECONDARY EDU BASED ON ADDRESS
hsB = sampleData %>%
  select(Hsc_b,Address)%>%
  count(Hsc_b,Address)
hsB 

ggplot(hsB, aes(x=Hsc_b, y=n, fill=Address)) +
  geom_bar(position="dodge", stat="identity") + geom_text(aes(label=n)) +
  labs(title = "Board of Higher Secondary Education Based On Address",
       x = "Higher Education Board", y = "Frequency") +
  scale_fill_manual(values = c("#90A4AE", "#607D8B"))



#---Q2A5------------------------------------------------------------------------
# CORRELATION BETWEEN HIGHER EDU BY PARENTS AND MASTER %
# if M_edu & F_edu == 4, show $MBA_p 
P_edu = sampleData %>%
  select(M_edu,F_edu,MBA_p)%>%
  mutate(P_edu = ifelse(M_edu==4 & F_edu==4, "High Education","Other"))%>%
  count(P_edu,MBA_p)
P_edu 
# Only get High Education row 
P_edu_high = P_edu%>%
filter(P_edu == "High Education")
P_edu_high

summary(sampleData$MBA_p[sampleData$M_edu==4 & sampleData$F_edu==4])
mean_Val = mean(subset(sampleData, M_edu==4 & F_edu == 4)$MBA_p)
mean_Val

ggplot(P_edu_high,aes(x = P_edu, y = MBA_p)) +
  geom_violin(aes(fill = P_edu)) +
  geom_boxplot(width=0.2, fill="white") +
  scale_fill_manual(values=c("#9575CD"), name="Parents Edu", labels=c("MBA &")) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun = "mean", geom = "text", aes(label = paste0("Mean: ", round(mean_Val, 2))), vjust = -1.5) +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Parents Education", y = "MBA Marks", title = "Higher Education By Parents and MBA Percentage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#---Q2A6---------------------------------------------------------------------------
#IMPACT ON FAMILY SUPPORT TOWARDS DEGREE FIELD AND MASTER SPECIALISM 
df = sampleData %>%
  select(FamSup,Degree_sp,MBA_sp)%>%
  count(FamSup,Degree_sp,MBA_sp)
df


ggplot(df, aes(x = MBA_sp, y = Degree_sp, size = n, color=n)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  scale_color_viridis_c(option = "C") +
  geom_text(aes(label=n), size=4, color="black") +
  labs(title = "Family Suppot On Degree Field and Master Specialism", 
       x = "MBA Specialism", y = "Degree Course",
       size = "Freq", color = "Freq") +
  facet_wrap(~FamSup) +
  theme_bw()



#---Q2A7---------------------------------------------------------------------------
# MBA MARKS AND FAMILY SUPPORT 
# Filter to get MBA% >=75
mba_fams = sampleData %>%
  select(MBA_p, FamSup) %>%
  filter(FamSup == "yes" & MBA_p >= 75) %>%
  count(MBA_p, FamSup)
mba_fams

mba_fams2 = sampleData %>%
  select(MBA_p, FamSup) %>%
  filter(FamSup == "no" & MBA_p >= 75) %>%
  count(MBA_p, FamSup)
mba_fams2

#Calculate MBA marks >= 75 with/without family support
total_yes = sum(mba_fams$n)
total_yes    #3815
total_no = sum(mba_fams2$n)
total_no     #3932

pct_yes = percent(total_yes/sum(c(total_yes, total_no)),accuracy=0.01)
pct_yes     #49.24%
pct_no = percent(total_no/sum(c(total_yes, total_no)),accuracy=0.01)
pct_no      #50.76% 


pie3D(c(total_yes, total_no), 
    labels = c(paste0("Yes: ", pct_yes),
               paste0("No: ", pct_no)), 
    main = "Impact on high MBA marks with or without family support",
    col = c("#FF006E", "#607D8B"))



#---Q2A8---------------------------------------------------------------------------
# DEGREE MARKS AND FAMILY SUPPORT 
deg_fams = sampleData %>%
  select(Degree_p, FamSup) %>%
  filter(FamSup == "yes" & Degree_p >= 75) %>%
  count(Degree_p, FamSup)
deg_fams

deg_fams2 = sampleData %>%
  select(Degree_p, FamSup) %>%
  filter(FamSup == "no" & Degree_p >= 75) %>%
  count(Degree_p, FamSup)
deg_fams2

#Calculate Degree marks >= 75 with/without family support
yesSup = sum(deg_fams$n)
yesSup    #3759
noSup = sum(deg_fams2$n)
noSup     #3866

yes_p = percent(yesSup/sum(c(yesSup, noSup)),accuracy=0.01)
yes_p     #49.30%
no_p = percent(noSup/sum(c(yesSup, noSup)),accuracy=0.01)
no_p      #50.70% 


deg_supp=data.frame(Labels = c("Yes", "No"),
              Values = c(yesSup, noSup),
              Type = c(rep("Support", length(yesSup)), rep("No Support", length(noSup))))

ggplot(deg_supp, aes(x=Labels, y=Values, fill=Type)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=c(yesSup,noSup))) + 
  labs(title = "Effects on high Degree marks with or without family support",
       x = "Family Support", y = "Count") +
  scale_fill_manual(values = c("#607D8B", "#FF006E"))



#---Q2A9---------------------------------------------------------------------------
# DDDDDDDDDDDDDDDDDDELETEEEEEEEEEEEEEEEEEEEEEE
# MOTHER'S JOB AFFECTS DEGREE MARKS AND SPECIALISM 
mjob_degsp = sampleData %>%
  select(M_job, Degree_sp) %>%
  count(M_job, Degree_sp)
mjob_degsp

ggplot(mjob_degsp, aes(x = M_job, y = Degree_sp, size = n, color = M_job)) +
  geom_point() +
  scale_size(range = c(5, 15)) +
  geom_text(aes(label = n), size=4, color="black") +
  labs(title = "Relation of Mother's job and Degree Specialism",
       x = "Mother's Job", y = "Degree Specialism") +
  scale_color_manual(values = c("#F44336", "#F57C00","#FFEB3B","#40BF45","#2196F3")) 



#---Q2A10---------------------------------------------------------------------------
# FATHER JOB AND DEGREE SPECIALISM 
fjob_degsp = sampleData %>%
  select(F_job, Degree_sp) %>%
  count(F_job, Degree_sp)
fjob_degsp

ggplot(fjob_degsp, aes(x = F_job, y = Degree_sp, size = n, color = F_job)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(10, 20)) +
  geom_text(aes(label = n), size=4, color="black") +
  labs(title = "Relation of father's job and degree specialism",
       x = "Father's Job", y = "Degree Specialism") +
  scale_color_manual(values = c("purple", "orange","green","red","blue")) 






#**************** QUESTION 3 *******************************************************************************
#---Q3A1---------------------------------------------------------------------------
# Distribution of MBA%
mbaMarks = sampleData %>%
  select(MBA_p) %>%
  mutate(mba_marks = ifelse(MBA_p >= 75,"High MBA_p","Low MBA_p"))%>%
  count(mba_marks)
mbaMarks

pie(mbaMarks$n, labels=paste(mbaMarks$mba_marks,":",mbaMarks$n),
    main = "MBA Percentage Distribution", col=c("#ABB8C3","#00D084"), 
    clockwise=TRUE)


# Calculate percentage of High MBA_p
pct_high_mba = percent(mbaMarks$n[mbaMarks$mba_marks == "High MBA_p"]/sum(mbaMarks$n), accuracy = 0.01)
pct_high_mba


# Calculate percentage of Low MBA_p
pct_low_mba = percent(mbaMarks$n[mbaMarks$mba_marks == "Low MBA_p"]/sum(mbaMarks$n), accuracy = 0.01)
pct_low_mba



#---Q3A2---------------------------------------------------------------------------
# GENDER AND MBA MARKS
data1 = sampleData %>%
  select(Gender,MBA_p)%>%
  count(Gender,MBA_p)
data1

ggplot(data1, aes(x=MBA_p, y=n, fill=n)) +
  geom_bar(position="dodge", stat="identity") + geom_text(aes(label=n)) +
  labs(title = "Relationship Among Gender And MBA Marks ",
       x = "MBA Marks", y = "Frequency") +
  facet_wrap(~Gender)

summary(sampleData$MBA_p[sampleData$Gender=="M"])
summary(sampleData$MBA_p[sampleData$Gender=="F"])


#Get the row with highest value 
data_male = subset(data1,Gender=="M")
data_male

data_female = subset(data1,Gender=="F")
data_female


max_male = data_male[which.max(data_male$n),]
max_male    # M 85 224

max_female = data_female[which.max(data_female$n),]
max_female  # F 65 216



#---Q3A3---------------------------------------------------------------------------
#STUDENT AGE AND MBA MARKS 
data = sampleData %>%
  select(Age,MBA_p)%>%
  count(Age,MBA_p)
data 

ggplot(data,aes(x=MBA_p, y=n)) + 
  geom_point(aes(color=Age)) +
  scale_color_gradient("Age", low="green", high="red") + 
  labs(title = "Correlation Between Student Age and MBA Marks", 
       x = "MBA Marks in Percentage", y = "Frequency")  


#Average Marks for Age
sampleData %>%
  group_by(Age)%>%
  summarise(AverageMarks=mean(MBA_p))%>%
  arrange(desc(AverageMarks))

summary(sampleData$MBA_p)
summary(sampleData$MBA_p[sampleData$Age])



#---Q3A3-1---------------------------------------------------------------------------
# Filter to compare age group with MBA marks 
age_mbap1 = sampleData %>%
  select(Age, MBA_p) %>%
  filter(MBA_p >= 75 & Age >= 21) %>%
  count(Age, MBA_p)
age_mbap1

age_mbap2 = sampleData %>%
  select(Age, MBA_p) %>%
  filter(MBA_p >= 75 & Age < 21) %>%
  count(Age, MBA_p)
age_mbap2

t_above21 = sum(age_mbap1$n)
t_above21   #3838

t_below21 = sum(age_mbap2$n)
t_below21   #3909


#create lollipop chart
ageMBA = data.frame(
  ageGroup = c("Above (>=21)", "Below (<21)"),
  count = c(t_above21,t_below21)
)

ggplot(ageMBA, aes(x=ageGroup, y=count)) + 
  geom_segment(aes(x=ageGroup, xend=ageGroup, y=0, yend=count,color=ageGroup),
               linewidth=3) + 
  geom_point(size = 13, color ="#1A237E") +
  geom_text(aes(label = count), color="white") +
  labs(title = "Relationship Between High MBA marks and Age",
       x = "Age Group", y = "Number of Students Get Marks >= 75%") + 
  scale_color_manual(values = c("#FFC56F","#FF8A65")) 



#---Q3A4---------------------------------------------------------------------------
# ADDRESS AND MBA MARKS 
data2 = sampleData %>%
  select(MBA_p,Address)%>%
  count(MBA_p,Address)
data2

summary(sampleData$MBA_p[sampleData$Address=="R"])
summary(sampleData$MBA_p[sampleData$Address=="U"])

ggplot(sampleData, aes(x=Address, y=MBA_p, fill=Address)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "white", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 4, color = "black", show.legend = FALSE, 
               aes(label = round(..y.., digits = 2))) +
  labs(x='Address Area', y='MBA Percentage',
       title='Distribution of MBA Marks by Address') +
  scale_fill_manual(values = c("#00BCD4", "#FF6900"))
  
  

#---Q3A5---------------------------------------------------------------------------
# SECONDARY% AND MBA%
data3 = sampleData %>%
  select(MBA_p,Ssc_p)%>%
  count(MBA_p,Ssc_p)
data3

ggplot(sampleData,aes(x=MBA_p,y=Ssc_p)) + 
  geom_point(aes(shape=factor(Gender), colour=factor(Gender))) + 
  geom_smooth(mapping = aes(x=MBA_p, y=Ssc_p)) +
  labs(x='MBA Percentage', y='Secondary Percentage',
       title='Corresponding of MBA Marks and Secondary marks')

cor(sampleData$MBA_p,sampleData$Ssc_p)  
#0.001765331



#---Q3A6---------------------------------------------------------------------------
# HIGH DEGREE% AND MBA%
deg_mba = sampleData %>%
  select(Degree_p,MBA_p)%>%
  mutate(deg_mba = ifelse(Degree_p >= 75,"High Degree_p","Low Degree_p"))
  count(deg_mba,MBA_p)
deg_mba

h_deg_mba = deg_mba %>%
  select(deg_mba, MBA_p) %>%
  filter(deg_mba == "High Degree_p") %>%
  count(deg_mba, MBA_p)
h_deg_mba

ggplot(h_deg_mba,aes(x=MBA_p, y=n)) + 
  geom_point(aes(color=n)) +
  scale_color_gradient("Count", low="orange", high="blue") + 
  labs(title = "Correlation Between High Degree Marks and MBA Marks", 
       x = "MBA Marks in Percentage", y = "Frequency")  


#Number of students on MBA% >=75 or <75 
above75 = sum(h_deg_mba$n[h_deg_mba$MBA_p >= 75])  #3469
below75 = sum(h_deg_mba$n[h_deg_mba$MBA_p < 75])   #4156

View(data.frame(MBA_P = c(">=75", "<75"), 
           Total_n = c(above75, below75)))



#---Q3A7---------------------------------------------------------------------------
# MBA SPECIALISM AND MBA%
mba = sampleData %>%
  select(MBA_sp, MBA_p) %>%
  mutate(mba_marks = ifelse(MBA_p >= 75,"High MBA_p","Low MBA_p"))%>%
  count(mba_marks, MBA_sp)
mba

ggplot(mba, aes(x=n, y=mba_marks, fill=MBA_sp)) + 
  geom_col() + 
  geom_text(aes(label=n), color="white", position=position_stack(vjust=0.5)) +
  labs(x='Frequency', y='MBA Marks', 
       title='Relationship between MBA Specialism and MBA Percentage') +
  scale_fill_manual(values=c('#64B5F6', '#0D47A1')) +
  theme_light()



#---Q3A8---------------------------------------------------------------------------
# PAID COURSE AND MBA MARKS 
paid_mba = sampleData %>%
  select(Paid_Class, MBA_p) %>%
  count(Paid_Class, MBA_p)
paid_mba


summary(sampleData$MBA_p[sampleData$Paid_Class == "yes"])
summary(sampleData$MBA_p[sampleData$Paid_Class == "no"])

ggplot(sampleData, aes(x=Paid_Class, y=MBA_p, color=Paid_Class)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 4, color = "black", show.legend = FALSE, 
               aes(label = round(..y.., digits = 2))) +
  labs(x='Paid Class', y='MBA Percentage',
       title='Relationship between Paid Class and MBA Marks') +
  theme_bw()



#---Q3A9---------------------------------------------------------------------------
# INTERNET USAGE AND MBA MARKS 
summary(sampleData$MBA_p[sampleData$Internet == "yes"])
summary(sampleData$MBA_p[sampleData$Internet == "no"])

ggplot(sampleData,aes(x = Internet, y = MBA_p)) +
  geom_violin(aes(fill = Internet)) +
  scale_fill_manual(values=c("#4CAF50", "#2196F3"), name="Address", labels=c("no", "yes")) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "white", fill = "white") +
  stat_summary(fun = "mean", geom = "text", aes(label = paste0("Mean: ", round(..y.., 2))), vjust = -1.5) +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Internet Access", y = "MBA Percentage", title = "Relationship between Internet and MBA Marks") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#internet access VS MBA average score 
sampleData %>%
  group_by(Internet)%>%
  summarise(AverageMarks=mean(MBA_p))%>%
  arrange(desc(AverageMarks))



#---Q3A10---------------------------------------------------------------------------
# FATHER'S EDU LEVEL AND MBA MARKS
# Factor to convert values into categorical (DATA PRE-PROCESSING)
sampleData$F_edu_cat = factor(sampleData$F_edu, levels=0:4,
                              labels=c("0","1","2","3","4"))

fedu_mba = sampleData %>%
  select(F_edu_cat, MBA_p) %>%
  mutate(mba_marks = ifelse(MBA_p >= 75,"High MBA_p","Low MBA_p"))%>%
  count(mba_marks, F_edu_cat)
fedu_mba

ggplot(fedu_mba, aes(x=F_edu_cat, y=n)) + 
  geom_segment(aes(x=F_edu_cat, xend=F_edu_cat, y=0, yend=n,color=F_edu_cat),
               linewidth=3) + 
  geom_point(size = 13, color ="#455A64") +
  geom_text(aes(label = n), color="white") +
  labs(title = "Relationship Between Father's Education Level and MBA marks",
       x = "Father's Education Level", y = "Number of Students") + 
  scale_color_manual(values = c("#D32F2F","#FF6F00","#7B1FA2","#388E3C","#2196F3"))+
  facet_wrap(~mba_marks) 



#---Q3A11---------------------------------------------------------------------------
# MOTHER'S EDU LEVEL AND MBA MARKS
# Factor to convert values into categorical (DATA PRE-PROCESSING)
sampleData$M_edu_cat = factor(sampleData$M_edu, levels=0:4,
                              labels=c("0","1","2","3","4"))

medu_mba = sampleData %>%
  select(M_edu_cat, MBA_p) %>%
  mutate(mba_marks = ifelse(MBA_p >= 75,"High MBA_p","Low MBA_p"))%>%
  count(mba_marks, M_edu_cat)
medu_mba

ggplot(medu_mba, aes(x = M_edu_cat, y = n, size = n, color = M_edu_cat)) +
  geom_point() +
  scale_size(range = c(5, 10)) +
  geom_text(aes(label = n), size=4, color="white") +
  labs(title = "Relation of Mother's Education Level and MBA marks",
       x = "Mother's Education Level", y = "Frequency") +
  scale_color_manual(values = c("#0288D1","#7B1FA2","#303F9F","#0097A7","#D32F2F")) +
  facet_wrap(~mba_marks) +
  theme_dark()






#**************** QUESTION 4 *******************************************************************************
##---Q4A1---------------------------------------------------------------------------
#SALARY DISTRIBUTION
#DATA CLEANING FOR SALARY EMPTY VALUE
sampleData$Salary = ifelse(is.na(sampleData$Salary), "NA", sampleData$Salary)

salary_df = sampleData %>%
  select(Salary) %>%
  filter(Salary != "NA") %>% 
  count(Salary)
salary_df

ggplot(salary_df, aes(x = Salary, y = n)) +
  geom_bar(stat = "identity", fill = "#90A4AE") +
  geom_text(aes(label = n)) +
  labs(title = "Distribution of Salary", x = "Salary Amount", y = "Count") +
  theme_light()



#---Q4A2---------------------------------------------------------------------------
#AGE AND SALARY
# convert age to character value
sampleData$Age_cat = as.character(sampleData$Age)

age_df = sampleData %>%
  select(Age_cat,Salary)%>%
  filter(Salary != "NA") %>% 
  count(Age_cat,Salary)
age_df

#Stacked Bar Chart
ggplot(age_df, aes(x = Salary, y = n, fill = Age_cat)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5, reverse = TRUE)) +
  labs(title = "Distribution of Salary by Age Group", x = "Salary Amount", y = "Frequency") +
  scale_fill_manual(values=c("#FF2222", "#FF5A18", "#FCCB00", "#008B02", "#1273DE", "#7B1FA2")) +
  coord_flip() + 
  theme_light()



#---Q4A3---------------------------------------------------------------------------
#ADDRESS AND SALARY
address_df = sampleData %>%
  select(Address, Salary) %>%
  filter(Salary != "NA") %>%
  count(Address, Salary)
address_df

ggplot(address_df, aes(x = Salary, y = n, color = Address, group=Address)) +
  geom_line(aes(color=Address), size = 1) +  
  geom_point(aes(color=Address),size = 2) +  
  labs(title = "Distribution of Salary and Address",
       x = "Salary", y = "Frequency") +
  theme_bw() +  
  theme(legend.position = "top",  # Move the legend to the top
        plot.title = element_text(hjust = 0.5))  # Center the plot title

summary(sampleData$salary_num[sampleData$Address=="R"])
summary(sampleData$salary_num[sampleData$Address=="U"])



#---Q4A4---------------------------------------------------------------------------
#GENDER AND SALARY
gender_df = sampleData %>%
  select(Gender, Salary) %>%
  filter(Salary != "NA") %>%
  count(Gender, Salary)
gender_df

ggplot(gender_df, aes(x = Gender, y = Salary, size = n, color = Gender)) +
  geom_point(aes(color = Gender),alpha = 0.8) +
  geom_text(aes(label = n), size = 3.5, color = "black") +
  scale_size(range = c(1, 15)) +
  labs(title = "Bubble Chart for Gender and Salary",
       x = "Gender", y = "Salary",
       color = "Gender", size = "Frequency") 



#---Q4A5---------------------------------------------------------------------------
#EMPLOYMENT TEST AND SALARY
etest_df = sampleData %>%
  select(eTest_status, Salary) %>%
  filter(Salary != "NA") %>%
  count(eTest_status, Salary)
etest_df 

#Stacked Bar Chart
ggplot(etest_df, aes(x = Salary, y = n, fill = eTest_status)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.6, reverse = TRUE)) +
  labs(title = "Distribution of Salary by Employment Test", 
       x = "Salary Amount", y = "Frequency") +
  scale_fill_manual(values=c("#009688", "#F44336")) +
  theme_minimal()



#---Q4A6---------------------------------------------------------------------------
#DEGREE STREAM AND SALARY
# Convert Salary to numerical
sampleData$salary_num = as.numeric(as.character(sampleData$Salary))

degSp_df = sampleData %>%
  select(Degree_sp, salary_num) %>%
  filter(!is.na(salary_num)) %>% 
  count(Degree_sp, salary_num)
degSp_df


ggplot(sampleData, aes(x = Degree_sp, y = salary_num, fill=Degree_sp)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::label_number_si(scale = 1e-3, suffix = "k")) +
  labs(x = "Degree Course", y = "Salary",
       title="Relationship between degree course taken and salary") + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 4, color = "black", show.legend = FALSE, 
               aes(label = round(after_stat(y)))) +
  theme_bw() +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5)


summary(sampleData$salary_num[sampleData$Degree_sp=="Comm&Mgmt"])
summary(sampleData$salary_num[sampleData$Degree_sp=="Others"])
summary(sampleData$salary_num[sampleData$Degree_sp=="Sci&Tech"])


round(mean(sampleData$salary_num[sampleData$Degree_sp=="Sci&Tech"],na.rm = TRUE))


#---Q4A7---------------------------------------------------------------------------
# MBA SPECIALISM AND SALARY
mbaSp_df = sampleData %>%
  select(MBA_sp, salary_num) %>%
  filter(!is.na(salary_num)) %>% 
  count(MBA_sp, salary_num)
mbaSp_df

ggplot(mbaSp_df, aes(x = salary_num, y = n, color = MBA_sp)) +
  geom_line(size = 1) +  
  geom_point(size = 1.5) +  
  # Use k format for x-axis labels
  scale_x_continuous(labels = scales::label_number_si(scale = 1e-3, suffix = "k")) +
  labs(title = "Distribution of Salary and MBA Specialism",
       x = "Salary", y = "Frequency") +
  theme(legend.position = "top",  
        plot.title = element_text(hjust = 0.5)) +
  theme_light()

summary(sampleData$salary_num[sampleData$MBA_sp=="Mkt&Fin"])
summary(sampleData$salary_num[sampleData$MBA_sp=="Mkt&HR"])


#---Q4A8---------------------------------------------------------------------------
#MBA% AND SALARY 
mbap_df = sampleData %>%
  select(MBA_p, Salary) %>%
  filter(Salary != "NA") %>% 
  mutate(mba_marks = ifelse(MBA_p>=75,"High MBA_p","Low MBA_p"))%>%
  count(mba_marks, Salary)
mbap_df 

ggplot(mbap_df, aes(x=Salary, y=n, fill=mba_marks)) +
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label=n), position=position_dodge(0.9)) +
  labs(title = "Relationship Between MBA marks and Salary",
       x = "Salary", y = "Frequency") + 
  scale_fill_manual(values = c("#FF5722", "#0D47A1"))



#---Q4A9---------------------------------------------------------------------------
#MBA_SP AND MBA% AND SALARY
mba_df = sampleData %>%
  select(Degree_sp, MBA_sp, Salary) %>%
  filter(Salary != "NA") %>% 
  count(Degree_sp, MBA_sp,Salary)
mba_df 

ggplot(mba_df, aes(x = MBA_sp, y = Salary, size = n, color = MBA_sp)) +
  geom_point(aes(color = MBA_sp),alpha = 0.7) +
  geom_text(aes(label = n), size = 3.5, color = "black") +
  scale_size(range = c(2, 15)) +
  labs(title = "Connection among MBA and Degree Specialism with Salary",
       x = "MBA Specialism", y = "Salary",
       color = "MBA_sp", size = "Frequency") +
  scale_color_manual(values=c("#9C27B0","#A1887F")) +
  facet_wrap(~Degree_sp) + theme_light()



#---Q4A10---------------------------------------------------------------------------
#WORKING EXPERIENCE AND SALARY
workex_df = sampleData %>%
  select(Work_ex, salary_num) %>%
  filter(!is.na(salary_num)) %>% 
  count(Work_ex, salary_num)
workex_df

ggplot(workex_df, aes(x = salary_num, y = n, color = Work_ex)) +
  geom_point(size = 3) +
  geom_line(size = 1) + 
  scale_color_manual(values = c("red", "blue")) + 
  # Use k format for x-axis labels
  scale_x_continuous(labels = scales::label_number_si(scale = 1e-3, suffix = "k")) +
  labs(title = "Connection among Working experience and Salary",
       x = "Salary", y = "Frequency") +
  theme_light() 


summary(sampleData$salary_num[sampleData$Work_ex=="No"])
summary(sampleData$salary_num[sampleData$Work_ex=="Yes"])





#**************** QUESTION 5 *******************************************************************************
#---Q5A1-------------------------------------------------------------------------
# ACTIVITIES DISTRIBUTION 
activity_df = sampleData %>%
  select(Activities)%>%
  count(Activities)

# Get total students 
total = sum(activity_df$n)

# Calculate percentage% value 
activity_df$percentage = round(activity_df$n/total *100,2)
activity_df

pie(activity_df$n, labels=paste(activity_df$Activities,":",activity_df$percentage,"%"), 
    main="Activities Distribution",col=c("#FF5722","#4C4C4C"))



#---Q5A1-1-------------------------------------------------------------------------
# ACTIVITIES AND PLACEMENT STATUS 
act_stat = sampleData %>%
  select(Activities,Status)%>%
  count(Activities,Status)
act_stat 

ggplot(act_stat,aes(x = Activities, y = n, fill = Status)) + 
  geom_bar(stat = 'identity') + geom_text(aes(label=n)) +
  labs(x='Activities Participation', y='Frequency', 
       title='Correlation Between Placement Status and Activities') +
  scale_fill_manual(values=c('#FFEAA9', '#2D6686'), name='Status') +
  facet_wrap(~Status)    



#---Q5A1-2---------------------------------------------------------------------------
# ACTIVITIES AND SALARY
act_salary = sampleData %>%
  select(Activities, Salary) %>%
  filter(Salary != "NA") %>% 
  count(Activities, Salary)
act_salary

ggplot(act_salary, aes(x=Salary, y=n, color=Activities)) + 
  geom_segment(aes(x=Salary, xend=Salary, y=0, yend=n, color=Activities),
               linewidth = 1) + 
  geom_point(size = 2, color ="white") +
  geom_text(aes(label = n), color="black") +
  labs(title = "Relationship Between Salary and Activities",
       x = "Salary", y = "Frequency") + 
  scale_color_manual(values = c("red","purple"))+
  facet_wrap(~Activities) + coord_flip() +
  theme_bw()


#Calculate Salary mean value for Activities 
round(mean(sampleData$salary_num[sampleData$Activities=="yes"],na.rm = TRUE))
round(mean(sampleData$salary_num[sampleData$Activities=="no"],na.rm = TRUE))



#---Q5A2---------------------------------------------------------------------------
# PAID CLASS DISTRIBUTION 
paidClass = sampleData %>%
  select(Paid_Class)%>%
  count(Paid_Class)

# Get total students 
total = sum(paidClass$n)

# Calculate percentage% value 
paidClass$percentage = round(paidClass$n/total *100,2)
paidClass

pie(paidClass$n, labels=paste(paidClass$Paid_Class,":",paidClass$percentage,"%"), 
    main="Paid Class Distribution",col=c("#0D47A1","#00BCD4"), clockwise = TRUE)



#---Q5A2-1---------------------------------------------------------------------------
# PAID CLASS AND PLACEMENT STATUS 
paidC_stat = sampleData %>%
  select(Paid_Class, Status) %>%
  count(Paid_Class, Status)
paidC_stat

ggplot(paidC_stat, aes(x = Paid_Class, y = Status, size = n, color = Paid_Class)) +
  geom_point(aes(color = Paid_Class),alpha = 0.8) +
  geom_text(aes(label = n), size = 3.5, color = "black") +
  scale_size(range = c(10, 15)) +
  labs(title = "Bubble Chart for Paid Class and Status",
       x = "Paid_Class", y = "Status",
       color = "Paid_Class", size = "Frequency")



#---Q5A2-2---------------------------------------------------------------------------
# PAID CLASS AND SALARY 
sampleData %>%
  select(Paid_Class, salary_num) %>%
  filter(!is.na(salary_num)) %>% 
  count(Paid_Class, salary_num)

ggplot(sampleData, aes(x = Paid_Class, y = salary_num, fill=Paid_Class)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::label_number_si(scale = 1e-3, suffix = "k")) +
  labs(x = "Paid_Class", y = "Salary",
       title="Relationship between Paid Class and Salary") + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, 
               color = "white", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 4, 
               color = "black", show.legend = FALSE, 
               aes(label = round(after_stat(y)))) +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5) + 
  theme_bw()


summary(sampleData$salary_num[sampleData$Paid_Class=="no"])
summary(sampleData$salary_num[sampleData$Paid_Class=="yes"])


#Calculate Mean Value
round(mean(sampleData$salary_num[sampleData$Paid_Class=="yes"],na.rm = TRUE))
round(mean(sampleData$salary_num[sampleData$Paid_Class=="no"],na.rm = TRUE))



#---Q5A3---------------------------------------------------------------------------
# INTERNET DISTRIBUTION
internet_df = sampleData %>%
  select(Internet)%>%
  count(Internet)

# Get total students 
total = sum(internet_df$n)

# Calculate percentage% value 
internet_df$percentage = round(internet_df$n/total *100,2)
internet_df

pie(internet_df$n, labels=paste(internet_df$Internet,":",internet_df$percentage,"%"), 
    main="Internet Distribution",col=c("#333333","#9C27B0"), clockwise = TRUE)



#---Q5A3-1---------------------------------------------------------------------------
# INTERNET AND PLACEMENT STATUS
internet_stat = sampleData %>%
  select(Internet,Status)%>%
  count(Internet,Status)
internet_stat

ggplot(internet_stat, aes(x=Internet, y=n)) + 
  geom_segment(aes(x=Internet, xend=Internet, y=0, yend=n, color = Internet),
               linewidth = 2) + 
  geom_point(size = 11, color ="#1A237E") +
  geom_text(aes(label = n), color="white") +
  labs(title = "Relationship Between Internet and Placement Status",
       x = "Internet Access", y = "Frequency") + 
  scale_color_manual(values = c("#333333","#9C27B0"))+
  facet_wrap(~Status) 



#---Q5A3-2---------------------------------------------------------------------------
# INTERNET AND SALARY 
internet_salary = sampleData %>%
  select(Internet, salary_num) %>%
  filter(!is.na(salary_num)) %>% 
  count(Internet, salary_num)
internet_salary

ggplot(internet_salary, aes(x = salary_num, y = n, color = Internet)) +
  geom_point(size = 2) +
  geom_line(size = 1) + 
  scale_color_manual(values = c("#333333", "#9C27B0")) + 
  # Use k format for x-axis labels
  scale_x_continuous(labels = scales::label_number_si(scale = 1e-3, suffix = "k")) +
  labs(title = "Relationship between Internet and Salary",
       x = "Salary", y = "Frequency") +
  theme_light() 


summary(sampleData$salary_num[sampleData$Internet == "yes"])
summary(sampleData$salary_num[sampleData$Internet == "no"])




