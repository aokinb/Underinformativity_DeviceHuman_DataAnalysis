#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 1: Load necessary packages and create a negation operator.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Load necessary packages
library(readxl)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

# Create a "not in" operator (to be used later on)
`%!in%` <- Negate(`%in%`)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 2: Clean the data frame from Qualtrics.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Load the csv file from Qualtrics
setwd("~/Desktop/UCDavis/PhoneticsLab/Winter_2022/Winter_2022_Underinformativeness_TopDown_Congruency_Gradience")
setwd("Data Frames")
df <- read.csv("Underinformativeness TopDownCongruencyGradience (Winter 2022)_March 3, 2022_00.46.csv")
df <- df[c(4:nrow(df)),]

# Select the columns with the ratings
test <- df[,c(22:61, 64:103, 106:745)]

# Create an empty vector where the inputted ratings will be placed
values <- c()

# Loop through each row (corresponding to each subject) and collect their responses
for (i in 1:nrow(df)) {
  print(i)
  temp <- test[i,]
  
  for (j in 1:ncol(temp)) {
    # Only select rows that are not empty
    if (as.character(temp[1, j]) != "") {
      rating <- as.numeric(as.character(temp[1, j]))
      values <- c(values, rating)
    }
  }
}

# Create empty vectors where subject, sheet, order, and question info will be placed
subject <- c()
sheet <- c()
order <- c()
question <- c()
Q19 <- c()
Q20 <- c()
Q21 <- c()
Q22 <- c()
Q23 <- c()
Q24 <- c()
Q25 <- c()
Q26 <- c()

# Loop through each row (corresponding to each subject)
for (j in 1:nrow(df)) {
  print(j)
  
  # For each subject, save their subject numbers, lists, and questions
  subject_list <- rep(as.character(j), 120)
  sheet_list <- rep(as.character(df[j,]$Sheet), 120)
  order_list <- rep(as.character(df[j,]$Order), 120)
  question_list <- as.character(1:120)
  Q19_list <- rep(as.character(df[j,]$Q19), 120)
  Q20_list <- rep(as.character(df[j,]$Q20), 120)
  Q21_list <- rep(as.character(df[j,]$Q21), 120)
  Q22_list <- rep(as.character(df[j,]$Q22), 120)
  Q23_list <- rep(as.character(df[j,]$Q23), 120)
  Q24_list <- rep(as.character(df[j,]$Q24), 120)
  Q25_list <- rep(as.character(df[j,]$Q25), 120)
  Q26_list <- rep(as.character(df[j,]$Q26), 120)
  
  # Add the subject numbers, lists, and questions to the empty vectors
  subject <- c(subject, subject_list)
  sheet <- c(sheet, sheet_list)
  order <- c(order, order_list)
  question <- c(question, question_list)
  Q19 <- c(Q19, Q19_list)
  Q20 <- c(Q20, Q20_list)
  Q21 <- c(Q21, Q21_list)
  Q22 <- c(Q22, Q22_list)
  Q23 <- c(Q23, Q23_list)
  Q24 <- c(Q24, Q24_list)
  Q25 <- c(Q25, Q25_list)
  Q26 <- c(Q26, Q26_list)
}

# Bind the created vectors within a new data frame
df1 <- as.data.frame(cbind(subject, question, sheet, order, values,
                           Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26))
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 3: Load the lists used for counterbalancing.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Load the 'Loop and Merge' lists that were used for counterbalancing
setwd("~/Desktop/UCDavis/PhoneticsLab/Winter_2022/Winter_2022_Underinformativeness_TopDown_Congruency_Gradience")
setwd("Loop and Merge")

# Load the 6 lists used for counterbalancing
sheet1_human <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet1.xlsx", sheet = "Human"))
sheet1_furhat <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet1.xlsx", sheet = "Furhat"))
sheet1_cylinder <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet1.xlsx", sheet = "Cylinder"))
sheet1 <- rbind(sheet1_human, sheet1_furhat, sheet1_cylinder)

sheet2_human <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet2.xlsx", sheet = "Human"))
sheet2_furhat <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet2.xlsx", sheet = "Furhat"))
sheet2_cylinder <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet2.xlsx", sheet = "Cylinder"))
sheet2 <- rbind(sheet2_human, sheet2_furhat, sheet2_cylinder)

sheet3_human <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet3.xlsx", sheet = "Human"))
sheet3_furhat <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet3.xlsx", sheet = "Furhat"))
sheet3_cylinder <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet3.xlsx", sheet = "Cylinder"))
sheet3 <- rbind(sheet3_human, sheet3_furhat, sheet3_cylinder)

sheet4_human <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet4.xlsx", sheet = "Human"))
sheet4_furhat <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet4.xlsx", sheet = "Furhat"))
sheet4_cylinder <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet4.xlsx", sheet = "Cylinder"))
sheet4 <- rbind(sheet4_human, sheet4_furhat, sheet4_cylinder)

sheet5_human <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet5.xlsx", sheet = "Human"))
sheet5_furhat <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet5.xlsx", sheet = "Furhat"))
sheet5_cylinder <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet5.xlsx", sheet = "Cylinder"))
sheet5 <- rbind(sheet5_human, sheet5_furhat, sheet5_cylinder)

sheet6_human <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet6.xlsx", sheet = "Human"))
sheet6_furhat <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet6.xlsx", sheet = "Furhat"))
sheet6_cylinder <- as.data.frame(read_excel("Underinformative_DeviceHuman_Sheet6.xlsx", sheet = "Cylinder"))
sheet6 <- rbind(sheet6_human, sheet6_furhat, sheet6_cylinder)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 4: Match the cleaned Qualtrics data frame (from Part 2) to the counterbalanced lists.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# For each of the 6 lists, create a data frame matching the type and guise for each question to the prior data frame
df_sheet1 <- df1 %>% filter(sheet=="1")
df_sheet1_num <- nrow(df_sheet1)/120
df_sheet1 <- cbind(df_sheet1, rep(sheet1$Sentence, df_sheet1_num), 
                   rep(sheet1$Type, df_sheet1_num),
                   rep(sheet1$Guise, df_sheet1_num))
colnames(df_sheet1) <- c("subject", "question", "sheet", "order", "rating", 
                         "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26",
                         "sentence", "type", "guise")

df_sheet2 <- df1 %>% filter(sheet=="2")
df_sheet2_num <- nrow(df_sheet2)/120
df_sheet2 <- cbind(df_sheet2, rep(sheet2$Sentence, df_sheet2_num), 
                   rep(sheet2$Type, df_sheet2_num),
                   rep(sheet2$Guise, df_sheet2_num))
colnames(df_sheet2) <- c("subject", "question", "sheet", "order", "rating", 
                         "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26",
                         "sentence", "type", "guise")

df_sheet3 <- df1 %>% filter(sheet=="3")
df_sheet3_num <- nrow(df_sheet3)/120
df_sheet3 <- cbind(df_sheet3, rep(sheet3$Sentence, df_sheet3_num), 
                   rep(sheet3$Type, df_sheet3_num),
                   rep(sheet3$Guise, df_sheet3_num))
colnames(df_sheet3) <- c("subject", "question", "sheet", "order", "rating",
                         "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26",
                         "sentence", "type", "guise")

df_sheet4 <- df1 %>% filter(sheet=="4")
df_sheet4_num <- nrow(df_sheet4)/120
df_sheet4 <- cbind(df_sheet4, rep(sheet4$Sentence, df_sheet4_num), 
                   rep(sheet4$Type, df_sheet4_num),
                   rep(sheet4$Guise, df_sheet4_num))
colnames(df_sheet4) <- c("subject", "question", "sheet", "order", "rating",
                         "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26",
                         "sentence", "type", "guise")

df_sheet5 <- df1 %>% filter(sheet=="5")
df_sheet5_num <- nrow(df_sheet5)/120
df_sheet5 <- cbind(df_sheet5, rep(sheet5$Sentence, df_sheet5_num), 
                   rep(sheet5$Type, df_sheet5_num),
                   rep(sheet5$Guise, df_sheet5_num))
colnames(df_sheet5) <- c("subject", "question", "sheet", "order", "rating",
                         "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26",
                         "sentence", "type", "guise")

df_sheet6 <- df1 %>% filter(sheet=="6")
df_sheet6_num <- nrow(df_sheet6)/120
df_sheet6 <- cbind(df_sheet6, rep(sheet6$Sentence, df_sheet6_num), 
                   rep(sheet6$Type, df_sheet6_num),
                   rep(sheet6$Guise, df_sheet6_num))
colnames(df_sheet6) <- c("subject", "question", "sheet", "order", "rating",
                         "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26",
                         "sentence", "type", "guise")

# Bind all of the lists together
final <- rbind(df_sheet1, df_sheet2, df_sheet3, 
               df_sheet4, df_sheet5, df_sheet6)

# Change the rating column to the numeric type
final$rating <- as.numeric(as.character(final$rating))

# Order data frame by subject
final <- final[order(as.numeric(as.character(final$subject))),]
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 5: Remove participants who answered one or more of the comprehension questions incorrectly.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Create data frames with the subjects' responses for each group of comprehension questions

# Questions 4a, 4b, and 4c correspond to the Human block.
intro_responses_q4 <- select(df, c("Q4a", "Q4b", "Q4c"))
rownames(intro_responses_q4) <- 1:length(unique(final$subject))

# Questions 5a and 5b correspond to the Robot block.
intro_responses_q5 <- select(df, c("Q5a", "Q5b"))
rownames(intro_responses_q5) <- 1:length(unique(final$subject))

# Questions 6a and 6b correspond to the Cylindrical Device block.
intro_responses_q6 <- select(df, c("Q6a", "Q6b"))
rownames(intro_responses_q6) <- 1:length(unique(final$subject))

# Eliminate participants who answered the questions incorrectly (as determined by the first author)
final <- final[final$subject %!in% c(5, 8, 9, 10, 18, 19, 20, 22, 25, 26, 27, 29, 30, 
                                     34, 45, 46, 50, 54, 55, 59, 62, 64, 68, 69, 76, 78, 
                                     83, 84, 86, 87, 90, 91, 103, 108),]
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 6: Remove participants who did not show a significant difference between the true and false sentences with 'all'.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Create a data frame with ratings by subject and type
rating_by_subject_type <- as.data.frame(final %>% group_by(subject, type) %>% summarize(mean(rating)))
colnames(rating_by_subject_type) <- c("Subject", "Type", "Rating")

# Empty vectors where subject numbers and p-values will go
subject_nums <- c()
type1_coef <- c()

# For each subject, run a simple linear regression (rating ~ type) on a subset of the data with only the TA and FA types.
# This checks whether the participants had a significant difference in rating between the TA and FA sentences.
for (i in unique(final$subject)) {
  subject_nums <- c(subject_nums, i)
  temp <- final[final$subject==i & final$type %in% c("TA", "FA"),]
  temp$type <- as.factor(as.character(temp$type))
  contrasts(temp$type) <- contr.sum(2)
  type1_coef <- c(type1_coef, summary(lm(rating ~ type, data=temp))$coefficients[,4][2])
}

# Create a new data frame with the subject numbers and p values
test <- data.frame(subject_nums, type1_coef)

# Add a column where the value is true if the p-value is less than 0.05 and false otherwise
test$tf <- test$type1_coef < 0.05
sort(as.numeric(as.character(test[test$tf == FALSE,]$subject_nums)))

# Remove participants who did not show a significant difference in rating between the TA and FA types 
final <- final[final$subject %!in% c(3, 4, 17, 32, 37, 38, 39, 61, 74, 82, 106, 109),]
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 7: Remove sentences that were outliers.

# Find the mean ratings for each sentence type
rating_by_type_question <- as.data.frame(final %>% group_by(sentence, type) %>% summarize(mean(rating)))
colnames(rating_by_type_question) <- c("Sentence", "Type", "Rating")

# Median Absolute Devian for the 'FA' (false with 'all') sentences
median(rating_by_type_question[rating_by_type_question$Type=="FA",]$Rating) - 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="FA",]$Rating)
median(rating_by_type_question[rating_by_type_question$Type=="FA",]$Rating) + 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="FA",]$Rating)
sort(rating_by_type_question[rating_by_type_question$Type=="FA",]$Rating)

# Median Absolute Devian for the 'UI' (underinformative) sentences
median(rating_by_type_question[rating_by_type_question$Type=="UI",]$Rating) - 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="UI",]$Rating)
median(rating_by_type_question[rating_by_type_question$Type=="UI",]$Rating) + 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="UI",]$Rating)
sort(rating_by_type_question[rating_by_type_question$Type=="UI",]$Rating)

# Median Absolute Devian for the 'TA' (true with 'all') sentences
median(rating_by_type_question[rating_by_type_question$Type=="TA",]$Rating) - 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="TA",]$Rating)
median(rating_by_type_question[rating_by_type_question$Type=="TA",]$Rating) + 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="TA",]$Rating)
sort(rating_by_type_question[rating_by_type_question$Type=="TA",]$Rating)

# Median Absolute Devian for the 'TS' (true with 'some') sentences
median(rating_by_type_question[rating_by_type_question$Type=="TS",]$Rating) - 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="TS",]$Rating)
median(rating_by_type_question[rating_by_type_question$Type=="TS",]$Rating) + 2.5*mad(rating_by_type_question[rating_by_type_question$Type=="TS",]$Rating)
sort(rating_by_type_question[rating_by_type_question$Type=="TS",]$Rating)

# Remove sentences that were outliers
final <- final[final$sentence %!in% c("All trees have leaves.",
                                      "Some greenhouses grow plants.",
                                      "Some birds can talk.",
                                      "Some gyms have machines.",
                                      "Some monkeys have two hands with thumbs.",
                                      "Some airports have security screening."),]
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 7: Clean the columns corresponding to the demographic information.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Summary of the age variable
unique(final$Q19)
summary(as.numeric(as.character(final$Q19)))

final$Q20 <- as.character(final$Q20)
final$Q21 <- as.character(final$Q21)
final$Q22 <- as.character(final$Q22)
final$Q23 <- as.character(final$Q23)

# Summary of the gender variable
table(final$Q20)/114
final$Q20 <- ifelse(final$Q20 == "nonbinary" | final$Q20 == "Gender Fluid" | final$Q20 == "gender", "nonbinary/declined", 
                    ifelse(final$Q20 == "Make" | final$Q20 == "male" | final$Q20 == "Male" | final$Q20 == "Cis Male", 
                           "male", "female"))
table(final$Q20)/114

# Summary of the first languages of the participants
table(as.character(final$Q21))/114
final[final$Q21 == "spanish",]$Q21 <- "Spanish"
final[final$Q21 == "Egnlish",]$Q21 <- "English"
final[final$Q21 == "english",]$Q21 <- "English"
final[final$Q21 == "english ",]$Q21 <- "English"
final[final$Q21 == "English ",]$Q21 <- "English"
table(as.character(final$Q21))/114

# Summary of the languages the participants spoke at home as children
table(final$Q22)/114
final[final$Q22 == "n/a",]$Q22 <- "None"
final[final$Q22 == "N/A",]$Q22 <- "None"
final[final$Q22 == "none",]$Q22 <- "None"
final[final$Q22 == "Bulgarian (spoken to me)",]$Q22 <- "Bulgarian"
final[final$Q22 == "cantonese",]$Q22 <- "Cantonese"
final[final$Q22 == "chinese",]$Q22 <- "Chinese"
final[final$Q22 == "chinese ",]$Q22 <- "Chinese"
final[final$Q22 == "English ",]$Q22 <- "English"
final[final$Q22 == "Farsi and French ",]$Q22 <- "Farsi and French"
final[final$Q22 == "gujrati",]$Q22 <- "Gujarati"
final[final$Q22 == "just english",]$Q22 <- "English"
final[final$Q22 == "korean",]$Q22 <- "Korean"
final[final$Q22 == "None, just English.",]$Q22 <- "English"
final[final$Q22 == "only English",]$Q22 <- "English"
final[final$Q22 == "Only English",]$Q22 <- "English"
final[final$Q22 == "some spanish",]$Q22 <- "Spanish"
final[final$Q22 == "spanish",]$Q22 <- "Spanish"
final[final$Q22 == "spanish ",]$Q22 <- "Spanish"
final[final$Q22 == "telugu",]$Q22 <- "Telugu"
final[final$Q22 == "Urdhu",]$Q22 <- "Urdu"
table(as.character(final$Q22))/114

# Grouped summary of Q21 and Q22
table(final$Q21, final$Q22)/114

# Summary of the strongest language of the participants
table(as.character(final$Q23))/114
final[final$Q23 == "Egnlish",]$Q23 <- "English"
final[final$Q23 == "english",]$Q23 <- "English"
final[final$Q23 == "english ",]$Q23 <- "English"
final[final$Q23 == "English ",]$Q23 <- "English"
final[final$Q23 == "English and Spanish ",]$Q23 <- "English and Spanish"
table(as.character(final$Q23))/114

# Summary of the type of computer the study was done on
table(final$Q25)/114

# List of participants' comments 
unique(final$Q26)

# Write the data frame to a csv file
write.csv(final, "~/Desktop/Underinformativity_DeviceHuman.csv")
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------