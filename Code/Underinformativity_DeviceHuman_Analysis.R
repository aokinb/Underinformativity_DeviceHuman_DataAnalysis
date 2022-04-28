#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 1: Load necessary packages.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Load necessary packages
library(plyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 2: Load and organize data frame.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Load the cleaned data frame (created with 'Winter_2022_Underinformativeness_TopDown_Congruency_Gradience_Processing.R')
setwd("~/Desktop/UCDavis/PhoneticsLab/Winter_2022/Winter_2022_Underinformativeness_TopDown_Congruency_Gradience")
setwd("Data Frames")
final <- read.csv("Underinformativity_DeviceHuman.csv")

# Relabel the 'Furhat' and 'Cylinder' guises as 'Robot' and 'Device' respecively
final$guise <- as.character(final$guise)
final[final$guise == "Furhat", ]$guise <- "Robot"
final[final$guise == "Cylinder", ]$guise <- "Device"
final$guise <- as.factor(final$guise)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 3: Output summaries of the demographic variables.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Summary of the age variable
unique(final$Q19)
summary(as.numeric(as.character(final$Q19)))
sd(final$Q19)

# Summary of the gender variable
table(final$Q20)/114

# Summary of the first languages of the participants
table(as.character(final$Q21))/114

# Summary of the languages the participants spoke at home as children
table(as.character(final$Q22))/114

# Grouped summary of Q21 and Q22
table(final$Q21, final$Q22)/114

# Summary of the strongest language of the participants
table(as.character(final$Q23))/114

# Summary of the type of computer the study was done on
table(final$Q25)/114

# List of participants' comments 
unique(final$Q26)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 4: Visualize the data.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Refactor the 'guise' and 'type' variables
final$guise <- factor(final$guise, levels = c("Device", "Robot", "Human"))
final$type <- factor(final$type, levels = c("FA", "UI", "TA", "TS"))

# Find the mean rating for each type-guise combination
combined_summary_dat <- ddply(final,.(type, guise),summarize,
                              Mean=mean(na.omit(rating)),
                              Err=sqrt(var(na.omit(rating))/length(na.omit(rating))))
colnames(combined_summary_dat) <- c("Type", "Guise", "Mean", "Err")

# Find the mean rating for each type-guise combination for each participant
subject_summary_dat <- ddply(final,.(type, guise, subject),summarize,
                             Mean=mean(na.omit(rating)),
                             Err=sqrt(var(na.omit(rating))/length(na.omit(rating))))
colnames(subject_summary_dat) <- c("Type", "Guise", "Subject", "Mean", "Err")

# Create the plot for the manuscript
combined_plot <- ggplot(subject_summary_dat, 
                        aes(x=Type, y=Mean, color = Guise, group = Guise, shape =Guise, fill = Guise)) +
  geom_bar(position = position_dodge(), stat="identity", color = "black", data = combined_summary_dat) +
  geom_point(size=0.5, position = position_dodge(width = .9), shape = 21) +
  geom_errorbar(data = combined_summary_dat, aes(ymin=Mean-Err, ymax=Mean+Err), 
                color = "black", width=.2, position=position_dodge(width=.9)) +   
  coord_cartesian(ylim=c(1.18, 4.82)) +
  scale_color_manual(values=c("darkorange3", "deepskyblue4", "darkred")) +
  scale_fill_manual(values=c("orange", "deepskyblue3", "red")) + 
  xlab("Type") + ylab("Mean Rating") + 
  ggtitle("") + theme_bw() + 
  theme(axis.text=element_text(size=18), 
                  axis.title=element_text(size=20),
                  legend.text=element_text(size=18),
                  legend.title=element_text(size=20))
combined_plot
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Part 5: Run the statistical model.
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Recode the factor variables
final$guise <- factor(final$guise, levels = c("Human", "Device", "Robot"))
final$type <- factor(final$type, levels = c("UI", "FA", "TA", "TS"))

# Statistical model
model <- lmer(rating ~ type*guise + (1 + type + guise | subject) + (1 | sentence), data=final)
summary(model)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------