#################################
########### ECON 494 ############
# DESCRIPTIVE ANALYTICS PROJECT #
######## NOELLE FREEBURG ########

#GETTING & CLEANING PROJECT DATA#

#1. GETTING: 
install.packages("readxl") #install package to allow R to read Excel files
library("readxl") #load readxl package
my_data<-read_excel("Analytics Project Data.xlsm") #import data
names(my_data) #view variable names to confirm they are correct
View(my_data) #view dataset 
summary(my_data) #view summary statistics
write.table(my_data, "raw_data")

#2. CLEANING:
na.omit(my_data) #omit NA values from dataset
omitted_data<-na.omit(my_data) #name cleaned dataset omitted_data
#lost 11 observations from the original dataset 

#3. SAVING CLEANED DATA
write.csv(omitted_data, "TIDY_data") #save ommitted_data as csv named "clean data"

######## VISUALIZATIONS #########

#INSTALLING & LOADING GGPLOT2#
install.packages("ggplot2")
library(ggplot2)

#GDP & GINI SCATTER PLOT#
gdp_gini_plot <- ggplot(omitted_data, aes(x=omitted_data$`Avg GDP/capita 2008-2018`,y=omitted_data$`Avg Gini 2008-2018`)) + 
  geom_point() 
gdp_gini_plot <- gdp_gini_plot + ggtitle("Average GDP Per Capita and Gini Index from 2008-2018") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Average GDP Per Capita ($)") + 
  ylab("Average Gini (%)") + 
  geom_smooth(method = "lm")
print(gdp_gini_plot)

#GINI & ECONOMIC STATUS BAR CHART#
gini_status_plot <- ggplot(omitted_data, aes(x=Status, y=omitted_data$`Avg Gini 2008-2018`/100)) + 
  geom_bar(stat='Identity', width = 0.5)  
gini_status_plot <- gini_status_plot + ggtitle("Average Gini Index 2008-2018: Developed vs Developing") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Economic Status ($)") + 
  ylab("Average Gini (%)") 
print(gini_status_plot)

#ECONOMIC STATUS RATIO PIE CHART#

#1. CREATING NEW DATA FRAME:
pie_data<- data.frame(
  group = ("Status" = c("Developed", "Developing")),
  value=c(52,101))
View(pie_data)

#2. CREATING PIE CHART
pie <- ggplot(pie_data, aes(x="", y=value, fill=group)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = paste0(round(value)), "%"), position = position_stack(vjust = 0.5))
pie <- pie + ggtitle("Developed vs. Developing Countries") +
  theme(plot.title = element_text(hjust = 0.5))
print(pie)

#GINI & INCOME SHARE OF RICHEST 20% SCATTER PLOT#
gini_richest_plot <- ggplot(omitted_data, aes(x=omitted_data$`Avg Income Share of Richest 20% 2008-2018`,y=omitted_data$`Avg Gini 2008-2018`)) + 
  geom_point()
gini_richest_plot <- gini_richest_plot + ggtitle("Income Share of the Richest 20% and the Gini Index") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Income Share of the Richest 20% (%)") + 
  ylab("Average Gini (%)") + 
  geom_smooth(method = "lm")
print(gini_richest_plot)

#GINI & INCOME SHARE OF POOREST 20% SCATTER PLOT#
gini_poorest_plot <- ggplot(omitted_data, aes(x=omitted_data$`Avg Income Share of Poorest 20% 2008-2018`, y=omitted_data$`Avg Gini 2008-2018`)) + 
  geom_point()
gini_poorest_plot <- gini_poorest_plot + ggtitle("Income Share of the Poorest 20% and the Gini Index") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Income Share of the Poorest 20% (%)") + 
  ylab("Average Gini (%)") + 
  geom_smooth(method = 'lm')
print(gini_poorest_plot)




