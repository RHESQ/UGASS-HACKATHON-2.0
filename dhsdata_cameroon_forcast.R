###### DATA HACKATON 2.0 ######


library(readr)
library(dplyr) 
library(e1071) 
library(ggplot2) 
library(gmodels) 
library(ggthemes)  
library(ggrepel) 
library(gmodels) 
library(psych) 
library(rpart) 
library(rpart.plot) 
library(lsr)
library(data.tree)
library(xlsx)
library(FSelector)
library(caTools)
library(caret)
library(ElemStatLearn)
library(tidyr)

#system.setenv(JAVA_HOME = "c:\\Program Files\\Java\\jdk1.8.0_25\\jre")
#Sys.getenv(JAVA_HOME)

# importing the "dhsdata" to the global environment
dhsdata_dataset <- read.csv("dhsdata.csv", header = T, sep = ',')

#Extracting all the data concerning cameroon. 
cameroon <- filter(dhsdata_dataset, HV000 == "Cameroon (2011)")

glimpse(cameroon)

attach(cameroon)

#cleaning the cameroon dataframe:
#selecting meaningful datacolumns for prediction
main_cameroon <- select(cameroon, diarrhea, kidcurage, educlvl,hhWealth,
                        religion, hwbmizwhocat)

#clearing out all missing values in the dataset
main_cameroon <- drop_na(main_cameroon)

str(main_cameroon)
main_cameroon <- mutate(main_cameroon, diarrhea=factor(diarrhea), 
                        kidcurage=factor(kidcurage), educlvl=factor(educlvl), 
                        hhWealth=factor(hhWealth), religion=factor(religion), hwbmizwhocat=factor(hwbmizwhocat))




#Modeling
##partisioning the Dataset ~ train_set = 70% test_set = 30%
set.seed(12345)

splits <- sample.split(main_cameroon, SplitRatio = .70)
training_set <- subset(main_cameroon, splits == T)
test_set <- subset(main_cameroon, splits == F)

#building the decision tree classifier
#using diarrhea as the target variable and other 5 as the explanatory dataset
#Training the decision tree classifier
training_ClassificationTree <- rpart(diarrhea ~., 
                                     cp = 0.0012 ,data = training_set)

#predictions
training.ClassificationTree.diarrhea.predict <- predict(training_ClassificationTree, test_set, type = "class")
training.ClassificationTree.diarrhea.predict

#confusion matrix for evaluating the model
confusionMatrix(training.ClassificationTree.diarrhea.predict, test_set$diarrhea)

rpart.plot(training_ClassificationTree)



# DesCriptive Statistics
describe(f_HV025)
count(cameroon, HV025)
summary(count(cameroon, HV025))


#function for the mode
mode_function <- function(data_table){
  max_index <- which.max(data_table)
  names(data_table)[max_index]
}

#HV025
describe(f_HV025)
count(cameroon, HV025)
summary(count(cameroon, HV025))


factor(HV025)
t_HV025 <- table(HV025, useNA = "ifany")

f_HV025 <- as.data.frame(t_HV025)

f_HV025 <- f_HV025 %>%
  mutate(HV025, prob_HV025 = prop.table(t_HV025),
         perc_HV025 = prob_HV025 * 100)

attach(f_HV025)

mode_HV025 <- mode_function(data = t_HV025)
mode_HV025



# Basic piechart
ggplot(f_HV025, aes(x="", y = perc_HV025, fill = HV025)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  ggtitle("Plot of Paticipants Residency") +
  scale_color_discrete(name = "Residency") +
  theme_economist()



#kidcurage  
describe(f_kidcurage)
count(cameroon, kidcurage)
summary(count(main_cameroon, kidcurage))

factor(kidcurage)
t_kidcurage <- table(kidcurage, useNA = "ifany")

f_kidcurage <- as.data.frame(t_kidcurage)

f_kidcurage <- f_kidcurage %>%
  mutate(kidcurage, prob_kidcurage = prop.table(t_kidcurage),
         perc_kidcurage = prob_kidcurage * 100)

attach(f_kidcurage)

mode_kidcurage <- mode_function(data = t_kidcurage)
mode_kidcurage


# Basic piechart
ggplot(f_kidcurage, aes(x="", y = perc_kidcurage, fill = kidcurage)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  ggtitle("plot of Kids age") +
  theme_minimal()

#hwbmizwhocat
describe(f_hwb)
count(cameroon, hwbmizwhocat)
summary(count(main_cameroon, hwbmizwhocat))

factor(hwbmizwhocat)
t_hwb <- table(hwbmizwhocat, useNA = "ifany")

f_hwb <- as.data.frame(t_hwb)

f_hwb <- f_hwb %>%
  mutate(hwbmizwhocat, prob_hwb = prop.table(t_hwb),
         perc_hwb = prob_hwb * 100)

attach(f_hwb)

mode_hwb <- mode_function(data = t_hwb)
mode_hwb



# Basic piechart
f_hwb_plot <-
  ggplot(data =f_hwb, aes(x = hwbmizwhocat, y = Freq)) +
  geom_bar(stat = "identity", aes(fill = hwbmizwhocat)) +
  theme_economist()



#momAge
factor(momAge)
describe(f_momAge)
count(cameroon, momAge)
summary(count(cameroon, momAge))

t_momAge <- table(momAge, useNA = "ifany")

f_momAge <- as.data.frame(t_momAge)

f_momAge <- f_momAge %>%
  mutate(momAge, prob_momAge = prop.table(t_momAge),
         perc_momAge = prob_momAge * 100)

attach(f_momAge)

mode_momAge <- mode_function(data = t_momAge)
mode_momAge


# Basic histogram
ggplot() +
  geom_histogram(aes(x = momAge, y = ..density..),
                 colour = "blue") +
  ggtitle("A Histogram for the Mother's Ages") +
  theme_economist()

#association Between Education level and Wealth
eduwealth <- table(main_cameroon$educlvl, main_cameroon$hhWealth)
eduwealth
#graph_code

#diarrhea
factor(diarrhea)
t_diarrhea <- table(diarrhea, useNA = "ifany")

diarrheaFrame <- as.data.frame(t_diarrhea)

diarrheaFrame <- mutate(diarrheaFrame, prob_diarrhea = prop.table(t_diarrhea), perc_diarrhea = prob_diarrhea * 100)
mod_diarrhea <- mode_function(t_diarrhea)
mod_diarrhea

describe(f_diarrhea)
count(cameroon, diarrhea)
summary(count(cameroon, diarrhea))


attach(diarrheaFrame)
diarrheaFrame <- diarrheaFrame %>% 
  arrange(desc(diarrhea))

diarrheaFrame %>% 
  ggplot(aes(diarrhea, perc_diarrhea, label = diarrhea)) +
  geom_col(aes(col = diarrhea), size = 1,) +
  coord_polar("y", start = 0) +
  ylab("percentages of outcome") +
  ggtitle("A plot of the target variable") +
  scale_color_discrete(name = "Decision on diarrhea") +
  theme_economist()