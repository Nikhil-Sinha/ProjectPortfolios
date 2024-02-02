library(tidyverse) # for data wrangling and visualization
library(dfidx) # for indexed data frame
library(mlogit) # for multinomial logit
library(ggplot2) # for data visualisation

# -------------------------------------------------------------------------------------------------
#Q1. Read and inspect the data set.

data <- read.csv("cloud.csv") # read cloud file into data
sapply(data,class)#print class (variable type)
summary(data)# print summary of data

# -------------------------------------------------------------------------------------------------
#Q2. Convert the attribute variables and set reference level

data <- mutate(data,cloud_storage = factor(cloud_storage))# convert cloud_storage to factor
data$cloud_storage <- relevel(data$cloud_storage, ref = '30gb') # set reference level 30gb

data <- mutate(data,price = factor(price)) # convert price to factor
data$price <- relevel(data$price, ref = 'p6') # set reference level to p6

sapply(data,class)#  print class of cloud_storage & price
levels(data$cloud_storage) # print level of cloud_storage
levels(data$price) # print level of price

# -------------------------------------------------------------------------------------------------
#Q3.Create a new variable price_n and find mean

data <- mutate(data,price_n = gsub("p", "", data$price)) # Create a new variable price_n copy data set from price removing character 'p' 
data <- mutate(data,price_n = as.numeric(price_n)) # turn the variable into numeric class

mean(data$price_n) # print mean of price_n

# -------------------------------------------------------------------------------------------------
#Q4 No of time repsondent chose 30GB cloud storage & percentage of respondents who chose email only as cloud service

NUM_30GB_CHOICES <- count(data,data$cloud_storage == "30gb"& data$choice == 1 ) # count all chice where cloud_storage =30gb & choice = 1 

NUM_30GB_CHOICES$n[2]# print count - ans for no of time repsondent chose 30GB cloud storage

data2 <-filter(data,cloud_services == "email"& data$choice == 1) # filter data into data 2 where cloud_services =email & choice = 1
NUM_30GB_CHOICES_RESPONDEDNT <-length(unique(data2$respondent_id)) # number of repondent who have choosen email as the cloud serice for at least one time
TOTAL_RESPONDENT <-length(unique(data$respondent_id))# number of total repondent 

PERCENTAGE = NUM_30GB_CHOICES_RESPONDEDNT/TOTAL_RESPONDENT*100 # Percentage of respondents who chose email only as cloud service
PERCENTAGE # print Percentage of respondents who chose email only as cloud service

# -------------------------------------------------------------------------------------------------
#Q5 Formatting data object into m_data

m_data <- dfidx(data, # the data set to use
                choice = "choice", # variable that contains choice
                idx = list(c("choice_id", "respondent_id"), # the two indexes (choice_id and respondent_id) that define unique obs
                           "alternative_id")) # the levels of the alternatives

m_data# print m_data

# -------------------------------------------------------------------------------------------------
#Q6 Multinomial logit model that predicts choice

set.seed(123) # Using random not as set.seed(123)

model1 <- mlogit(choice ~ 0 + cloud_storage + customer_support + cloud_services + price, 
                 data = m_data) # Creating model1 using multinomial logit model

summary(model1)$CoefTable # summary of model

# -------------------------------------------------------------------------------------------------
#Q7 Multinomial logit model using price_n instead of price

set.seed(123) # Using random not as set.seed(123)

model2 <- mlogit(choice ~ 0 + cloud_storage + customer_support + cloud_services + price_n, 
                 data = m_data) # Creating model2 using multinomial logit model

summary(model2)$CoefTable # summary of model


# -------------------------------------------------------------------------------------------------
#Q8 likelihood ratio test to test the model2 against model1

lrtest(model1,model2)

# -------------------------------------------------------------------------------------------------
#Q9 Using model2 to predict the choice probabilities for different alternatives

predicted_propabilities <- predict(model2, m_data) %>% 
  as_tibble() # choice probabilities for different alternatives


predicted_propabilities[1,3] # print the predictions of choosing the third alternative in the first choice set


# -------------------------------------------------------------------------------------------------
#Q10 predicted alternative in the third choice set


predicted_alternative <-
  predicted_propabilities %>% 
  rowid_to_column("choicesetid") %>% 
  pivot_longer(!choicesetid, names_to = "choice", values_to = "prob") %>% 
  group_by(choicesetid) %>% 
  slice(which.max(prob)) %>% 
  ungroup() %>% 
  select(choice) %>% 
  as_vector()


predicted_alternative[3] # print predicted alternative in the third choice set


# -------------------------------------------------------------------------------------------------
#Q11 selected alternative in the fifteenth choice set

selected_alternative <- 
  data %>% 
  filter(choice > 0) %>% 
  select(alternative_id) %>% 
  as_vector()

selected_alternative[15] # selected alternative in the fifteenth choice set?

# -------------------------------------------------------------------------------------------------
#Q12 Compute the confusion matrix for model2

table(selected_alternative, predicted_alternative) # print confusion matrix


# -------------------------------------------------------------------------------------------------
#Q13 Build a custom function to predict market share

# function 
predict.share <- function(model, d) {
  temp <- model.matrix(update(model$formula, 0 ~ .), data = d)[, -1] # generate dummy matrix
  u <- temp %*% model$coef[colnames(temp)] # calculate utilities
  probs <- t(exp(u) / sum(exp(u))) # calculate probabilities
  colnames(probs) <- paste("alternative", colnames(probs))
  return(probs)
}

# -------------------------------------------------------------------------------------------------
#Q14 Create a data object d_base

cloud_storage <- c("30gb","30gb","30gb","5000gb","5000gb")
customer_support <- c("no","no","yes","yes","no")
cloud_services <- c("email","email, video","email","email","email, video, productivity")
price_n <- c(6,12,12,18,18)

d_base <- data.frame(cloud_storage,customer_support,cloud_services,price_n)


d_base  # print d_base


# -------------------------------------------------------------------------------------------------
#Q15 predicted market share for hypothetical market


d_base <- cbind(d_base, as.vector(predict.share(model2, d_base))) # run custom function using model2 & d_base

colnames(d_base)[5] <- "predicted_share" # change the column name


d_base # print market share as per hypothetical market share

d_base[4,"predicted_share"] # print predicted market share for alternative four of this hypothetical market


# -------------------------------------------------------------------------------------------------
#Q16 predicted market share for new hypothetical market 

d_new <- d_base[c("cloud_storage","customer_support","cloud_services","price_n")] # make a copy of the d_base data
d_new[5, "cloud_services"] <- "email, video" # change fifth attribute 
d_new # print d_new

d_new <- cbind(d_new, as.vector(predict.share(model2, d_new))) # run custom function using model2 and new hypothetical market d_new

colnames(d_new)[5] <- "predicted_share_new" # change the column name

d_new # print market share as per new hypothetical market

d_new[4,"predicted_share_new"] # print market share for fourth alterntive as per new hypothetical market


# -------------------------------------------------------------------------------------------------
#Q17 Compare the market share after change

d_compare <- cbind(d_base[c("predicted_share")] , d_new[c("predicted_share_new")]) #combine predict_share and predict_share_new

alternative <-c("Alternative 1","Alternative 2", "Alternative 3","Alternative 4", "Alternative 5") # Naming the alternatives

percent_affected <- ((d_new$predicted_share_new-d_base$predicted_share)/d_base$predicted_share*100) # calculating percentage change in preicted share

d_compare<- data.frame(d_compare,percent_affected,alternative) # combining percentage with the orignal data

d_compare#print all data showing change in market share

maxd<-d_compare$alternative[which.max(abs(d_compare$percent_affected))] # alternative which is most affected by modification
maxd # print alternative which is most affected by modification
print(max(abs(d_compare$percent_affected))) #  Percentage affected for the alternative

d_compare_long <- pivot_longer(d_compare,cols =c("predicted_share","predicted_share_new"), names_to = "share") # Convert wide format to long format


ggplot(d_compare_long, aes(x = alternative, y = value, fill = share)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
    labs(x = "Alternative", y = "Predicted Share", title = "Grouped Bar Graph Comparing Predicted Share before and after modification") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), name = "") 


# -------------------------------------------------------------------------------------------------
#Q18

# Using the model2 coefficients to calculate the willingness to pay (in £ per month) for customer support. (customer support "no" is set as reference level)

-1* coef(model2)['customer_supportyes'] / coef(model2)["price_n"]

# -------------------------------------------------------------------------------------------------
#Q19

# Using the model2 coefficients to calculate the willingness to pay (in £ per month) for an upgrade from 30GB to 2000GB cloud storage. (30gb is set as reference level)

-1* coef(model2)['cloud_storage2000gb'] / coef(model2)["price_n"]

# -------------------------------------------------------------------------------------------------
#Q20

# Using the model2 coefficients to calculate the willingness to pay (in £ per month) for an upgrade from 2000GB to 5000GB cloud storage.

-1* (coef(model2)['cloud_storage5000gb'] - coef(model2)['cloud_storage2000gb']) / coef(model2)["price_n"]

