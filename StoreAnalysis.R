#Marketing Analytics Project 1

#Following are the libraries that need to be downloaded to conduct the upcoming tasks
library(tidyverse) # for data tidying and visualization
library(NbClust) # for determining the best number of clusters and
library(janitor) # The Janitor library is intended to make data cleaning and organisation tasks simpler and more efficient.
library(flexclust) # The flexclust package is intended to provide flexible and powerful clustering analysis tools, for segment profile plot

# -------------------------------------------------------------------------------------------------
#Q1. Read and inspect the data set.

data <- read.csv("office.csv") #upload data file
glimpse(data) #displays a brief summary of a data frame's structure
summary(data) #provides overall summary of the data 


# -------------------------------------------------------------------------------------------------
#Q2. standardize the data set , scale value in row
data2 <- scale(data[, c("variety_of_choice",
                             "electronics",
                             "furniture",
                             "quality_of_service",
                             "low_prices",
                             "return_policy"
)]) %>%
  as_tibble() # convert to tibble

summary(data2) #summary of standardized data
min_min <-  which.min(apply(data2, 2, min)) # variable which has the maximum value in the normalized data set
max_var <- which.max(apply(data2, 2, max))# variable which has the minimum value in the normalized data set
colnames(data2)[min_min]# print min variable 
colnames(data2)[max_var] #print max variable

# -------------------------------------------------------------------------------------------------
#Q3.  hierarchical clustering


#step 1 Compute Euclidean distance
# compute distances

dist <- dist(data2,
             method = "euclidean") # use Euclidean

as.matrix(dist)[1:10, 1:10]


set.seed(123) # for repreducibility

hc <- hclust(dist, method = "ward.D2") # run the algorithm and store the result to an object

hc # print the cluster object

plot(hc) # plot the dendogram



# -------------------------------------------------------------------------------------------------
#Q4. Create 6-cluster solution

# draw rectangles around dendrogram
# highlighting the 6 clusters purple
rect.hclust(hc, k = 6, border = "purple") 

# creates a six cluster solution
hc6 <- cutree(hc, k = 6) 

table(hc6) #show values in each cluster

# -------------------------------------------------------------------------------------------------
#Q5. Generate a segment profile plot

data2 %>% #take data.frame standardized data
  mutate(hc6 = factor(hc6)) %>% #add the cluster assignment
  group_by(hc6) %>% #cluster group
  mutate(n = n()) %>%  #used to calculate number per group
  summarise_all(~ mean(.x)) %>%  #calculate mean 
  mutate(prop = n/sum(n)) %>%  #used to calculate prop
  print(width = Inf) 

hc6_flex <- as.kcca(hc, data2, k = 6) #convert hclust to kcca

barchart(hc6_flex) #create bar chart

table(hc6, clusters(hc6_flex)) #to check concordance between as.kcca and cutree


# -------------------------------------------------------------------------------------------------
#Q6. AND EXPLANATION IN REPORT

# -------------------------------------------------------------------------------------------------
#Q7. Comment on cluster 6 

#Cluster 5
plot(hc)
rect.hclust(hc, k = 5, border = "purple") 

# creates a six cluster solution
hc5 <- cutree(hc, k = 5) 

table(hc5) #show vaow values in each cluster


data2 %>% #take data.frame standardized data
  mutate(hc5 = factor(hc5)) %>% #add the cluster assignment
  group_by(hc5) %>% #cluster group
  mutate(n = n()) %>%  #used to calculate number per group
  summarise_all(~ mean(.x)) %>%  #calculate mean 
  mutate(prop = n/sum(n)) %>%  #used to calculate prop
  print(width = Inf)

#Cluster 4
# draw rectangles around dendrogram
# highlighting the 4 clusters purple
plot(hc)

rect.hclust(hc, k = 4, border = "purple") 

# creates a six cluster solution
hc4 <- cutree(hc, k = 4) 

table(hc4) #show vaow values in each cluster


data2 %>% #take data.frame standardized data
  mutate(hc4 = factor(hc4)) %>% #add the cluster assignment
  group_by(hc4) %>% #cluster group
  mutate(n = n()) %>%  #used to calculate number per group
  summarise_all(~ mean(.x)) %>%  #calculate mean 
  mutate(prop = n/sum(n)) %>%  #used to calculate prop
  print(width = Inf) 


# -------------------------------------------------------------------------------------------------
#Q8. Generate a 5-cluster

# plot the dendogram
plot(hc)

# draw rectangles around the branches of a dendrogram 
# highlighting the corresponding clusters
rect.hclust(hc, k = 5, border = "red")

# creates a four cluster solution
hc5 <- cutree(hc, k = 5) 

table(hc5)


# -------------------------------------------------------------------------------------------------
#Q9.
data2 %>% 
  mutate(hc5 = factor(hc5)) %>% 
  group_by(hc5) %>% 
  mutate(n = n()) %>% 
  summarise_all(~ mean(.x)) %>%  
  mutate(prop = n/sum(n)) %>%  
  print(width = Inf) 

hc5_flex <- as.kcca(hc, data2, k = 5)

barchart(hc5_flex)

table(hc5, clusters(hc5_flex))

hc5 <- factor(hc5, 
              levels = c(1, 2, 3,4,5),
              labels = c("Diversity HC", "Affordability HC", "Customer Service HC", "Product Quality HC", "Consumer Friendly HC"))


# -------------------------------------------------------------------------------------------------
#Q10.

set.seed(123) # for reproducibility
NbClust(data = data2[, 1:5], # use 5 not all 6 attributes 
        min.nc = 4, # min number of clusters
        max.nc = 15, # max number of clusters
        index = "all", # use all indexes
        method = "ward.D2" # minimize the within-cluster variance
)$Best.ncBest #  print only the number of clusters proposed


# -------------------------------------------------------------------------------------------------
#Q11.
# Including demographic variables
data <- data %>% mutate(hc5 = hc5)


# made age group for age more than or less than 30

data$age_group <- ifelse(data$age < 30, "Age less than 30","Age more than 30")

# made income group for income more than or less than 20
data$income_group <- ifelse(data$income < 20, "Income less than 20","Income more than 20")


data %>% #FOR PROFESSIONAL DATA 
  tabyl(hc5, professional) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

data %>% #FOR INCOME
  tabyl(hc5, income_group) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

data %>% #FOR AGE
  tabyl(hc5, age_group) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# -------------------------------------------------------------------------------------------------
#Q12.
# k-means algorithm

set.seed(123)

km <- kmeans(data2,
             centers = 5,
             iter.max = 1000,
             nstart = 100) # run the kmeans function, 5 is number of cluster
km

# -------------------------------------------------------------------------------------------------
#Q13. comparsion Concordance between hclust and kmeans

km5 <- factor(
  km$cluster,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Diversity KM", "Affordability KM", "Customer Service KM", "Product Quality KM", "Consumer Friendly KM"))

data <- data %>% mutate(km5 = km5)

data %>%
  tabyl(km5, hc5) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 



# Hit rate
hf <- data %>%
  tabyl(km5, hc5) %>% 
  as.data.frame()

(60+17+33+29+59)/200



