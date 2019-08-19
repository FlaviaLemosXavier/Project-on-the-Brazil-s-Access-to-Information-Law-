# Assure all required packages are installed properly

if(!require(kableExtra)) install.packages("kableExtra", 
                                          repos = "http://cran.us.r-project.org", 
                                          dependencies = TRUE)

if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org", 
                                     dependencies = TRUE)

if(!require(gridExtra)) install.packages("gridExtra", 
                                         repos = "http://cran.us.r-project.org", 
                                         dependencies = TRUE)

if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org", 
                                         dependencies = TRUE)

# import all required packages for projects
library(tidyverse)
library(gridExtra)
library(lubridate)
library(knitr)
library(kableExtra)
library(caret)
library(data.table)
library(dplyr)
library(readxl)
library(readr)

####################
# 1. INTRODUCTION
####################

## 1.3 Data overview

# Download the Requests for Access to Information in 2019 and and remove id requesters = 0 to start our analysis:


url<- "https://raw.githubusercontent.com/FlaviaLemosXavier/Project-on-the-Brazil-s-Access-to-Information-Law-/master/20190814_Requests_csv_2019.csv"


data <- read_csv(url)
data<-data%>%filter(!data$'Id do Solicitante'== 0)
data<- as_tibble(data)


head(data) %>%  kable(caption = "Top rows of data file") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down", "HOLD_position"))

class(data)


dim(data)
str(data)

#let's create a summary table:

data_summary <- data.frame(number_of_rows = nrow(data),
                          number_of_column = ncol(data),
                          number_of_different_requesters = n_distinct(data$`Id do Solicitante`),
                          number_of_different_subjetcs = n_distinct(data$`Categoria do Pedido`),
                          number_of_different_decisions = n_distinct(data$`Tipo de resposta`))

data_summary %>% 
  kable(caption = "Summary of data (part 1)") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

#see we have 3 columns ('IdRequester'='Id do Solicitante','Subject' ='Categoria do Pedido','Administrative Decisions' = 'Tipo de resposta') 
#and 36,417 rows (each row is a request for access to information made until August 15, 2019.).

names(data)
str(data)

#Let's look at the distribution of requests by subject:

data %>% group_by(`Categoria do Pedido`)%>%
  dplyr::count( )%>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Distribution of Requests by Subject")

top_5 <- data %>% group_by(`Categoria do Pedido`) %>% summarize(count = n())%>% 
  arrange(desc(count))%>% top_n(5)

top_5%>% 
  kable(caption = "Ranking of the most requested subjects") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

last_5 <- data %>% group_by(`Categoria do Pedido`) %>% summarize(count = n())%>% 
  arrange(desc(count))%>% tail(5)

last_5%>% 
  kable(caption = "Ranking of the least requested subjects") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

# Decision Distribution:

decisions<-data %>% group_by(`Tipo de resposta`) %>% summarize(count = n())

decisions%>%
  ggplot(aes(x = `Tipo de resposta`, y = count)) +
  geom_col() + 
  ggtitle("Decision Distribution")


#Note that our database has 3 different types of governing body decisions ("Tipo de resposta"): 
# y= "Acesso concedido", "Acesso Negado" and "Acesso Parcialmente Concedido".
#It means: "Success", "Failure" and "Partial success". 
# we will adapt "y" to a binary format, to facilitate fitting our first model development 
#Now I need to incorporate dummy variables from my model as follows. For each denied decision, I associate 0 and 1 if the decision is favorable or partially favorable.

data<- select(data,"RequestCategory"='Categoria do Pedido','Decision'='Tipo de resposta')
view(data)


data$Decision[data$Decision == "Acesso Parcialmente Concedido"] <- "Acesso Concedido"

data<-data %>% 
  mutate(Decision = factor(Decision, levels = c("Acesso Concedido", "Acesso Negado" ))) 


levels(data$Decision)


## Create Test and Train Data Sets

#I will split data into a training and test/validation set to develop the model.
#Following previous courses recommendation, the data set `train_set` will contain 80% of the available data. This data set will be used to train any model. To test and evaluate these models, the data set `test` with about 20% of the available data will be used.


set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$Decision, times = 1, p = 0.5, list = FALSE)
train <- data %>% slice(-test_index)
temp <- data %>% slice(test_index) 

#Make sure RequestCategory in validation set are also in train set

validation <- temp %>% 
  semi_join(data, by = "RequestCategory") 
validation <-validation [-1,]

# Add rows removed from validation set back into train set

removed <- anti_join(temp, validation)
train<- rbind(train, removed)

y <-train$Decision 
x <- train$RequestCategory

#let’s provide a prediction for a request on Public Administration. What is the conditional probability of being favorable decision if the request on Public Administration?

train %>% 
  filter(RequestCategory == 'Administracao publica') %>%
  summarize(y_hat = mean(Decision== "Acesso Concedido"))

#To construct a prediction algorithm, we want to estimate the proportion of the favorable decision for any given request category X=x, which we write as the conditional probability described above: 

data %>% 
  group_by(RequestCategory) %>%
  filter(n() >= 100) %>%
  summarize(prop = mean(Decision == "Acesso Concedido")) %>%
  ggplot(aes(RequestCategory, prop)) +
  geom_point()+
  ggtitle("Proportion of a favorable decision by Request Category")

lm_fit <- mutate(train, y = as.numeric(y == "Acesso Concedido")) %>% lm(y ~ x, data = .)
head(lm_fit$coefficients)
p_hat <- predict(lm_fit, validation)
plot(p_hat) 
y_hat <- ifelse(p_hat > 0.5, "Acesso Concedido", "Acesso Negado")%>%as.factor() 
plot(y_hat)
plot(validation$Decision)
length(y_hat)
length(validation$Decision)
confusionMatrix(y_hat, validation$Decision)$overall["Accuracy"]

#Note that we have defined a variable x that is predictive of a binary outcome y
train %>% ggplot(aes(x, color = y)) + geom_density()


#We see this method does substantially better than guessing.

glm_fit <- train %>% mutate(y = as.numeric(y == "Acesso Concedido")) %>%glm(y ~ x, data = ., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = validation, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.7, "Acesso Concedido", "Acesso Negado") %>% factor
plot(y_hat_logit)
plot(validation$Decision)
confusionMatrix(y_hat_logit, validation$Decision)[["Accuracy"]]


