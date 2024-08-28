#### Importing Libraries ####
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(psych)
library(pastecs)

# Importing Dataset
nypd = read_csv(url("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv"))

#Let's check a summary of the data:

head(nypd) #Visualize the first rows and columns of the dataset

summary(nypd) #Statistical summary of the data

#Correcting time and date type
nypd <- nypd %>% mutate(OCCUR_DATE = mdy(OCCUR_DATE))
nypd <- nypd %>% mutate(HOUR = hour(hms(OCCUR_TIME)))

#Dropping columns we won't use for this analysis
nypd <- nypd %>% select(-c(PRECINCT,X_COORD_CD,Y_COORD_CD, Latitude, Longitude, Lon_Lat, LOC_CLASSFCTN_DESC, INCIDENT_KEY,JURISDICTION_CODE))

#Grouping by Borough (district) and the Victims' Sex, so we can get a count of cases
grouped = nypd %>% group_by(BORO,OCCUR_DATE, VIC_SEX) %>% summarise(total_count = n())

# Plotting to see in which borough the most shooting happened
grouped %>% 
  count(OCCUR_DATE, VIC_SEX, BORO) %>% 
  ggplot(aes(y = total_count, x = BORO, fill = interaction(VIC_SEX))) +
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(values = c("lightpink","lightblue","red"))+
  labs(y = "# of Shootings", x = "Borough", fill = "Victim's Sex")+
  theme_minimal()+
  theme(panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line.y = element_blank(),
        axis.line.x = element_line())
 
# We can see that Brooklyn and Bronx are the borough with the most reported shootings bias: 
# shooting should be reduced per population to mitigate possible bias, 
# as the higher population the more shooting is likely to happen. Additionally, we noticed that the vast majority
# of the victims are male.

#Now let's find out the demographics of our main victims.

victims = nypd %>% group_by(VIC_AGE_GROUP,VIC_RACE, VIC_SEX, STATISTICAL_MURDER_FLAG) %>% summarise(total_count = n())

#dropping the datapoint where there seems to have a typo in the age group, saying 1022

victims <- victims %>% filter(VIC_AGE_GROUP !=1022)

# Calculate the percentage of occurrences for each combination of VIC_RACE and VIC_AGE_GROUP

victims <- victims %>%
  mutate(total_vict = sum(victims$total_count))%>%
  mutate(percentage = total_count/total_vict *100)

#aggregating the percentage 
vic_dem <- aggregate(victims$percentage,by=list(Category=victims$VIC_RACE, victims$VIC_SEX), FUN=sum)

#Plotting the results
vic_dem %>% 
  count(Category, Group.2, x) %>% 
  ggplot(aes(y = vic_dem$x, x = vic_dem$Category, fill = interaction(vic_dem$Group.2))) +
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(values = c("lightpink","lightblue","red"))+
  labs(y = "% of Cases", x = "Victim's Race", fill = "Victim's Sex")+
  theme_minimal()+
  theme(panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(angle = 35, hjust = 1))


# As we can see, the vast majority of the victms are black, followed by White Hispanic and then Hispanic. We can also notice that most of the victms are men'''


#Diving deeper into the demographics of the victms, we can see that people from 18 to 44 are the most vulnerable to attacks
vic_dem2 <- aggregate(victims$percentage,by=list(Category=victims$VIC_AGE_GROUP, victims$STATISTICAL_MURDER_FLAG), FUN=sum)


#calculating percentage 
ind_sum <- aggregate(victims$total_count, by=list(victims$VIC_AGE_GROUP), FUN=sum)

total_dem <-aggregate(victims$total_count,by=list(Category=victims$VIC_AGE_GROUP, victims$STATISTICAL_MURDER_FLAG), FUN=sum) %>%
  mutate(total_dem_count = sum(x)) %>%
  mutate(percent_dem = x / ind_sum$x * 100)
  
total_dem <-total_dem%>%
  mutate(Group.2 = case_when(
    Group.2 == TRUE ~ "Death reported",
    Group.2 == FALSE ~ "No Death",
    TRUE ~ as.character(Group.2)  # Handle other cases if any
  ))

# Plotting the results
ggplot(total_dem, aes(x = Category, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +  # Adjust width as needed
  scale_fill_manual(values = c("No Death" = "#F7766D", "Death reported" = "#03BFC4")) +
  geom_text(aes(label = sprintf("%.1f%%", percent_dem),
                y = x / 2), 
            position = position_dodge(width = .9),  # Match the width with geom_bar
            size = 3) +
  labs(y = "% of Cases", x = "Age Group", fill = "Death Happened") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line())





#Now let's see what part of the day most of the cases happened.

#Getting the total count by hour for each borough
period_case <-nypd %>% group_by(HOUR,BORO) %>% summarise(total_count = n())

# Aggregate the total count by HOUR and BORO, so
time_case <- aggregate(total_count ~ HOUR + BORO, data = period_case, FUN = sum)

# Create hour labels in 12-hour format with "AM" and "PM" indicators
time_case$hour_label <- ifelse(time_case$HOUR < 12, paste0(time_case$HOUR, "AM"), 
                               ifelse(time_case$HOUR == 12, "12PM", paste0(time_case$HOUR - 12, "PM")))

#Ploting the results
time_case %>% 
  count(BORO, HOUR, total_count) %>% 
  ggplot(aes(y = total_count, x = HOUR, fill = interaction(BORO))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "# of cases", x = "Time of the Day", fill = "Borough") +
  scale_x_continuous(breaks = unique(time_case$HOUR), labels = unique(time_case$hour_label)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line.y = element_line(),   # Show y-axis line
        axis.line.x = element_line(),   # Show x-axis line
    axis.text.x = element_text(angle = 35, hjust = 1))

## Our analysis shows that the majority of the shootings happened between 6pm and 5am, with a significant increase after 2pm. The peak of the number of shootings happened between 10 and 11pm and  started to slowly decrease from that until 5am, when it dropped significantly decrease. 


##Further analysis to identify the reasons as for why the number of shootings drops so drastically at 5am and increases at 2pm, is required to understand the data. I could have incremented this analysis by diving deeper into the locations and conditions where the shootings happened, however, I found to be challenging since more than half of the data doesn't not contain the location that the shooting happened ("LOCATION_DESC" or even "LOC_CLASSFCTN_DESC"), thus not enough data was provide to support the analysis. 

#extracting the month and year of the dates

nypd$Month <- format(nypd$OCCUR_DATE, "%Y-%m")

#creating the dataset with the number of cases for each death toll
monthly_cases <- nypd %>%
  group_by(Month,STATISTICAL_MURDER_FLAG) %>%
  summarise(total_cases = n())

#Pivoting the results, so we can fit it to the model in a better way
separated_cases <- monthly_cases %>%
  pivot_wider(names_from = STATISTICAL_MURDER_FLAG, values_from = total_cases)
names(separated_cases)[2:3] <- c("death", "non_death")
#getting the total of cases, so we can better fit the model
separated_cases$total_cases <- separated_cases$death + separated_cases$non_death

# Fit a linear model to predict the total number of cases
lm_model <- lm(death ~ total_cases, data = separated_cases)

# View the summary of the linear model
summary(lm_model)
plot_lm <- plot(lm_model)
plot_lm[2]
