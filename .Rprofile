install.packages("tidyverse")
library(tidyverse)

  
# Use the function `read_csv()` to read the file 
# only the path to the folder 'data' is provided since the entire >pipeline is organized in a 'Project'
routine_data <- read_csv("raw_data/routine_data.csv")
population <- read_csv("raw_data/population.csv")

# To see the full path use the function `getwd()`

# Explore the data (call the object/name assigned) using functions `str()`, `head()` and `summary()`

# Tabulate unique records for adm1, adm2 and year - note of various ways to get the output
with(routine_data, table(unique(adm1)))
unique(routine_data$adm2)
with(routine_data, table(unique(year)))
##for checking the occurence of the variable remove the unique or use the group by function
with(routine_data, table(unique(year)))
routine_data %>% group_by(year) %>% count(year)

## Convert the variable "month" to an ordered factor
routine_data$month <- factor(routine_data$month, levels = month.abb) # Note: There is in R a variable called month.abb

## Clean the names of adm1, year records, create date variable 
cleaned_routine<- routine_data %>% 
  mutate(adm1 = recode(adm1, "N. Coast" = "North Coast", "central" = "Central"),
         year = recode(year, '3018' = 2018, 
                       '18' = 2018)) %>%  
  unite(date, year, month, sep = "-", remove = F) %>% 
  mutate(date = ymd(parse_date_time(date, "ym")))

## Clean the missing values to have a common format (*-9999* or *NA*)
cleaned_routine$test_u5[cleaned_routine$test_u5 == -9999] <-NA

## Save the file as new data with name *routine_data_clean.csv* using the `write_csv()` function
write_csv(cleaned_routine, "raw_data/cleaned_routine.csv")



#plotting scatter plot fot test u5 and conf u5
ggplot(data = cleaned_routine, aes(x=test_u5, y=conf_u5, color= month)) +
  geom_point()

# Main file 
cleaned_routine <- cleaned_routine %>% 
  rowwise() %>% 
  mutate(total_tested = sum(test_u5, test_ov5, na.rm = TRUE),
         total_conf = sum(conf_u5, conf_ov5, na.rm = TRUE ))

# Quick plots 
boxplot(cleaned_routine$total_conf ~ cleaned_routine$adm1)


# Aggregate at adm 1 and save the output file
data_adm1 <- cleaned_routine %>% 
  group_by(adm1) %>% 
  summarise(total_tested = sum(test_u5, test_ov5, na.rm = TRUE),
            total_conf = sum(conf_u5, conf_ov5, na.rm = TRUE ))

write_csv(data_adm1, "raw_data/aggreg_adm1.csv")

# Aggregate at adm 2 and save the output file
data_adm2 <- cleaned_routine %>% 
  group_by(adm2) %>% 
  summarise(total_tested = sum(test_u5, test_ov5, na.rm = TRUE),
            total_conf = sum(conf_u5, conf_ov5, na.rm = TRUE ))

write_csv(data_adm2, "raw_data/aggreg_adm2.csv")

# Aggregate monhtly and save the output file
data_months <- cleaned_routine %>% 
  group_by(month) %>% 
  summarise(total_tested = sum(test_u5, test_ov5, na.rm = TRUE),
            total_conf = sum(conf_u5, conf_ov5, na.rm = TRUE ))

write_csv(data_months, "raw_data/aggreg_monthly.csv")

###merging the files(cleaned routine and the population)
merged_data <- data_adm2 %>% 
  left_join(population, by= "adm2")


####visualization of the data
# Read the adm2 aggregaed data 
#ata_adm2 <- read_csv("raw_data/aggreg_adm2.csv")

# bar plots with change of data format from wide to long
# with selection of color palettes 
data_adm2 %>% 
  pivot_longer(
    cols = total_tested:total_conf,
    names_to =  "group",
    values_to = "counts"
  ) %>% 
  mutate(group2 = factor(group, levels = c('total_tested', 'total_conf'))) %>% 
  ggplot(aes(x=reorder(adm2, -counts), y=counts, fill= group2)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  
  # scale_fill_brewer(palette="Reds") +
  scale_fill_manual(values=c('blue4','green4')) +
  labs(fill = "Indicator", x= "Adm2") +
  #scale_fill_manual(values = c('darkgrey', 'firebrick')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


####exploring  on data frames
head(routine_data)

#extracting part of the data frame using indices
routine_data[,1] #first column only
routine_data[1,] #first row only

#to access variables of the data frame
summary(routine_data$test_u5)
table(routine_data$adm1)


##cleaning the NAs in the data we just drop them when calling or reading the file
routine_data <- read_csv('data/routine_data.csv', na = c("", "NA", -9999))


##Factors in the categorical data, for R to store/analyse the categorical data you h'v to make sure it is a factor and not a character
cleaned_routine$month <- factor(cleaned_routine$month)
class(cleaned_routine$month)

##month should be an ordered variable and factors need to be ordered accordingly or use the month.abb as before
cleaned_routine$month <- factor(cleaned_routine$month,
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
table(cleaned_routine$month)


##DATA WRAGLING AND VIZ IN R
#TASK 1
###install.packages("tidyverse")
library(tidyverse)
routine_data <- read_csv("data/routine_data.csv", na = c('NA', '', -9999))
str(routine_data)
head(routine_data)
summary(routine_data)
routine_data$month <- factor(routine_data$month, levels = month.abb)


##Data cleaning and manipulation in tidyverse
select(routine_data, adm1, adm2, hf, year, month, test_u5, conf_u5)
select(routine_data, 
       -test_rdt_u5, -test_rdt_ov5, -test_mic_u5, -test_mic_ov5,
       -conf_rdt_u5, -conf_rdt_ov5, -conf_mic_u5, -conf_mic_ov5) # if you want to remove some variables

filter(routine_data, 
       adm1 == "West")

filter(routine_data, 
       test_u5 >500)

filter(routine_data, 
       (adm1 == "West" | adm1 == "Central") &
         test_u5 >=500)

filter(routine_data,
       conf_u5>test_u5)

##Null values can be treated by filtering data to remove all missing values for the particular variable
filter(routine_data, !is.na(test_u5))

routine_data <- filter(routine_data,
                    !is.na(test_ov5), 
                    !is.na(conf_ov5),
                    test_ov5 > conf_ov5)
routine_data <- drop_na(routine_data, test_u5) # alternative for removing na's

rename(routine_data, 
       positive_u5 = conf_u5)

mutate(routine_data,
       prop_positive_u5 = conf_u5/test_u5)

table(routine_data$year)
count(routine_data, adm1, month)

###another way to clean the data using case when function
routine_data <- mutate(routine_data,
                    year = case_when(year == 18 ~ 2018,
                                     year == 3018 ~ 2018),
                    date_tested = make_date(year = year, month = month))



###TASK 5

clean_data <- select(cleaned_routine, adm1, adm2, date, conf_u5, conf_ov5)

clean_data <-   pivot_longer(clean_data, 
                              cols = starts_with("conf"),
                              names_to = "age_group", 
                              values_to = "confirmed_cases",
                              names_prefix = "conf_")



###DEMO 2
# Import the population dataset
pop <- read_csv('data/population.csv')

# Confirmed cases and malaria incidence by age group per month for each admin 2 location
pf_incidence <- inner_join(clean_routine_data, pop) %>% 
  select(adm1, adm2, date, starts_with('conf'), starts_with('pop') ) %>% 
  pivot_longer(cols = starts_with("conf")|starts_with("pop"),
               names_to = c("metric", "age_group"),
               names_sep = "_",
               values_to = 'value') %>% 
  pivot_wider(id_cols = c('adm1', 'adm2', 'date_tested', 'age_group'),
              names_from = metric,
              values_from = value) %>% 
  mutate(incidence = conf/pop*100000)

# Confirmed cases and malaria incidence by age group per month for the whole country
pf_incidence_national <- 
  group_by(pf_incidence,
           date_tested, age_group) %>% 
  summarise(across(c(conf, pop), sum)) %>% 
  mutate(incidence = conf/pop*100000)

write_csv(pf_incidence, 'outputs/pf_incidence.csv')
write_csv(pf_incidence_national, 'outputs/pf_incidence_national.csv')



