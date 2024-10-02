#GP_data_wales


#load the needed packages

library(RPostgreSQL)    #To access the database.
library(GetoptLong)     #To substitute variables into strings.
library(tidyverse)      #To tidy, read and write data also for visualizing.
library(dplyr)          #To join two or more data frames based on a common column.
library(data.table)     #To create a data table from a data frame
library(ggplot2)        #To Initialize a ggplot object and specify its data 
                        #and aesthetics
library(tidyr)          #transforming and reshaping data to make it more 
                        #suitable for analysis and visualization



cat("What five drugs does the practice spend the most money on?")
cat("\n\n")
cat("user will input a practiceid")
cat("\n\n")

cat("password = femooo")

user_input <- function(){ 
  y <- ""
  while(y == ""){
    y <- readline(prompt="Enter practice ID -e.g.W12345: ")
    if(tolower(y) == "q"){
      break
    }
    if(grepl('^W[0-9]{5}$', y)==FALSE){
      print ("This practiceid does not exist")
      y <- "" 
    }
  }
  return(y)
}
practiceid <- user_input()

# The drivers required to connect to PostgreSQL are loaded and connected 

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',
                 port=5432, user='postgres',
                 password=.rs.askForPassword('Password(femooo):'))

#this code will list the top five drugs that the chosen practice spends money on
#in 2015

top_five_drugs <- dbGetQuery(con, paste0("
SELECT bnfname 
AS top_five_drugs, 
CAST(SUM(actcost) AS DECIMAL(10, 2)) AS drug_cost
FROM gp_data_up_to_2015
WHERE practiceid = '", practiceid,"' AND CAST(period AS TEXT) 
LIKE '2015%'
GROUP BY bnfname
ORDER BY drug_cost DESC
LIMIT 5;
")) 

cat("\n\n")
cat("FIVE DRUGS '", practiceid,"' SPENDS THE MOST MONEY ON:, \n\n")

print(top_five_drugs)
cat("\n\n")

# A graphical presentation of these drugs distribution will be displayed by 
#the script below.

#Bar chart
#Sort the data by 'drug_cost' in descending order for a better representation
top_five_drugs <- top_five_drugs[order(-top_five_drugs$drug_cost), ]

# Create the plot
top_five_drugs_bar <- ggplot(data = top_five_drugs, aes(x = reorder(top_five_drugs, 
                                                                    drug_cost), 
                                  y = drug_cost)) +
  geom_bar(stat = 'identity', fill = 'aquamarine', col = 'black', width = 0.6) +
  labs(title =paste("1a. - Top Five Drugs by Cost in '", practiceid,"'"),
       x = "Drugs Name",
       y = "Drugs Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

print(top_five_drugs_bar)

#Jitter chart
# Create the jitter plot
top_five_drugs_jitter <- ggplot(data = top_five_drugs, aes(x = reorder
                                                      (top_five_drugs, drug_cost), 
                                  y = drug_cost)) +
  geom_jitter(color = 'blue', shape = 16, width = 0.2, height = 0.2) +
  geom_line(color = 'red', linewidth = 1, group = 1) +
  labs(title =paste("1a. - Top Five Drugs by Cost in '", practiceid,"'"),
       x = "Drugs Name",
       y = "Drugs Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  geom_text(aes(label = drug_cost), vjust = -0.5, size = 4, color = "blue")

print(top_five_drugs_jitter)
cat("\n\n")
cat("- These plots show the top five drugs that '", practiceid,"' 
    spends the most money on, \n\n")



cat("What region is this practice in?")
cat("\n\n")

cat("I am going to use the Health Boards as regions to describe the 
location of '", practiceid,"' and define variables of '", practiceid,"' to 
Health Board mapping")
cat("\n\n")

gp_data <- dbGetQuery(con, paste0("SELECT hb 
                                   FROM gp_data_up_to_2015
                                   WHERE practiceid =  '", practiceid,"'"))

# Nested loop to check 'hb' column and print corresponding Health Board name

cat("-  '", practiceid,"' belongs to the - ")
for (i in 1:nrow(gp_data)) {
  hb <- gp_data$hb[i]
  if (hb == "7A1") {
    print("Betsi Cadwaladr University LHB")
    break  
  } else if (hb == "7A2") {
    print("Hywel Dda University LHB")
    break
  } else if (hb == "7A3") {
    print("Swansea Bay University Local Health Board")
    break
  } else if (hb == "7A4") {
    print("Cardiff & Vale University LHB")
    break
  } else if (hb == "7A5") {
    print("Cwm Taf Morgannwg University Local Health Board")
    break
  } else if (hb == "7A6") {
    print("Aneurin Bevan University LHB")
    break
  } else if (hb == "7A7") {
    print("Powys Teaching Local Health Board")
    break
  }
}

cat("\n\n")

cat("How does '", practiceid,"' rate of smoking compare to Wales as a whole, 
as well as other practices in its region?")

#this code will get all the practice IDs in the same health board as the chosen 
#practiceid

cat("\n\n")
gp_data_hb <- dbGetQuery(con, paste0("
SELECT hb, practiceid 
FROM gp_data_up_to_2015
WHERE hb = '", hb,"' AND CAST(period AS TEXT) 
                                     LIKE '2015%'"))

#this code will show the rate of smoking in the user chosen practice in perc(%)

user_practiceid_smoking <- dbGetQuery(con, paste0("
SELECT AVG(ratio * 100) AS user_practice_smoking_perc
FROM qof_achievement
WHERE indicator = 'SMOK004' AND orgcode = '", practiceid,"' AND year = '15'"))

#this code will show the rate of smoking in Wales in percentage

wales_smoking <- dbGetQuery(con, paste0("
SELECT AVG(ratio * 100) AS wales_smoking_perc
FROM qof_achievement
WHERE indicator = 'SMOK004' AND orgcode = 'WAL' AND year = '15'"))

#this code will create the qof_achievement table which will be joined 
#to gp_data_hp to show the indicators in the health board of the practice

qof_achievement <- dbGetQuery(con, paste0("
SELECT indicator, orgcode AS practiceid, ratio 
FROM qof_achievement WHERE year = '15'"))

#inner join qof_achievement and gp_data_hb
#this will give us data with only the health board that the user chosen practice 
#belongs to

# Convert data frames to data.tables
setDT(gp_data_hb)
setDT(qof_achievement)

# Perform inner join
qof_hb <- gp_data_hb[qof_achievement, on = 'practiceid', 
                     nomatch = 0, allow.cartesian = TRUE]

#this code will show the rate of smoking in all the practices in the 
#same health board as the user chosen practice in percentage

board_practices_smoking <- qof_hb %>%
  filter(indicator == 'SMOK004') %>%
  group_by(indicator) %>%
  summarize(board_practices_smoking = mean(ratio * 100)) %>%
  select(-indicator)

# Convert columns of the table into values in the environment using lists funct
rates_list <- list(
  user_practiceid_smoking = user_practiceid_smoking$user_practice_smoking_perc,
  board_practices_smoking = board_practices_smoking$board_practices_smoking,
  wales_smoking = wales_smoking$wales_smoking_perc
)

#Define a function to get Health Board names

get_hb_name <- function(hb) {
  if (hb == "7A1") {
    return("Betsi Cadwaladr University LHB")
  } else if (hb == "7A2") {
    return("Hywel Dda University LHB")
  } else if (hb == "7A3") {
    return("Swansea Bay University Local Health Board")
  } else if (hb == "7A4") {
    return("Cardiff & Vale University LHB")
  } else if (hb == "7A5") {
    return("Cwm Taf Morgannwg University Local Health Board")
  } else if (hb == "7A6") {
    return("Aneurin Bevan University LHB")
  } else if (hb == "7A7") {
    return("Powys Teaching Local Health Board")
  } 
}

#Apply the function to get Health Board names for the plot
gp_data$hb <- sapply(gp_data$hb, get_hb_name)

#create a rate table for the compared rates from the list

rates_table <- data.frame(
  practices_and_wales = c("user_practiceid_smoking", "board_practices_smoking", 
                          "wales_smoking"),
  rates = c(rates_list$user_practiceid_smoking, rates_list$board_practices_smoking, 
            rates_list$wales_smoking),
  stringsAsFactors = FALSE
)

#plot line and bar charts of the comparism of the rates of smoking 
#in the practice to other practices in its health board and the whole of Wales

rate_of_smoking_bar <- ggplot(data = rates_table) +
  geom_bar(aes(x = practices_and_wales, y = rates),
           stat = 'identity', fill = 'aquamarine', col = 'black', width = 0.4) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5),
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size = 12),  
        panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank(),  
        panel.grid.major.y = element_line(color = "gray80"),  
        panel.grid.minor.y = element_blank(),  
        plot.title = element_text(size = 12, hjust = 0.6)) +  
  labs(title = paste("1C. Smoking Rates in Practice '", practiceid,"' , Wales and", 
                     gp_data$hb),
       y = "Rates")

print(rate_of_smoking_bar)

cat("\n\n")
cat("- The plot shows the comparism among '", practiceid,"' rate of smoking, 
other practices in the same Health Board and the whole of Wales., \n\n")

cat("Smoking is associated with a number of chronic diseases. I will Use statistical 
analysis to show the relationship between the rate of smoking at 
practices and the rates of the following diseases: asthma, cancer, 
coronary heart disease, dementia, hypertension. If I find statistically 
significant relationships, i will show the disease that is most strongly 
associated with smoking?")

qof_achievement_new <- dbGetQuery(con, paste0("
SELECT indicator, orgcode 
AS practiceid, 
ROUND(CAST(ratio AS NUMERIC), 2) AS ratio 
FROM qof_achievement 
WHERE year = '15' 
AND indicator IN ('AST001', 'CAN001', 'CHD001', 'DEM001', 'HYP001')"))


# Reshape data to convert indicators into separate columns

reshaped_data <- qof_achievement_new %>%
  pivot_wider(names_from = indicator, values_from = ratio)

# Calculate correlation matrix for relevant columns (diseases)
correlation_matrix <- cor(reshaped_data[, c("AST001", "CAN001", "CHD001", 
                                            "DEM001", "HYP001")])

# Rename the columns
colnames(correlation_matrix) <- c("ASTHMA", "CANCER", "CORONARY HEART DISEASE", 
                                  "DEMENTIA", "HYPERTENSION")


# Extract correlations of smoking with other diseases
smoking_correlations <- correlation_matrix[1, -1]


# Create a bar plot to visualize correlations
correlation_data <- data.frame(Disease = names(smoking_correlations), 
                               Correlation = smoking_correlations)

correlation_data <- correlation_data %>%
  mutate(Disease = recode(Disease,
                           "AST001" = "ASTHMA",
                           "CAN001" = "CANCER",
                           "CHD001" = "CHD",
                           "DEM001" = "DEMENTIA",
                           "HYP001" = "HYPERTENSION"))

rate_of_smoking_vs_diseases <- ggplot(correlation_data, aes(x = Disease, 
                                                            y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "2a. Correlation between Smoking and Chronic Diseases in Practices",
       x = "Disease",
       y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(rate_of_smoking_vs_diseases)

# Identify the disease most strongly associated with smoking
most_strongly_associated_disease <- names(smoking_correlations)[which.max(abs(smoking_correlations))]
cat("\n\n")
cat("2a. - The disease most strongly associated with smoking, using correlation
    is:", most_strongly_associated_disease, "\n\n")


cat("Do practices with higher rates of smoking spend more or less on medications?")
cat("\n\n")

#I will create a table showing the cost of drugs per practiceid
smok_cost <- dbGetQuery(con, paste0("
SELECT gp_data_up_to_2015.practiceid,
CAST(SUM(gp_data_up_to_2015.actcost) AS DECIMAL(10, 2)) AS drug_cost,
qof_achievement.ratio AS smoking_rates
FROM gp_data_up_to_2015
INNER JOIN qof_achievement ON qof_achievement.orgcode = gp_data_up_to_2015.practiceid
WHERE qof_achievement.indicator = 'SMOK004' AND 
CAST(gp_data_up_to_2015.period AS TEXT) 
LIKE '2015%'
GROUP BY gp_data_up_to_2015.practiceid, qof_achievement.ratio;"))

# Calculate the correlation between Smoking and drug cost
correlation <- cor(smok_cost$smoking_rates, smok_cost$drug_cost)

# Create a scatter plot to visualize the relationship
drug_vs_smoking_plot <- ggplot(smok_cost, aes(x = smoking_rates, y = drug_cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Smoking Rate", y = "Drug Cost", 
       title = "2B. Scatter Plot of Smoking Rate vs Drug Cost")

print(drug_vs_smoking_plot)

cat("- The scatter plot shows the relationship between the smoking rate and the 
drug cost in practices. 
The smoking rate is the percentage of people who 
smoke in the practices, and the drug cost is the average cost of prescription 
drugs the practices prescribe.
The scatter plot shows a POSITIVE correlation between the smoking rate and the 
drug cost.
This means that as the smoking rate increases, the drug cost also
increases in the practices. 
One possibility is that smoking can lead to health problems, which can require 
expensive prescription drugs. For example, smoking can increase the risk of 
cancer, heart disease, and neurological diseases, all of which require expensive 
treatments.
In conclusion, practices with a higher rate of smoking spend more on medications.
Another possibility is that smokers are more likely to have chronic health 
conditions, which require expensive prescription drugs. For example, smokers 
are more likely to have chronic obstructive pulmonary disease (COPD), which 
is a chronic lung disease that requires expensive medication., \n\n")

#End of Part 1

#######################

cat("\n\n")
cat("a.I will show the relationship between metformin prescription and the rate
of obesity and hypertension in practices in wales, and also show Which 
relationship is stronger with plots.") 

cat("\n\n")
cat("\n\n")

hyp_ob <- dbGetQuery(con, paste0("
SELECT gp_data_up_to_2015.practiceid, 
SUM(1) AS metformin_prescription, 
qof_achievement.indicator, 
ROUND(CAST(qof_achievement.ratio AS NUMERIC), 2) AS disease_rate
FROM gp_data_up_to_2015
INNER JOIN qof_achievement ON gp_data_up_to_2015.practiceid = qof_achievement.orgcode
WHERE gp_data_up_to_2015.bnfname LIKE 'Metformin%' 
AND qof_achievement.indicator IN ('HYP001', 'OB001W')
GROUP BY gp_data_up_to_2015.practiceid, qof_achievement.indicator, 
                                 qof_achievement.ratio;"))


hypertension <- hyp_ob %>%
  filter(indicator == 'HYP001') %>%
  select(-indicator) %>%
  rename(hypertension_rate = disease_rate)

obesity <- hyp_ob %>%
  filter(indicator == 'OB001W') %>%
  select(-indicator) %>%
  rename(obesity_rate = disease_rate)

hyp_ob_met <- inner_join(hypertension, obesity, by = "practiceid") %>% 
  select(-metformin_prescription.x) %>%
  rename(metformin_prescription = metformin_prescription.y)


#The code below will create the scatter plot with hypertension rate and metformin 
#prescription
hyp_scatter_plot <- ggplot(hyp_ob_met, aes(x = metformin_prescription , 
                                           y = hypertension_rate )) +
  geom_point(shape = 16, color = "blue") +
  labs(title = "Part2a. Scatter Plot - Hypertension Rate by Metformin Rate in Practices 
                                                     in Wales",
       x = "Metformin Rate",
       y = "Hypertension Rate") +
  theme_minimal()

print(hyp_scatter_plot)

#this code will create the line plot of Obesity and Metformin prescription 
#relationship
obesity_line_plot <- ggplot(hyp_ob_met, aes(x = metformin_prescription, 
                                            y = obesity_rate, group = 1)) +
  geom_line(color = "blue") +
  geom_point(shape = 16, color = "blue") +
  labs(title = "Part2A. Line Plot: Obesity Rate by Metformin Prescription in Wales Practices",
       x = "Metformin Prescription Rate",
       y = "Obesity Rate") +
  theme_minimal()

# Display the line plot
print(obesity_line_plot)

cat("\n\n")
cat("- The Scatter plot shows the relationship between the hypertension rate 
and the metformin prescription rate. The hypertension rate is the percentage of 
people who have hypertension, and the metformin prescription rate is the percentage 
of people who are prescribed metformin.
The plot shows that there is a positive correlation between the hypertension rate 
and the metformin prescription rate. This means that as the hypertension 
rate increases, the metformin prescription rate also increases. This is because 
metformin is a medication that is used to treat diabetes which is a risk factor 
for hypertension.
Overall, the plot shows that there is a strong positive relationship between the 
hypertension rate and the metformin prescription rate., \n\n")


cat("\n\n")

cat("b.I will show the prevalence of illnesses at user chosen practice showing 
percentages and comparing it to the whole of Wales.") 

cat("\n\n")
cat("User will input a practiceID")
cat("\n\n")

user_input2 <- function(){ 
  y <- ""
  while(y == ""){
    y <- readline(prompt="Enter practice ID -e.g.W12345: ")
    if(tolower(y) == "q"){
      break
    }
    if(grepl('^W[0-9]{5}$', y)==FALSE){
      print ("This practiceid does not exist")
      y <- "" 
    }
  }
  return(y)
}
practiceid2 <- user_input2()

prac_dis <- dbGetQuery(con, paste0("
SELECT indicator AS Diseases, (ROUND(CAST(ratio AS NUMERIC), 3) *100) AS prac_rates
FROM qof_achievement 
WHERE indicator IN ('SMOK004', 'AF001', 'AST001', 'CAN001', 'CHD001', 'COPD001', 
                    'DEM001', 'DM001', 'EP001', 'HF001', 'HYP001', 'LD001', 
                    'MH001', 'OST001', 'PAD001', 'RA001', 'STIA001', 'OB001W', 
                    'DEPPREV3') AND year = 15 AND orgcode =  '", practiceid2,"'"))

#Rename the variables

prac_dis <- prac_dis %>%
  mutate(diseases = recode(diseases,
                            "SMOK004" = "Smoking",
                            "AF001" = "Atrial Fibrillation",
                            "AST001" = "Asthma",
                            "CAN001" = "Cancer",
                            "CHD001" = "Coronary Heart Disease",
                            "COPD001" = "Chronic Obstructive Pulmonary Disease",
                            "DEM001" = "Dementia",
                            "DM001" = "Diabetes",
                            "EP001" = "Epilepsy",
                            "HF001" = "Heart Failure",
                            "HYP001" = "Hypertension",
                            "LD001" = "Learning Disabilities",
                            "MH001" = "Mental Health",
                            "OST001" = "Osteoporosis",
                            "PAD001" = "Peripheral Arterial Disease",
                            "RA001" = "Rheumatoid Arthritis",
                            "STIA001" = "Stroke/TIA",
                            "OB001W" = "Obesity",
                            "DEPPREV3" = "Depression (Prevalence)"))

wales_dis <- dbGetQuery(con, paste0("                 
SELECT indicator AS Diseases, (ROUND(CAST(ratio AS NUMERIC), 3) *100) AS wales_rates
FROM qof_achievement 
WHERE indicator IN ('SMOK004', 'AF001', 'AST001', 'CAN001', 'CHD001', 'COPD001', 
                    'DEM001', 'DM001', 'EP001', 'HF001', 'HYP001', 'LD001', 
                    'MH001', 'OST001', 'PAD001', 'RA001', 'STIA001', 'OB001W', 
                    'DEPPREV3') AND year = 15 AND orgcode = 'WAL'"))

#Rename the variables
wales_dis <- wales_dis %>%
  mutate(diseases = recode(diseases,
                            "SMOK004" = "Smoking",
                            "AF001" = "Atrial Fibrillation",
                            "AST001" = "Asthma",
                            "CAN001" = "Cancer",
                            "CHD001" = "Coronary Heart Disease",
                            "COPD001" = "Chronic Obstructive Pulmonary Disease",
                            "DEM001" = "Dementia",
                            "DM001" = "Diabetes",
                            "EP001" = "Epilepsy",
                            "HF001" = "Heart Failure",
                            "HYP001" = "Hypertension",
                            "LD001" = "Learning Disabilities",
                            "MH001" = "Mental Health",
                            "OST001" = "Osteoporosis",
                            "PAD001" = "Peripheral Arterial Disease",
                            "RA001" = "Rheumatoid Arthritis",
                            "STIA001" = "Stroke/TIA",
                            "OB001W" = "Obesity",
                            "DEPPREV3" = "Depression (Prevalence)"))

#Merge the two tables using inner join
prac_wales <- merge(prac_dis, wales_dis, by = "diseases", all = FALSE)

reshaped_table <- gather(prac_wales, key = "source", value = "rates", -diseases)

#Plot the bar chart of user practice vs Wales
prac_wales_plot <- ggplot(reshaped_table, aes(x = diseases, y = rates, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Part2B. - Diseases prevalence in '", practiceid2,"' and Wales"), 
       x = "Diseases",
       y = "Prevalence(%)", 
       fill = "") +
  scale_fill_manual(values = c("prac_rates" = "blue", "wales_rates" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(prac_wales_plot)

cat("\n\n")
cat("- This plot shows the prevalence of the diseases in '", practiceid2,"'
    and compared to the prevalence in the whole of Wales for the year 2015, \n\n")
cat("\n\n")

cat("\n\n")
cat("c.I will show the prevalence of illnesses in '", practiceid2,"'showing 
percentages and comparing it to the average of the different prevalence in 
other practices in its health board.") 

#Define variables for practiceid to Health Board mapping
gp_data2 <- dbGetQuery(con, paste0("SELECT hb 
                                    FROM gp_data_up_to_2015
                                    WHERE practiceid =  '", practiceid2,"'"))

for (i in 1:nrow(gp_data2)) {
  hb2 <- gp_data2$hb[i]}

#This code will show the average of the prevalence of diseases in the same 
#health board as user chosen practice
hb_avg_rates <- dbGetQuery(con, paste0("
SELECT Diseases, hb,
ROUND(AVG(Rates_practice), 2) AS Average_Rates_Practice
FROM (
  SELECT indicator AS Diseases,
  hb,
  (ROUND(CAST(ratio AS NUMERIC), 3) * 100) AS Rates_practice
  FROM qof_achievement 
  INNER JOIN gp_data_up_to_2015
  ON qof_achievement.orgcode = gp_data_up_to_2015.practiceid
  WHERE indicator IN ('SMOK004', 'AF001', 'AST001', 'CAN001', 'CHD001', 'COPD001', 
                      'DEM001', 'DM001', 'EP001', 'HF001', 'HYP001', 'LD001', 
                      'MH001', 'OST001', 'PAD001', 'RA001', 'STIA001', 'OB001W', 
                      'DEPPREV3') AND year = 15 AND hb = '", hb2,"'
) AS subquery
GROUP BY Diseases, hb"))

hb_avg_rates <- hb_avg_rates %>%
  mutate(diseases = recode(diseases,
                           "SMOK004" = "Smoking",
                           "AF001" = "Atrial Fibrillation",
                           "AST001" = "Asthma",
                           "CAN001" = "Cancer",
                           "CHD001" = "Coronary Heart Disease",
                           "COPD001" = "Chronic Obstructive Pulmonary Disease",
                           "DEM001" = "Dementia",
                           "DM001" = "Diabetes",
                           "EP001" = "Epilepsy",
                           "HF001" = "Heart Failure",
                           "HYP001" = "Hypertension",
                           "LD001" = "Learning Disabilities",
                           "MH001" = "Mental Health",
                           "OST001" = "Osteoporosis",
                           "PAD001" = "Peripheral Arterial Disease",
                           "RA001" = "Rheumatoid Arthritis",
                           "STIA001" = "Stroke/TIA",
                           "OB001W" = "Obesity",
                           "DEPPREV3" = "Depression (Prevalence)"))

#merge rates tables for 
prac_hb <- merge(prac_dis, hb_avg_rates, by = "diseases", all = FALSE)

#Plot the bar chart of user practice vs other practices in its Health Board
#Define a function to get Health Board names

get_hb_name_2 <- function(hb2) {
  if (hb2 == "7A1") {
    return("Betsi Cadwaladr University LHB")
  } else if (hb2 == "7A2") {
    return("Hywel Dda University LHB")
  } else if (hb2 == "7A3") {
    return("Swansea Bay University Local Health Board")
  } else if (hb2 == "7A4") {
    return("Cardiff & Vale University LHB")
  } else if (hb2 == "7A5") {
    return("Cwm Taf Morgannwg University Local Health Board")
  } else if (hb2 == "7A6") {
    return("Aneurin Bevan University LHB")
  } else if (hb2 == "7A7") {
    return("Powys Teaching Local Health Board")
  } 
}

#Apply the function to get Health Board names for the plot
prac_hb$hb_name <- sapply(prac_hb$hb, get_hb_name_2)

reshaped_table_2 <- gather(prac_hb, key = "source", value = "rates", -diseases, 
                           -hb, -hb_name)

# Plot the bar chart of user practice vs Wales
prac_hb_plot <- ggplot(reshaped_table_2, aes(x = diseases, y = rates, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  labs(title = paste("Part2C. Rates of Diseases in '", practiceid2, "' and", prac_hb$hb_name),
       x = "Diseases",
       y = "Prevalence(%)",
       fill = "") +
  scale_fill_manual(values = c("prac_rates" = "blue", "average_rates_practice" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(prac_hb_plot)

cat("\n\n")
cat("- This plot shows the prevalence of the diseases in '", practiceid2,"'
and compared to the average of all the practices in the same Health 
Board for the year 2015, \n\n")
cat("END")
