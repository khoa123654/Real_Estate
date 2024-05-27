library(readr)
library(tidyverse)
library(dplyr)

df <- read_csv("2021 Western Carolina University Student Needs Survey_August 10, 2023_17.45.csv")


#Edit Rows
df <- df %>%
  slice(3 : n()) %>%
  setNames(as.character(df[1,]))


names(df) <- make.unique(names(df))


#Remove Identifiers
df <- df %>%
  dplyr::select(-c(1:11))

colnames(df)[1] <- "Payment_Ways"

#Create new features based on Payment Ways
df$Work_Study <- ifelse(grepl("work-study", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Student_Loans <- ifelse(grepl("student loans", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Savings <- ifelse(grepl("savings", df$Payment_Ways, ignore.case = TRUE), 0, 1)
df$Pell_Grant <- ifelse(grepl("Pell Grant", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Federal_Grants <- ifelse(grepl("grants from the federal or state government", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Family_Support <- ifelse(grepl("I get help from family or friends", df$Payment_Ways, ignore.case = TRUE), 0, 1)
df$Credit_Cards <- ifelse(grepl("credit card", df$Payment_Ways, ignore.case = TRUE), 1, 0)

# Extracting potentially relevant features into new_df
new_df <- data.frame(row.names = seq_len(nrow(df)))

new_df$Searching_Work <- df$`In the past 30 days have you been looking for work?`
new_df$Have_Job <- df$`LAST WEEK, did you have a job where you worked for pay or profit? Include a job even if you were temporarily absent from it last week.`
new_df$Number_Jobs <- df$`LAST WEEK, not including Federal Work-Study, how many jobs did you have?`
new_df$Hours_Work <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Working for pay -`
new_df$Commuting_Hours <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Commuting to or from work or school -`
new_df$Sleeping <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Sleeping -`
new_df$Leisure_Activites <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Leisure activities (for example, spending time with friends, watching TV or... - Leisure activities (for example, spending time with friends, watching TV or movies, using the internet for leisure, talking or texting on the phone) -`

new_df$Scale_Food_Insecurity <- df$`On a scale from 1-10, with 1 being strongly disagree and 10 being strongly agree, please rate your agreement with the following statement:
In the last 12 months, there were times when I was hungry but didn't eat because there wasn't enough money for food`

new_df$ClassWork_Hours <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Attending college classes, labs, or discussion sections either in person or... - Attending college classes, labs, or discussion sections either in person or online -`
new_df$Studying_Hours <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Preparing for class by yourself or with others by studying, reading, writin... - Preparing for class by yourself or with others by studying, reading, writing, rehearsing, or doing other academic activities -`
new_df$Qualify_Reduced_Lunch <- df$`In the past 12 months, has anyone in your family under age 18 received free or reduced price breakfast or lunch at school?`
new_df$Worry_Food_Running_Out <- df$`Thinking about the last 12 months, how true would you say the following statements are? - I worried whether my food would run out before I got money to buy more`
new_df$Afford_Balanced_Meals <- df$`Thinking about the last 12 months, how true would you say the following statements are? - I couldn’t afford to eat balanced meals`
new_df$Cut_Size_Or_Skip_Meals <- df$`In the last 12 months, did you ever cut the size of your meals or skip meals because there wasn’t enough money for food?`
new_df$How_Often_Skip_Meals <- df$`How often did this happen?`
new_df$Food_Pantry_On_Campus <- df$`Is there a food pantry on campus?`
new_df$On_Campus_Residence_Halls <- df$`Does your college have on-campus residence halls?`
new_df$No_Place_Winter_Spring_Breaks <- df$`In the last 12 months, have you ever not known where you would stay during winter/spring breaks because the on-campus residence halls were closed?`
new_df$Stayed_Others_Room <- df$`In the past 12 months, were there times when you stayed in someone else’s room in an on-campus residence hall because you didn’t have anywhere else to sleep?`
new_df$Left_Staying_Admin_Rules <- df$`In the past 12 months, were there times you stayed in someone else’s room in an on-campus residence hall but had to leave because of administration rules?`
new_df$Share_Residence <- df$`Do you share your residence with other people?`
new_df$Rent_Mortgage_Hold <- df$`In the past 12 months, was there a rent or
mortgage increase that made it difficult to pay?`
new_df$Home_Public_Housing <- df$`Is your home in a public housing project, owned by a local housing authority or other public agency?`
new_df$Public_Housing_Voucher <- df$`Do you receive a public housing voucher, such as Section 8, to subsidize the cost of private housing?`
new_df$Monthly_Income_Rent_Mortgage <- df$`Approximately how much of your total household monthly income do you spend on rent or mortgage?`
new_df$Times_Moved <- df$`In the past 12 months, how many times have you moved?`
new_df$Parent1_Education <- df$`What is the highest level of education completed by Parent 1?`
new_df$Safety_Feeling <- df$`How safe do you feel where you currently live?`
new_df$Left_Household_Unsafe <- df$`In the past 12 months, did you leave your household because you felt unsafe?`
new_df$Thrown_Out_By_Household <- df$`In the past 12 months, were you thrown out of your home by someone else in the household?`
new_df$Parent2_Education <- df$`What is the highest level of education completed by Parent 2?`
new_df$Race_Ethnicity <- df$`How do you usually describe your race and/or ethnicity? (Select all that apply) - Selected Choice`
new_df$US_Citizen_Resident <- df$`Are you a U.S. citizen or permanent resident?`
new_df$Have_Children <- df$`Do you have any biological, adopted, step or foster children?`
new_df$Been_In_Foster_Care <- df$`Have you ever been in foster care?`
new_df$Age <- df$Age
new_df$Gender <- df$Gender
new_df$GPA <- df$GPA
new_df$Student_Status <- df$student_full_part_time
new_df$Work_Study <- df$Work_Study
new_df$Student_Loans <- df$Student_Loans
new_df$Savings <- df$Savings
new_df$Pell_Grant <- df$Pell_Grant
new_df$Federal_Grants <- df$Federal_Grants
new_df$Family_Support <- df$Family_Support
new_df$Credit_Cards <- df$Credit_Cards
new_df$Sexual_Orientation <- df$`Do you consider yourself to be:`



colnames(new_df)

#Turn Characters into Valid Format for Regression Analysis

# Convert "Yes"/"No" responses to 1/0 for Searching_Work, leaving NA as is
new_df$Searching_Work <- ifelse(is.na(new_df$Searching_Work), NA, ifelse(grepl("Yes", new_df$Searching_Work, ignore.case = TRUE), 1, 0))

# Convert "Yes"/"No" responses to 1/0 for Have_Job, leaving NA as is
new_df$Have_Job <- ifelse(is.na(new_df$Have_Job), NA, ifelse(grepl("Yes", new_df$Have_Job, ignore.case = TRUE), 1, 0))

# Remove non-numeric characters from Scale_Food_Insecurity, leaving NA as is
new_df$Scale_Food_Insecurity <- gsub("[^0-9]", "", new_df$Scale_Food_Insecurity)


# Convert "Yes"/"No" responses to 1/0 for Qualify_Reduced_Lunch, leaving NA as is
new_df$Qualify_Reduced_Lunch <- ifelse(is.na(new_df$Qualify_Reduced_Lunch), NA, ifelse(grepl("Yes", new_df$Qualify_Reduced_Lunch, ignore.case = TRUE), 1, 0))

# Convert "Never"/other responses to 0/1 for Worry_Food_Running_Out, leaving NA as is
new_df$Worry_Food_Running_Out <- ifelse(is.na(new_df$Worry_Food_Running_Out), NA, ifelse(grepl("Never", new_df$Worry_Food_Running_Out, ignore.case = TRUE), 0, 1))

# Convert "Never"/other responses to 1/0 for Afford_Balanced_Meals, leaving NA as is
new_df$Afford_Balanced_Meals <- ifelse(is.na(new_df$Afford_Balanced_Meals), NA, ifelse(grepl("Never", new_df$Afford_Balanced_Meals, ignore.case = TRUE), 0, 1))


# Convert "Yes"/"No" responses to 1/0 for Share_Residence, leaving NA as is
new_df$Share_Residence <- ifelse(is.na(new_df$Share_Residence), NA, ifelse(grepl("Yes", new_df$Share_Residence, ignore.case = TRUE), 1, 0))

# Convert "Yes"/"No" responses to 1/0 for Home_Public_Housing, leaving NA as is
new_df$Home_Public_Housing <- ifelse(is.na(new_df$Home_Public_Housing), NA, ifelse(grepl("Yes", new_df$Home_Public_Housing, ignore.case = TRUE), 1, 0))

# Convert "More"/other responses to 1/0 for Monthly_Income_Rent_Mortgage, leaving NA as is
new_df$Monthly_Income_Rent_Mortgage <- ifelse(is.na(new_df$Monthly_Income_Rent_Mortgage), NA, ifelse(grepl("More", new_df$Monthly_Income_Rent_Mortgage, ignore.case = TRUE), 1, 0))


new_df$Rent_Mortgage_Hold <- ifelse(is.na(new_df$Rent_Mortgage_Hold), NA, ifelse(grepl("Yes", new_df$Rent_Mortgage_Hold, ignore.case = TRUE), 1, 0))

# Modify Parent1_Education and convert to 1/0, leaving NA as is
new_df$Parent1_Education <- ifelse(is.na(new_df$Parent1_Education), NA, ifelse(grepl("Some college (but no college degree)", new_df$Parent1_Education, ignore.case = TRUE), 0, new_df$Parent1_Education))
new_df$Parent1_Education <- ifelse(is.na(new_df$Parent1_Education), NA, ifelse(grepl("degree", new_df$Parent1_Education, ignore.case = TRUE), 0, 1))

new_df$Parent2_Education <- ifelse(is.na(new_df$Parent2_Education), NA, ifelse(grepl("Some college (but no college degree)", new_df$Parent2_Education, ignore.case = TRUE), 0, new_df$Parent2_Education))
new_df$Parent2_Education <- ifelse(is.na(new_df$Parent2_Education), NA, ifelse(grepl("degree", new_df$Parent2_Education, ignore.case = TRUE), 0, 1))

new_df$Parents_Education <- ifelse(new_df$Parent1_Education == 1 | new_df$Parent2_Education == 1, 1, 0)

# Modify Safety_Feeling and convert to 1/0, leaving NA as is
new_df$Safety_Feeling <- ifelse(is.na(new_df$Safety_Feeling), NA, ifelse(grepl("extremely", new_df$Safety_Feeling, ignore.case = TRUE), 0, new_df$Safety_Feeling))
new_df$Safety_Feeling <- ifelse(is.na(new_df$Safety_Feeling), NA, ifelse(grepl("very", new_df$Safety_Feeling, ignore.case = TRUE), 0, new_df$Safety_Feeling))
new_df$Safety_Feeling <- ifelse(is.na(new_df$Safety_Feeling), NA, ifelse(grepl("0", new_df$Safety_Feeling, ignore.case = TRUE), 0, 1))



new_df$Left_Household_Unsafe <- ifelse(is.na(new_df$Left_Household_Unsafe), NA, ifelse(grepl("yes", new_df$Left_Household_Unsafe, ignore.case = TRUE), 1, 0))
new_df$Thrown_Out_By_Household <- ifelse(is.na(new_df$Thrown_Out_By_Household), NA, ifelse(grepl("yes", new_df$Thrown_Out_By_Household, ignore.case = TRUE), 1, 0))


new_df$Have_Children <- ifelse(is.na(new_df$Have_Children), NA, ifelse(grepl("yes", new_df$Have_Children, ignore.case = TRUE), 1,0))


new_df$Gender <- ifelse(is.na(new_df$Gender), NA, ifelse(grepl("F", new_df$Gender, ignore.case = TRUE), 0,1))

new_df$Student_Status <- ifelse(is.na(new_df$Student_Status), NA, ifelse(grepl("F", new_df$Student_Status, ignore.case = TRUE), 0,1))

new_df$Sexual_Orientation <- ifelse(is.na(new_df$Sexual_Orientation), NA, ifelse(grepl("straight", new_df$Sexual_Orientation, ignore.case = TRUE), 0,1))



new_df <- new_df %>%
  select(-c(Parent1_Education, Parent2_Education ,How_Often_Skip_Meals, Food_Pantry_On_Campus, Stayed_Others_Room, Public_Housing_Voucher, US_Citizen_Resident, Been_In_Foster_Care))

for (col in names(new_df)) {
  new_df[[col]] <- as.numeric(new_df[[col]])
}

