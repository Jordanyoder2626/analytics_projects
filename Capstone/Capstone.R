#ANALYZE TIME SERIES DATA
library(ggplot2)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)


data = read.csv("small_data.csv")
#PA <- read.csv("state_PA.csv")

#COLUMN SELECTION
# PA %>% select(c(derived_msa.md, conforming_loan_limit, derived_loan_product_type, derived_dwelling_category, derived_ethnicity,
#                      derived_race, derived_sex, action_taken, purchaser_type, loan_type, loan_purpose, loan_amount, loan_to_value_ratio,
#                      interest_rate, total_loan_costs, total_points_and_fees, origination_charges, lender_credits, loan_term,
#                      property_value, occupancy_type, income, debt_to_income_ratio, applicant_credit_score_type, co.applicant_credit_score_type,
#                      applicant_age, co.applicant_age, aus.1, aus.2, aus.3, aus.4, aus.5, denial_reason.1, denial_reason.2, denial_reason.3,
#                      denial_reason.4, tract_population, ffiec_msa_md_median_family_income, tract_to_msa_income_percentage))
# summary(PA)

#changing datatypes
col_factor <- c("conforming_loan_limit", "derived_loan_product_type", "loan_type", "loan_purpose", "occupancy_type", 
               "applicant_credit_score_type", "co.applicant_credit_score_type", "aus.1","aus.2","aus.3","aus.4","aus.5",
               "denial_reason.1","denial_reason.2","denial_reason.3","denial_reason.4", "action_taken", "purchaser_type", "derived_dwelling_category",
               "derived_ethnicity", "derived_race", "derived_sex", "occupancy_type", "lien_status", "hoepa_status", "intro_rate_period",
               "negative_amortization", "interest_only_payment", "balloon_payment", "other_nonamortizing_features", "construction_method",
               "multifamily_affordable_units", "applicant_ethnicity.1", "applicant_ethnicity.2", "applicant_ethnicity.3", "applicant_ethnicity.4", 
               "applicant_ethnicity.5", "co.applicant_ethnicity.1", "co.applicant_ethnicity.2","co.applicant_ethnicity.3", "co.applicant_ethnicity.4", 
               "co.applicant_ethnicity.5", "applicant_ethnicity_observed",
               "co.applicant_ethnicity_observed", "applicant_race.1", "applicant_race.2", "applicant_race.3", "applicant_race.4", "applicant_race.5",
               "co.applicant_race.1", "co.applicant_race.2", "co.applicant_race.3", "co.applicant_race.4", "co.applicant_race.5", "applicant_race_observed",
               "co.applicant_race_observed","applicant_sex",   "co.applicant_sex", "applicant_sex_observed", "co.applicant_sex_observed",
               "submission_of_application", "initially_payable_to_institution",
               "preapproval", "reverse_mortgage", "open.end_line_of_credit", "business_or_commercial_purpose", "prepayment_penalty_term",
               "manufactured_home_secured_property_type", "manufactured_home_land_property_interest", "county_code", "census_tract")


data = data %>% select(-c("X", "activity_year","lei", "applicant_age_above_62", "co.applicant_age_above_62"))
# PA <- PA %>%
#   mutate(across(all_of(col_factor), as.factor))
data<- data %>%
  mutate(across(all_of(col_factor), as.factor))

col_numeric <- c("loan_to_value_ratio", "interest_rate", "total_loan_costs", "debt_to_income_ratio", "applicant_age",
                 "total_points_and_fees", "lender_credits", "origination_charges", "loan_term", "property_value",
                 "co.applicant_age", "rate_spread", "discount_points", "income", "intro_rate_period")
data <- data %>%
  mutate(across(all_of(col_numeric), as.numeric))

data_numeric = data %>% select_if(is.numeric)

data_numeric = data_numeric %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))))




cor_data = cor(data_numeric)
#dev.off()
corrplot(cor_data, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

  




# #NA data cleaning
# sum(is.na(PA))
# colSums(is.na(PA))



#plot
colSums(is.na(data))

data = data %>% filter(income < 50000 |is.na(income), loan_to_value_ratio < 500000000|is.na(loan_to_value_ratio)
                       , total_loan_costs<500000|is.na(total_loan_costs),
                       income < 30000|is.na(income), rate_spread>-200|is.na(rate_spread), property_value<800000000
                       |is.na(property_value), interest_rate< 50|is.na(interest_rate))

p<-ggplot(data=data, aes(x=action_taken, y=income)) +
  geom_boxplot()
p

ggplot(data=data, aes(x=action_taken, y=debt_to_income_ratio)) +
  geom_boxplot()

data$denial_reason.1 = ifelse(data$denial_reason.1 == 1,"DTI", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 2,"Employment History", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 3,"Credit History", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 4,"Collaeral", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 5,"Insufficient Cash", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 6,"Unverifiable Info", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 7,"Credit Application Incomplete", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 8,"Mortgatge Insurance Denied", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 9,"Other", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 10,"NA", data$denial_reason.1)
data$denial_reason.1 = ifelse(data$denial_reason.1 == 11,"Exempt", data$denial_reason.1)

plot1 = data %>% group_by(data$denial_reason.1) %>% summarise(mean(debt_to_income_ratio,na.rm=T))



options(scipen=999)
ggplot(data=plot1, aes(x=plot1$`data$denial_reason.1`, y=plot1$`mean(debt_to_income_ratio, na.rm = T)`)) +
  geom_boxplot() + coord_flip() + ylab("Mean DTI") + xlab("Denial Reason") + ggtitle("Mean DTI by Denial Reason")+
  theme_grey(base_size = 22)

ggplot(data=data, aes(x=denial_reason.1, y=debt_to_income_ratio, col = denial_reason.1)) +
  geom_boxplot(show.legend = F
               ) + coord_flip() + ylab("Mean DTI") + xlab("Denial Reason") + ggtitle("Mean DTI by Denial Reason")+
  theme_grey(base_size = 22)



ggplot(data = data, aes(x= action_taken, y = loan_to_value_ratio, fill = action_taken))+
  geom_bar(position = "dodge",
                    stat = "summary",
                    fun = "mean") +
  xlab("Action Taken") +
  ylab("Mean Loan to Value Ratio")+
  ggtitle("Loan to Value Mean versus Action Taken")+
  theme_grey(base_size = 30)

data2 = sample_n(data, 10000)
data2 = data2 %>% filter(action_taken !=1)
ggplot(data = data, aes(x=property_value, y = interest_rate, col = action_taken))+
  geom_point()+
  xlim(-5000, 1250000)+
  ylim(0,20)+
  geom_smooth(method=lm)+
  xlab("Property Value")+
  ylab("Interest Rate")+
  ggtitle("Interest Rates by Property Value")+
  theme_grey(base_size = 20)

ggplot(data = data, aes(x=property_value, y = income, col = action_taken))+
  geom_point()+
  xlim(-5000, 1250000)+
  ylim(0,2000)+
  geom_smooth(method=lm)+
  xlab("Property_Value")+
  ylab("Income")+
  ggtitle("Interest Rates by Property Value")+
  theme_grey(base_size = 20)


boxplot(data$action_taken, data$interest_rate) 

data2 = na.omit(data)


data %>% group_by(action_taken) %>% summarise(mean(interest_rate, na.rm = T), #interest rate out
                                              mean(rate_spread, na.rm =T), #rate spread out
                                              mean(origination_charges, na.rm=T), #origination charges out
                                              mean(income, na.rm=T), #in
                                              mean(total_loan_costs, na.rm=T), #out
                                              mean(loan_to_value_ratio, na.rm=T), #out
                                              mean(tract_minority_population_percent, na.rm=T),#in
                                              mean(tract_to_msa_income_percentage, na.rm=T), #in
                                              mean(intro_rate_period, na.rm=T), #in
                                              mean(loan_amount, na.rm=T), #in
                                              mean(property_value, na.rm = T), #can likely keep
                                              mean(tract_population, na.rm = T), #in
                                              mean(tract_one_to_four_family_homes, na.rm = T),#in
                                              mean(lender_credits, na.rm = T), #out
                                              mean(tract_owner_occupied_units, na.rm =T)
                                              
                                              ) 
table(data$denial_reason.1, data$action_taken) #denial reason out

table(data$hoepa_status, data$action_taken) #hoepa status out

table(data$submission_of_application, data$action_taken) #submission of application out

table(data$co.applicant_credit_score_type, data$action_taken) #co applicant score type can stay if we do binary

table(data$applicant_credit_score_type, data$action_taken) #same as above

table(data$aus.1, data$action_taken) #out

table(data$occupancy_type, data$action_taken) #keep

table(data$loan_purpose, data$action_taken) #can keep if 0 and 1

table(data$purchaser_type, data$action_taken)

table(data$initially_payable_to_institution, data$action_taken)

table(data$applicant_sex, data$action_taken)

table(data$lender_credits, data$action_taken)


# MODEL **************************************

data_2 = data %>% select("income", "tract_minority_population_percent",
                        "tract_to_msa_income_percentage", "intro_rate_period",
                        "loan_amount", "property_value", "tract_population",
                        "tract_one_to_four_family_homes", "occupancy_type",
                        "applicant_sex", "tract_owner_occupied_units", "action_taken")


library(caret)
library(randomForest)
library(dplyr)
library(caTools)

set.seed(123)  # Set a seed for reproducibility
data_2$action_taken = as.factor(data_2$action_taken)
data_2$action_taken = ifelse(is.na(data_2$action_taken), 1111, data_2$action_taken)
index = sample(nrow(data_2),0.7*nrow(data_2)) 

data_train = data_2[index, ]
data_test = data_2[-index, ]



# model <- randomForest(action_taken~.,data= data_train)
# 
# dev.off()
# importance(model)
# 
# varImpPlot(model)
# 
# 
# pred_test <- predict(model, newdata = data_test, type="class")
# pred_test
# 
# dim(pred_test)
# 
# confusionMatrix(table(pred_test,data_test$action_taken)) # The prediction to compute the confusion matrix and see the accuracy score
# 
# table(pred_test, data_test$action_taken)
# 
# 
# write.csv(data_2,"model_data.csv", row.names = FALSE) 

# ggplot(data = data_race,aes(x = action_taken, y = action_taken, fill = derived_race))+
#   geom_bar(position = "dodge", stat = "identity")
data$dti_range = cut(data$debt_to_income_ratio, breaks = 3, labels = c("Low","Medium","High"))


nums = c(1,2,6)
data$approval = ifelse(data$action_taken %in% nums, 1, 0)

data$derived_race = ifelse(data$derived_race == "2 or more minority races", "Multiple Minority Races", data$derived_race)
data$derived_race = ifelse(data$derived_race == 4, "Black", data$derived_race)
data$derived_race = ifelse(data$derived_race == 9, "White", data$derived_race)
data$derived_race = ifelse(data$derived_race == 3, "Asian", data$derived_race)

races = c("Black", "White", "Asian", "Multiple Minority Races")
data_race = data %>% filter(data$derived_race %in% races)
acount = data %>% filter(data$approval == 1)
rate = str(nrow(acount)/nrow(data))
lab = paste("Approval rate for entire dataset is ", round(nrow(acount)/nrow(data),2))


### APPROVAL RATE BY RACE BARPLOT
tbl = with(data_race, table(approval, derived_race))
tbl
prop_tbl = proportions(tbl, margin = 2)
par(mar = c(5,5,5,5))
b = barplot(prop_tbl, beside = TRUE, ylim = c(0,1), xlab = "Race", 
        ylab = "Proportion", main = "Approval Rates By Race" ,col = c("lightblue", "navy"))
legend("topright", legend = c("Denied", "Approved"), fill = c("lightblue", "navy"), title = "Decision")
text(3.5,.85, label = lab, font=2)
text(x = b, y = prop_tbl + .025, labels = round(prop_tbl,2))


### APPROVAL RATE BY SEX BARPLOT
tbl2 = with(data, table(approval, derived_sex))
prop_tbl2 = proportions(tbl2, margin = 2)
par(mar = c(5,5,5,5))
b2 = barplot(prop_tbl2,beside = TRUE, ylim = c(0,1), xlab = "Sex",
        ylab = "Proportion", main = "Approval Rates By Sex", col = c("lightblue", "navy"))
legend("topright", legend = c("Denied", "Approved"), fill = c("lightblue", "navy"), title = "Decision")
text(3.5,.85, label = lab, font=2)
text(x = b2, y = prop_tbl2 + .025, labels = round(prop_tbl2,2))
#text(x = b2, y = prop_tbl2 + .025, labels = paste(round(prop_tbl2,3)*100, "%"))

### DENIAL REASON BY RACE BARPLOT
rzns = c("NA", "Mortgatge Insurance Denied", "Exempt")
data_deni = data_race %>% filter(!(data_race$denial_reason.1 %in% rzns))
tbl3 = with(data_deni, table(denial_reason.1, derived_race))
tbl3
prop_tbl3 = proportions(tbl3, margin = 2)
rnbw = c("red", "orange", "yellow", "green", "blue", "purple", "white", "black")
barplot(prop_tbl3, beside = TRUE, ylim = c(0,.6), xlab = "Race",
        ylab = "Proportion", main = "Proportion of Denial Reasons By Race", col = rnbw)
legend("topright", legend = rownames(prop_tbl3), fill = rnbw, title = "Denial Reason")


### DENIAL REASON BY RACE BARPLOT 2
rzns = c("NA", "Mortgatge Insurance Denied", "Exempt")
data_deni = data_race %>% filter(!(data_race$denial_reason.1 %in% rzns))
tbl4 = with(data_deni, table(derived_race,denial_reason.1))
tbl4
prop_tbl4 = proportions(tbl4, margin = 1)
cols = c("lightblue", "black", "grey", "white" )
dr = "Bars show the proportion each denial reason appears by race"
par(mar = c(5, 13, 5, 5)) # Set the margin on all sides to 2
b4 = barplot(prop_tbl4, horiz = T, beside = TRUE, las = 2, xlim = c(0,.5), xlab = "Proportion", main = "Proportion of Denial Reasons By Race", col = cols)
legend("topright", legend = rownames(prop_tbl4), fill = cols, title = "Race")
text(.33, 3.5, label = dr, font=2, cex = .80)
text(x = prop_tbl4 + .012, y = b4+.02,  labels = round(prop_tbl4,2), cex = .70)

ggplot(data = data_race, aes(derived_race, debt_to_income_ratio)) +
  geom_boxplot() + xlab("Race") + ylab("DTI") + ggtitle("Debt to Income ratio by Race")

par(mar=c(5,5,5,5))
tbl5 = with(data_race, table(approval,dti_range))
tbl5
prop_tbl5 = proportions(tbl5, margin = 2)
b5 = barplot(prop_tbl5, beside = T, legend = F, xlab = "DTI Range",
             ylab = "Proportion", main = "Proportion of Approval By DTI Range", ylim = c(0, 1), col = c("lightblue", "navy"))
text(x = b5, y = prop_tbl5 + .05, labels = round(prop_tbl5,2))
legend(x = 7, y = 1, legend = c("Denied", "Approved"), fill = c("lightblue", "navy"), title = "Approval")
