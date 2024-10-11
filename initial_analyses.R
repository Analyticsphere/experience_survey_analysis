## Description =================================================================
# Title:        Connect Experience Survey Analytics
# Author:       Leila Orszag
# Date:         2024-10-10
# Objective:    Metrics for experience survey
# GH-Issue:     Github Issue


## Script Parameters ===========================================================

# Use can modify these
dataset <- "FlatConnect"
table   <- "participants_JP"
tier    <- "dev" # "dev", "stg", or "prod"

project <- switch(tier,
                  dev  = "nih-nci-dceg-connect-dev",
                  stg  = "nih-nci-dceg-connect-stg-5519",
                  prod = "nih-nci-dceg-connect-prod-6d04")

billing <- project # Use the same project for billing, always.


## Dependencies ================================================================



## Connect to Database =========================================================
bq_auth() # Authenticate with BigQuery

# Establish connection to BigQuery
con_completion <- dbConnect(bigrquery::bigquery(), 
                            project=project, 
                            dataset=dataset, 
                            billing=billing)

# Specify just the data we need with a query
sql <- glue(
  "
              SELECT d_956490759
              FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP`
            ")

# Query the data and store as reference object
data_completion <- dbGetQuery(con_completion, sql)
data_completion2 <- tbl(con_completion, "nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP")

data_completion2 %>%  select(d_956490759) %>%  count(d_956490759) %>%
  mutate (survey_completion = case_when(d_956490759 == '972455046'  ~ 'Not Started',
                                        d_956490759 == '615768760' ~ 'Started',
                                        d_956490759 == '231311385' ~ 'Submitted')) %>%
  collect()
# Transform Data ===============================================================
data_completion <- data_completion %>%
  count(d_956490759) %>%
  mutate (survey_completion = case_when(d_956490759 == '972455046'  ~ 'Not Started',
                                        d_956490759 == '615768760' ~ 'Started',
                                        d_956490759 == '231311385' ~ 'Submitted')) %>%
  collect()

total_eligible = sum(data_completion$n[1:3])
data_completion$percentage = data_completion$n/(total_eligible)*100
data_completion = data_completion[1:3, ]

# Visualize Data ===============================================================
gt_completion = data_completion %>%
  gt::gt()%>%
  tab_header(title = "Survey Completion") %>% 
  fmt_number(columns = "percentage", decimals = 2) %>%
  cols_label(n= md("**Number of Participants**"), percentage = md("**Percentage of Participants**"), survey_completion = md("**Survey Completion**")) %>%
  cols_hide(d_956490759) %>%
  cols_move(n, survey_completion)

print(gt_completion)

# Unused large query================================================

# Specify just the data we need with a query
sql2 <- glue(
  "
              SELECT 
                  D_260186214 as device_my_connect,
                  D_875017278 as my_connect_ease,
                  D_630940888 as technical_issues,
                  D_646060480 as finding_my_connect,
                  D_124830305 as comms,
                  D_586132480 as first_survey_completion,
                  D_886084185_D_126388230 as why_not_first_survey,
                  D_945546878 as length_first_survey,
                  D_472709337 as biospecimen_donation,
                  D_890945599 as comms_biospeciment_donation,
                  D_465287908 as time_biospecimen_donation,
                  D_956625094 as ease_donating_samples,
                  D_476960744 as exp_dontating_samples,
                  D_307813936 as sample_survey_completion,
                  D_800057241 as at_visit_sample_survey,
                  D_943119849_D_943119849 as why_not_sample_survey,
                  D_482763096 as survey_length_pref,
                  D_176469609 as overall_experience,
                  D_960544981_d_101837333 as why_joined_nci, 
                  D_960544981_d_313446770 as why_joined_team, 
                  D_960544981_d_393996571 as why_joined_cancer_research, 
                  D_960544981_d_490731188 as why_joined_hc_system_rep, 
                  D_960544981_d_604524950 as why_joined_cancer_personal_impact, 
                  D_960544981_d_815468840 as why_joined_like_research, 
                  D_960544981_d_847753225 as why_joined_payment, 
                  D_960544981_d_925993577 as why_joined_help_other,
                  D_960544981_d_985468594 as why_joined_friend_involved
                  
              FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.experience2024_JP`
            ")
data_survey = gsub("353358909", "Yes", data_survey)
data_survey = gsub("104430631", "No", data_survey)
data_survey = gsub("242016061", "Too many", data_survey)
data_survey = gsub("185898646", "Just enough", data_survey)
data_survey = gsub("803970496", "Not enough", data_survey)
data_survey = gsub("178420302", "Unsure", data_survey)
data_survey = gsub("198030665", "Yes, completed all", data_survey)
data_survey = gsub("687057737", "Yes, completed some", data_survey)
data_survey = gsub("333628672", "No, completed none", data_survey)

# Second Query==========================================
sql3 = glue("
  SELECT D_875017278 as my_connect_ease
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.experience2024_JP`")

# Query the data and store as reference object
data_survey <- dbGetQuery(con_completion, sql3)


# Transform Data ===============================================================
data_survey_ease<- data_survey %>%
  count(my_connect_ease) %>%
  collect()
data_survey_ease = data.frame(data_survey_ease)
  
data_survey_ease$my_connect_ease = gsub("371268341", "Very easy", data_survey_ease$my_connect_ease)
data_survey_ease$my_connect_ease = gsub("417633823", "Easy", data_survey_ease$my_connect_ease)
data_survey_ease$my_connect_ease = gsub("308765753", "Neutral", data_survey_ease$my_connect_ease)
data_survey_ease$my_connect_ease = gsub("940801246", "Hard", data_survey_ease$my_connect_ease)
data_survey_ease$my_connect_ease = gsub("527229910", "Very hard", data_survey_ease$my_connect_ease)
data_survey_ease$my_connect_ease[6] = "No response"

data_survey_ease$percentage = data_survey_ease$n/sum(data_survey_ease$n)*100
data_survey_ease$my_connect_ease = factor(data_survey_ease$my_connect_ease, levels = c("Very easy", "Easy", "Neutral", "Hard", "Very hard", "No response"))

# Visualize Data ===============================================================
gt_ease = data_survey_ease %>%
  arrange(my_connect_ease)%>%
  gt::gt()%>%
  tab_header(title = "How easy or hard is it to use MyConnect?") %>% 
  fmt_number(columns = "percentage", decimals = 2) %>%
  cols_label(n= md("**Number of Participants**"), percentage = md("**Percentage of Participants**"), my_connect_ease = md("**Ease of Completion**")) %>%
  cols_move(n, my_connect_ease)

print(gt_ease)

# Third Query==========================================
sql4 = glue("SELECT
  D_630940888 as technical_issues
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.experience2024_JP`")

# Query the data and store as reference object
data_survey <- dbGetQuery(con_completion, sql4)


# Transform Data ===============================================================
data_survey_tech<- data_survey %>%
  count(technical_issues) %>%
  collect()
data_survey_tech = data.frame(data_survey_tech)

data_survey_tech$technical_issues = gsub("353358909", "Yes", data_survey_tech$technical_issues)
data_survey_tech$technical_issues = gsub("104430631", "No", data_survey_tech$technical_issues)
data_survey_tech$technical_issues[3] = "No response"


data_survey_tech$percentage = data_survey_tech$n/sum(data_survey_tech$n)*100
data_survey_tech$technical_issues = factor(data_survey_tech$technical_issues, levels = c("Yes", "No", "No response"))

# Visualize Data ===============================================================
gt_tech = data_survey_tech %>%
  arrange(technical_issues)%>%
  gt::gt()%>%
  tab_header(title = "Have you experienced technical problems?") %>% 
  fmt_number(columns = "percentage", decimals = 2) %>%
  cols_label(n= md("**Number of Participants**"), percentage = md("**Percentage of Participants**"), technical_issues = md("**Presence of Technical Issues**")) %>%
  cols_move(n, technical_issues)

print(gt_tech)

# Wrap-up checklist ==========================================================
# [] 1. Share results with stakeholder via appropriate Box.com folder
# [] 2. Make sure there is no sensitive data in this script or in my repo
# [] 3. Commit to git with a thoughtful commit message, referencing GH Issue if appropriate. 
# [] 4. Push to GitHub Repository