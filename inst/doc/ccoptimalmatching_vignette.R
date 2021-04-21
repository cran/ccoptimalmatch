## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
1 + 1

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("ccoptimalmatch")

## ----eval=FALSE---------------------------------------------------------------
#  library(ccoptimalmatch)

## ---- echo=FALSE--------------------------------------------------------------
data(being_processed, package = "ccoptimalmatch")

## -----------------------------------------------------------------------------
head(being_processed)

## ---- eval=FALSE--------------------------------------------------------------
#  help("being_processed")

## ---- eval=FALSE--------------------------------------------------------------
#  being_processed$case_control

## -----------------------------------------------------------------------------
table(being_processed$case_control)

## ---- echo=FALSE--------------------------------------------------------------
data(not_processed, package = "ccoptimalmatch")

## -----------------------------------------------------------------------------
head(not_processed)

## -----------------------------------------------------------------------------
table(not_processed$case_control)

## ---- echo=F, results='hide',message=FALSE------------------------------------
library(dplyr)

## -----------------------------------------------------------------------------
create_subset <- not_processed %>% 
                 filter(case_control =="case") %>%
                 arrange(Practice_Id, Gender, JCG) %>%
                 distinct(Gender, JCG, Practice_Id, .keep_all = TRUE) %>%
                 mutate(subset = 1:n()) %>%
                 select(Gender, JCG, Practice_Id, subset)

## -----------------------------------------------------------------------------
head(create_subset)

## -----------------------------------------------------------------------------
case_with_subset <- not_processed %>% 
                          filter(case_control =="case") %>%
                           full_join(create_subset, by = c("Gender", "JCG", "Practice_Id"))

## -----------------------------------------------------------------------------
control_with_subset <- not_processed %>% 
                             filter(case_control =="control") %>%
                             right_join(create_subset, by = c("Gender", "JCG", "Practice_Id"))

## -----------------------------------------------------------------------------
not_processed <- rbind(case_with_subset,control_with_subset)

## -----------------------------------------------------------------------------
table(not_processed$case_control)

## -----------------------------------------------------------------------------
bdd_controls <- not_processed[not_processed$case_control=="control",]
bdd_controls$cluster_case <- 0
bdd_cases <- not_processed[not_processed$case_control=="case",]
bdd_cases$cluster_case <- paste("case",1:nrow(bdd_cases),sep = "_")

## -----------------------------------------------------------------------------
not_processed <- rbind(bdd_cases,bdd_controls)
not_processed$age <- not_processed$JCG-not_processed$Birth_Year 

## ---- echo=F, results='hide',message=FALSE------------------------------------
not_processed <- as.data.frame(not_processed)

## -----------------------------------------------------------------------------
bdd_cases <- not_processed[not_processed$case_control=="case",]
bdd_control <- not_processed[not_processed$case_control=="control",]

## -----------------------------------------------------------------------------
bdd_temp <- data.frame()
list_p <- unique(bdd_cases$cluster_case)

## -----------------------------------------------------------------------------
for(i in 1:length(list_p)){
 temp <- bdd_cases[bdd_cases$cluster_case==list_p[i],]
 subset_identified <- temp$subset
 temp0 <- bdd_control[bdd_control$subset==temp$subset,]
 temp_final <- rbind(temp,temp0)
 temp_final$cluster_case <- list_p[i]
 temp_final=temp_final %>%
            group_by(cluster_case) %>%
            mutate(age_diff = abs(age - age[case_control=="case"]),
            fup_diff = foll_up - foll_up[case_control=="case"])
 temp_final$age_fup <- ifelse(temp_final$age_diff<=2&temp_final$fup_diff==0,"accept","delete")
 temp_final <- temp_final[temp_final$age_fup=="accept",]
 temp_final$age_fup <- NULL
 bdd_temp <- rbind(bdd_temp,temp_final)
}

## -----------------------------------------------------------------------------
table(bdd_temp$case_control)

## -----------------------------------------------------------------------------
bdd_temp = bdd_temp %>% group_by(cluster_case) %>% mutate(total_control_per_case = n()-1)
bdd_temp$case_ind <- ifelse(bdd_temp$case_control=="case",1,0)
bdd_temp <- subset(bdd_temp, select=c(cluster_case, Patient_Id, case_control, case_ind,
                      JCG, entry_year, CI, age_diff, fup_diff, total_control_per_case))

## -----------------------------------------------------------------------------
bdd_temp = bdd_temp %>% group_by(Patient_Id) %>% mutate(freq_of_controls = n())

## ---- echo=F, results='hide',message=FALSE------------------------------------
bdd_temp <- as.data.frame(bdd_temp)

## -----------------------------------------------------------------------------
head(bdd_temp, 10)

## -----------------------------------------------------------------------------
bdd_temp<-bdd_temp[order(bdd_temp$cluster_case,bdd_temp$case_control,bdd_temp$fup_diff,
                         bdd_temp$age_diff,bdd_temp$freq_of_controls),]

## -----------------------------------------------------------------------------
head(bdd_temp, 10)

## ---- echo=F, results='hide',message=FALSE------------------------------------
library(ccoptimalmatch)

## -----------------------------------------------------------------------------
final_data <- optimal_matching(bdd_temp, n_con=4, cluster_case, Patient_Id, 
                               total_control_per_case, case_control, with_replacement = FALSE)

## ---- echo=F, results='hide',message=FALSE------------------------------------
final_data <- as.data.frame(final_data)

## -----------------------------------------------------------------------------
final_data <- final_data %>% arrange(cluster_case)
head(final_data,20)

## -----------------------------------------------------------------------------
final_data = final_data %>% group_by(cluster_case) %>% mutate(total_control_matched = n()-1)
table(final_data$case_control,final_data$total_control_matched)

