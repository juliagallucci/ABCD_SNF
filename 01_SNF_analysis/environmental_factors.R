## Organizing ABCD environmental data
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/abcd-general/")

### SOCIODEMOGRAPHIC
########### Income and parental education
demo <- read.csv("abcd_p_demo.csv",  header=TRUE)
demo  <- demo[which(demo$eventname == "baseline_year_1_arm_1"), ]

demo$demo_comb_income_v2 <- as.factor(demo$demo_comb_income_v2)
#1= Less than $5,000; 2=$5,000 through $11,999; 3=$12,000 through $15,999; 4=$16,000 through $24,999; #5=$25,000 through $34,999; 6=$35,000 through $49,999; 7=$50,000 through $74,999; 8= $75,000 through #$99,999; 9=$100,000 through $199,999; 10=$200,000 and greater. 777 = Refuse to answer No deseo responder ; #999 = Don't know
# demo$income <- ifelse(demo$demo_comb_income_v2 == "1", "5000", NA)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "2", "12000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "3", "16000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "4", "25000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "5", "35000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "6", "50000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "7", "75000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "8", "100000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "9", "200000", demo$income)
# demo$income <- ifelse(demo$demo_comb_income_v2 == "10", "250000", demo$income)
demo$income <- demo$demo_comb_income_v2
demo$income <- ifelse(demo$demo_comb_income_v2 == "999", NA, demo$income)
demo$income <- ifelse(demo$demo_comb_income_v2 == "777", NA, demo$income)
demo$income <- as.numeric(demo$income)

table(demo$income, exclude=NULL)

#demo$income = factor(demo$income,levels=c("5000", "12000", "16000", "25000", "35000", 
#                                          "50000", "75000", "100000", "200000", "over200"),ordered=T)

# parental education
demo$demo_prnt_ed_v2 <- as.numeric(demo$demo_prnt_ed_v2)
demo$demo_prnt_ed_v2 <- ifelse(demo$demo_prnt_ed_v2 == "777" | demo$demo_prnt_ed_v2 == "999", NA, demo$demo_prnt_ed_v2)
demo$demo_prtnr_ed_v2 <- as.numeric(demo$demo_prtnr_ed_v2)
demo$demo_prtnr_ed_v2 <- ifelse(demo$demo_prtnr_ed_v2 == "777" | demo$demo_prtnr_ed_v2 == "999", NA, demo$demo_prtnr_ed_v2)

table(demo$demo_prnt_ed_v2, exclude = NULL)
table(demo$demo_prtnr_ed_v2, exclude = NULL)

demo$parental_education <- ifelse(demo$demo_prtnr_ed_v2 > demo$demo_prnt_ed_v2, demo$demo_prtnr_ed_v2, demo$demo_prnt_ed_v2)
demo$parental_education <- ifelse(is.na(demo$demo_prtnr_ed_v2), demo$demo_prnt_ed_v2, demo$parental_education)

table(demo$parental_education, exclude = NULL)

# economic deprivation
demo$demo_fam_exp1_v2 <- as.numeric(demo$demo_fam_exp1_v2)
demo$demo_fam_exp2_v2 <- as.numeric(demo$demo_fam_exp2_v2)
demo$demo_fam_exp3_v2 <- as.numeric(demo$demo_fam_exp3_v2)
demo$demo_fam_exp4_v2 <- as.numeric(demo$demo_fam_exp4_v2)
demo$demo_fam_exp5_v2 <- as.numeric(demo$demo_fam_exp5_v2)
demo$demo_fam_exp6_v2 <- as.numeric(demo$demo_fam_exp6_v2)
demo$demo_fam_exp7_v2 <- as.numeric(demo$demo_fam_exp7_v2)

demo$econ_deprivation <- rowSums(demo[ , c("demo_fam_exp1_v2", "demo_fam_exp2_v2", "demo_fam_exp3_v2", "demo_fam_exp4_v2", 
                                           "demo_fam_exp6_v2", "demo_fam_exp7_v2")])

demo <- demo[ ,c("src_subject_id", "parental_education", "income", "econ_deprivation")]


##### PROXIMAL ENVIRONMENT
## Organizing ABCD environmental data
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/culture-environment/")


# neighbourhood safety - parental report (used in Dylan Gee, both used in Modabbernia)
# mean score
safety <- read.csv("ce_p_nsc.csv", header=TRUE)
safety  <- safety[which(safety$eventname == "baseline_year_1_arm_1"), ]
safety$neighborhood1r_p <- as.numeric(safety$neighborhood1r_p)
safety$neighborhood2r_p <- as.numeric(safety$neighborhood2r_p)
safety$neighborhood3r_p <- as.numeric(safety$neighborhood3r_p)
safety$total_neighbourhood_safety <- (safety$neighborhood1r_p + safety$neighborhood2r_p + safety$neighborhood3r_p)/3
table(safety$total_neighbourhood_safety, exclude=NULL)
safety <- safety[ ,c("src_subject_id", "total_neighbourhood_safety")]

# residential history 
# removed all the biases because they don't have any data, also traffic and urban classification, O3, marijuana
# they are all missing ~694 participants except for elevation which is missing ~2770
# it's not possible to weight the measures by the length of time spent at each address because all duration 
#data is missing. 694 missing from address 1, 10261 missing from address 2 so don't need to worry about that
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/linked-external-data/")
led_l_den <- read.csv("led_l_densbld.csv", header=TRUE)
led_l_den <- led_l_den[which(led_l_den$eventname == "baseline_year_1_arm_1"), ]
led_l_den <- led_l_den[, -2]

led_l_adi <- read.csv("led_l_adi.csv", header=TRUE)
led_l_adi <- led_l_adi[which(led_l_adi$eventname == "baseline_year_1_arm_1"), ]
led_l_adi <- led_l_adi[, -2]

led_l_walk <- read.csv("led_l_walk.csv", header=TRUE)
led_l_walk <- led_l_walk[which(led_l_walk$eventname == "baseline_year_1_arm_1"), ]
led_l_walk <- led_l_walk[, -2]

led_l_crime <- read.csv("led_l_crime.csv", header=TRUE)
led_l_crime <- led_l_crime[which(led_l_crime$eventname == "baseline_year_1_arm_1"), ]
led_l_crime <- led_l_crime[, -2]

led_l_elevation <- read.csv("led_l_elevation.csv", header=TRUE)
led_l_elevation <- led_l_elevation[which(led_l_elevation$eventname == "baseline_year_1_arm_1"), ]
led_l_elevation <- led_l_elevation[, -2]


led_l_road <- read.csv("led_l_roadprox.csv", header=TRUE)
led_l_road <- led_l_road[which(led_l_road$eventname == "baseline_year_1_arm_1"), ]
led_l_road <- led_l_road[, -2]


led_l_leadrisk <- read.csv("led_l_leadrisk.csv", header=TRUE)
led_l_leadrisk <- led_l_leadrisk[which(led_l_leadrisk$eventname == "baseline_year_1_arm_1"), ]
led_l_leadrisk <- led_l_leadrisk[, -2]


led_l_no2 <- read.csv("led_l_no2.csv", header=TRUE)
led_l_no2 <- led_l_no2[which(led_l_no2$eventname == "baseline_year_1_arm_1"), ]
led_l_no2 <- led_l_no2[, -2]

led_l_pm25 <- read.csv("led_l_pm25.csv", header=TRUE)
led_l_pm25 <- led_l_pm25[which(led_l_pm25$eventname == "baseline_year_1_arm_1"), ]
led_l_pm25 <- led_l_pm25[, -2]

led_l_admin <- read.csv("led_l_admin.csv", header=TRUE)
led_l_admin <- led_l_admin[which(led_l_admin$eventname == "baseline_year_1_arm_1"), ]
led_l_admin <- led_l_admin[, -2]



# Merge led_l_den with led_l_adi
combined_data <- merge(led_l_den, led_l_adi, by = "src_subject_id", all = TRUE)

# Merge with led_l_walk
combined_data <- merge(combined_data, led_l_walk, by = "src_subject_id", all = TRUE)

# Merge with led_l_crime
combined_data <- merge(combined_data, led_l_crime, by = "src_subject_id", all = TRUE)

# Merge with led_l_elevation
combined_data <- merge(combined_data, led_l_elevation, by = "src_subject_id", all = TRUE)

# Merge with led_l_road
combined_data <- merge(combined_data, led_l_road, by = "src_subject_id", all = TRUE)

combined_data <- merge(combined_data, led_l_leadrisk, by = "src_subject_id", all = TRUE)

combined_data <- merge(combined_data, led_l_no2, by = "src_subject_id", all = TRUE)

combined_data <- merge(combined_data, led_l_pm25, by = "src_subject_id", all = TRUE)

resid <- data.frame(
  src_subject_id = combined_data$src_subject_id,
  gross_density_addr1 = combined_data$reshist_addr1_d1a,
  gross_density_addr2 = combined_data$reshist_addr2_d1a,
  gross_density_addr3 = combined_data$reshist_addr3_d1a,
  
  ADD_nat_perc_addr1 = combined_data$reshist_addr1_adi_perc,
  ADD_nat_perc_addr2 = combined_data$reshist_addr2_adi_perc,
  ADD_nat_perc_addr3 = combined_data$reshist_addr3_adi_perc,
  
  walkindex_addr1 = combined_data$reshist_addr1_walkindex,
  walkindex_addr2 = combined_data$reshist_addr2_walkindex,
  walkindex_addr3 = combined_data$reshist_addr3_walkindex,
  
  crime_addr1 = combined_data$reshist_addr1_grndtot,
  crime_addr2 = combined_data$reshist_addr2_grndtot,
  crime_addr3 = combined_data$reshist_addr3_grndtot,
  
  elevation_addr1 = combined_data$reshist_addr1_elevation,
  elevation_addr2 = combined_data$reshist_addr2_elevation,
  elevation_addr3 = combined_data$reshist_addr3_elevation,
  
  proximity_to_roads_addr1 = combined_data$reshist_addr1_mjsale,
  proximity_to_roads_addr2 = combined_data$reshist_addr2_mjsale,
  proximity_to_roads_addr3 = combined_data$reshist_addr3_mjsale,
  
  lead_risk_addr1 = combined_data$reshist_addr1_leadrisk,
  lead_risk_addr2 = combined_data$reshist_addr2_leadrisk,
  lead_risk_addr3 = combined_data$reshist_addr3_leadrisk,
  
  no2_pollution_addr1 = combined_data$reshist_addr1_no2_2016_aavg,
  no2_pollution_addr2 = combined_data$reshist_addr2_no2_2016_aavg,
  no2_pollution_addr3 = combined_data$reshist_addr3_no2_2016_aavg,
  
  pm_pollution_addr1 = combined_data$reshist_addr1_pm252016aa,
  pm_pollution_addr2 = combined_data$reshist_addr2_pm252016aa,
  pm_pollution_addr3 = combined_data$reshist_addr3_pm252016aa
) 

# Merge percentiles into the resid data frame
resid <- merge(resid, led_l_admin, by = "src_subject_id", all.x = TRUE)

# need to average these across the different residences that the child has lived 
# Function to calculate the average, ignoring NAs
average_non_na <- function(...) {
  values <- c(...)
  mean(values, na.rm = TRUE)
}

# Calculate the averages for each metric
resid_summary <- resid %>%
  group_by(src_subject_id) %>%
  summarise(
    avg_gross_density = average_non_na(
      gross_density_addr1, gross_density_addr2, gross_density_addr3
    ),
    avg_ADD_nat_perc = average_non_na(
      ADD_nat_perc_addr1, ADD_nat_perc_addr2, ADD_nat_perc_addr3
    ),
    avg_walkindex = average_non_na(
      walkindex_addr1, walkindex_addr2, walkindex_addr3
    ),
    avg_crime = average_non_na(
      crime_addr1, crime_addr2, crime_addr3
    ),
    avg_elevation = average_non_na(
      elevation_addr1, elevation_addr2, elevation_addr3
    ),
    avg_proximity_to_roads = average_non_na(
      proximity_to_roads_addr1, proximity_to_roads_addr2, proximity_to_roads_addr3
    ),
    avg_lead_risk = average_non_na(
      lead_risk_addr1, lead_risk_addr2, lead_risk_addr3
    ),
    avg_no2_pollution = average_non_na(
      no2_pollution_addr1, no2_pollution_addr2, no2_pollution_addr3
    ),
    avg_pm_pollution = average_non_na(
      pm_pollution_addr1, pm_pollution_addr2, pm_pollution_addr3
    )
  ) %>%
  ungroup()


# Replace NaN with NA in all numeric columns of resid_summary
resid_summary[] <- lapply(resid_summary, function(x) {
  if (is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
})


###### SOCIAL ENVIRONMENT
# school risk and protective factors - child report
# school environment and involvement/performance (not sure I should include this) and disengagement
# sum of scores
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/culture-environment/")

school <- read.csv("ce_y_srpf.csv", header=TRUE)
school <- school[which(school$eventname == "baseline_year_1_arm_1"), ]
school[, 3:25] <- apply(school[, 3:25], 2, function(x) as.numeric(x))

# how it's divided up in Modabbernia et al and Hong et al
school$total_school_environment <- rowSums(school[, 3:8]) # questions 2-7
school$total_school_involvement <- school$school_8_y + school$school_9_y + school$school_10_y +
  school$school_12_y
school$total_school_disengagement <- school$school_15_y + school$school_17_y
table(school$total_school_environment, exclude = NULL)

# school environment and engagement
#school$total_school_environment <- rowSums(school[, 10:15]) #Arthur, M. W., Briney, J. S., et al. (2007) Measuring risk and protection in communities using the Communities That Care Youth Survey. Eval Program Plann 30(2): 197-211

school <- school[ ,c("src_subject_id", "total_school_environment")]

# 

# Children's Report of Parental Behavioral Inventory
# parental support and acceptance
# primary NAs = 39, secondary NAs = 924
# mean score
parents <- read.csv("ce_y_crpbi.csv", header=TRUE)
#parents <- parents[-1, ]
parents <- parents[which(parents$eventname == "baseline_year_1_arm_1"), ]
parents$crpbi_parent1_y <- as.numeric(parents$crpbi_parent1_y)
parents$crpbi_parent2_y <- as.numeric(parents$crpbi_parent2_y)
parents$crpbi_parent3_y <- as.numeric(parents$crpbi_parent3_y)
parents$crpbi_parent4_y <- as.numeric(parents$crpbi_parent4_y)
parents$crpbi_parent5_y <- as.numeric(parents$crpbi_parent5_y)
table(parents$crpbi_parent5_y, exclude = NULL)
parents$total_parental_support <- (parents$crpbi_parent1_y + parents$crpbi_parent2_y + parents$crpbi_parent3_y + 
  parents$crpbi_parent4_y + parents$crpbi_parent5_y)/5
table(parents$total_parental_support, exclude = NULL)

# second caregiver report - more missing information so probably excluding
parents$crpbi_caregiver12_y <- as.numeric(parents$crpbi_caregiver12_y)
parents$crpbi_caregiver13_y <- as.numeric(parents$crpbi_caregiver13_y)
parents$crpbi_caregiver14_y <- as.numeric(parents$crpbi_caregiver14_y)
parents$crpbi_caregiver15_y <- as.numeric(parents$crpbi_caregiver15_y)
parents$crpbi_caregiver16_y <- as.numeric(parents$crpbi_caregiver16_y)

parents$total_caregiver_support <- (parents$crpbi_caregiver12_y + parents$crpbi_caregiver13_y + parents$crpbi_caregiver14_y +
  parents$crpbi_caregiver15_y + parents$crpbi_caregiver16_y)/5
table(parents$total_caregiver_support, exclude = NULL)

parents <- parents[ ,c("src_subject_id", "total_parental_support", "total_caregiver_support")]

#caregiver support (mean score of second five items from the ABCD Children's Report of  Parental  Behavioral  Inventory)

## Family conflict - (parent report in Gee and both in Modabbernia)
# sum of 9 questions, parent report
#only 6000 kids with stimulation data so far
conflict <- read.csv("ce_p_fes.csv", header=TRUE)
conflict <- conflict[which(conflict$eventname == "baseline_year_1_arm_1"), ]
#conflict <- conflict[-1, ]
#table(conflict[ ,10], exclude = NULL)
conflict[, 3:87] <- apply(conflict[, 3:87], 2, function(x) as.numeric(x))
conflict$family_conflict_total <- rowSums(conflict[, 3:11])
table(conflict$family_conflict_total, exclude = NULL)

conflict <- conflict[ ,c("src_subject_id", "family_conflict_total")]
conflict <- unique(conflict, by="src_subject_id")


## traumatic events - parent report
# sum score
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/mental-health/")
trauma <- read.csv("mh_p_ksads_ptsd.csv", header=TRUE)
trauma <- trauma[which(trauma$eventname == "baseline_year_1_arm_1"), ]

#table(trauma[ ,10], exclude = NULL)
trauma[, 3:19] <- apply(trauma[, 3:19], 2, function(x) as.numeric(x))
trauma$traumatic_events_total <- rowSums(trauma[, 3:19])

table(trauma$traumatic_events_total, exclude = NULL)

trauma <- trauma[ ,c("src_subject_id", "traumatic_events_total")]

### parental monitoring - youth report
# mean score
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/culture-environment/")

monitoring <- read.csv("ce_y_pm.csv", header=TRUE)

monitoring <- monitoring[which(monitoring$eventname == "baseline_year_1_arm_1"), ]
monitoring$parent_monitor_q1_y <- as.numeric(monitoring$parent_monitor_q1_y)
monitoring$parent_monitor_q2_y <- as.numeric(monitoring$parent_monitor_q2_y)
monitoring$parent_monitor_q3_y <- as.numeric(monitoring$parent_monitor_q3_y)
monitoring$parent_monitor_q4_y <- as.numeric(monitoring$parent_monitor_q4_y)
monitoring$parent_monitor_q5_y <- as.numeric(monitoring$parent_monitor_q5_y)
monitoring$parental_monitoring_total <- (monitoring$parent_monitor_q1_y + monitoring$parent_monitor_q2_y +
  monitoring$parent_monitor_q3_y + monitoring$parent_monitor_q4_y + monitoring$parent_monitor_q5_y)/5

table(monitoring$parental_monitoring_total, exclude = NULL)
monitoring <- monitoring[ ,c("src_subject_id", "parental_monitoring_total")]

## number of friends

#### DEVELOPMENTAL 
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/physical-health//")
develop <- read.csv("ph_p_dhx.csv", header=TRUE)
develop <- develop[which(develop$eventname == "baseline_year_1_arm_1"), ]
#develop <- develop[-1, ]
develop[, 3:270] <- apply(develop[, 3:270], 2, function(x) as.numeric(x))
develop[develop == 999] <- NA
develop$maternal_age <- develop$devhx_3_p
develop$paternal_age <- develop$devhx_4_p
develop$paternal_age <- ifelse(develop$paternal_age > 100, NA, develop$paternal_age )
develop$birth_weight <- ifelse(is.na(develop$birth_weight_oz),develop$birth_weight_lbs, (develop$birth_weight_lbs*16 + develop$birth_weight_oz)/16)
#develop$caffeine_during <- develop$devhx_caffeine_11 # there are follow up questions about how much
#develop$caffeine_during <- ifelse(develop$caffeine_during == "999", NA, develop$caffeine_during)
develop$doctor_visits <- develop$devhx_11_p
develop$premature <- ifelse(develop$devhx_12a_p == "0", develop$devhx_12a_p, develop$devhx_12_p)
develop$premature <- ifelse(develop$premature == "999", NA, develop$premature)
develop$fever <- develop$devhx_16_p
develop$fever <- ifelse(develop$fever == "999", NA, develop$fever)
develop$fever <- ifelse(develop$fever == "9990", NA, develop$fever)
develop$illness <- develop$devhx_17_p
develop$breastfeeding <- develop$devhx_18_p
develop$verbal_development <- develop$devhx_21_p
develop$verbal_development <- ifelse(develop$verbal_development == "999", NA, develop$verbal_development)
develop$motor_development <- develop$devhx_20_p
develop$motor_development <- ifelse(develop$motor_development == "999", NA, develop$motor_development)
#develop$bedwetting <- ifelse(develop$devhx_22_3_p == "0", develop$devhx_22_3_p, develop$devhx_23b_p)
#develop$bedwetting <- ifelse(develop$bedwetting == "999", NA, develop$bedwetting)

develop$pregnancy_complications <- rowSums(develop[c("devhx_10a3_p", "devhx_10b3_p", "devhx_10c3_p", "devhx_10d3_p", 
                                                     "devhx_10e3_p", "devhx_10f3_p", "devhx_10g3_p", "devhx_10h3_p", "devhx_10i3_p", 
                                                     "devhx_10j3_p", "devhx_10k3_p", "devhx_10l3_p", "devhx_10m3_p")], na.rm = TRUE)
develop$pregnancy_complications <- ifelse(is.na(develop$devhx_10a3_p) & is.na(develop$devhx_10b3_p) & is.na(develop$devhx_10d3_p) & is.na(develop$devhx_10e3_p)
                                          & is.na(develop$devhx_10f3_p) & is.na(develop$devhx_10g3_p) & is.na(develop$devhx_10h3_p) & is.na(develop$devhx_10i3_p)
                                          & is.na(develop$devhx_10j3_p) & is.na(develop$devhx_10k3_p) & is.na(develop$devhx_10m3_p) & is.na(develop$devhx_10c3_p) & is.na(develop$devhx_10l3_p), NA, develop$pregnancy_complications)
 
develop$birth_complications <- rowSums(develop[c("devhx_13_3_p", "devhx_14a3_p", "devhx_14b3_p", "devhx_14c3_p", 
                                                 "devhx_14d3_p", "devhx_14e3_p", "devhx_14f3_p", "devhx_14g3_p", "devhx_14h3_p")], na.rm = TRUE)
develop$birth_complications <- ifelse(is.na(develop$devhx_13_3_p) & is.na(develop$devhx_14a3_p) & is.na(develop$devhx_14b3_p) & is.na(develop$devhx_14c3_p)
                                          & is.na(develop$devhx_14d3_p) & is.na(develop$devhx_14e3_p) & is.na(develop$devhx_14f3_p) & is.na(develop$devhx_14g3_p)
                                          & is.na(develop$devhx_14h3_p), NA, develop$birth_complications)


develop$maternal_substance_use <- rowSums(develop[c("devhx_9_tobacco", "devhx_9_alcohol", "devhx_9_marijuana", 
     "devhx_9_coc_crack", "devhx_9_her_morph", "devhx_9_oxycont")])
develop$maternal_substance_use <- ifelse(is.na(develop$devhx_9_tobacco) & is.na(develop$devhx_9_alcohol) & is.na(develop$devhx_9_marijuana) & is.na(develop$devhx_9_her_morph)
                                      & is.na(develop$devhx_9_coc_crack) & is.na(develop$devhx_9_oxycont), NA, develop$maternal_substance_use)


# develop$preg_comp_check <- develop$devhx_10a3_p
# develop$substance_check <- develop$devhx_9_marijuana
# develop$birth_comp_check <-develop$devhx_14a3_p

develop <- develop[ ,c("src_subject_id", "maternal_substance_use", "birth_complications", "pregnancy_complications", "motor_development", 
                      "verbal_development", "breastfeeding",  "illness", "fever", "premature", "doctor_visits", 
                      "birth_weight", "paternal_age", "maternal_age")]

#add maternal substance  use
tbi <- read.csv("ph_p_otbi.csv", header=TRUE)
#tbi <- tbi[-1, ]
tbi <- tbi[which(tbi$eventname == "baseline_year_1_arm_1"), ]

tbi$tbi_ss_worst_overall <- as.numeric(tbi$tbi_ss_worst_overall)
table(tbi$tbi_ss_worst_overall)

tbi <- tbi[,c("src_subject_id", "tbi_ss_worst_overall")]
tbi <- unique(tbi, by="src_subject_id")


#### PARENT PSYCHOPATHOLOGY
# family history of mental health / psychopathology and mental health
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/mental-health/")
famhist_1 <- read.csv("mh_p_fhx.csv", header=TRUE)
famhist_1 <- famhist_1[which(famhist_1$eventname == "baseline_year_1_arm_1"), ]
#famhist_1 <- famhist_1[-1, ]
famhist_1[famhist_1 == 999] <- 0
famhist_1[, 3:1863] <- apply(famhist_1[, 3:1863], 2, function(x) as.numeric(x))

# 1 means problems, 0 no means problems

famhist_1$alcohol <- 0
famhist_1$alcohol <- rowSums(famhist_1[,c("famhx4a_p___0", "famhx_4d_p___0", "q4k_full_sib_young1_alc___0",
           "q4k_full_sib_young2_alc___0", "q4k_full_sib_young3_alc___0", "q4k_full_sib_young4_alc___0", 
           "q4k_full_sib_young5_alc___0",  "q4l_full_sib_old1_alc___3", "q4l_full_sib_old2_alc___3",
           "q4l_full_sib_old3_alc___3", "q4l_full_sib_old4_alc___3", "q4l_full_sib_old5_alc___3")], na.rm=TRUE)
famhist_1$alcohol <- ifelse(is.na(famhist_1$famhx4a_p___0) & is.na(famhist_1$famhx_4d_p___0) & is.na(famhist_1$q4k_full_sib_young1_alc___0) & is.na(famhist_1$q4k_full_sib_young2_alc___0) & is.na(famhist_1$q4k_full_sib_young3_alc___0) & is.na(famhist_1$q4k_full_sib_young4_alc___0) & is.na(famhist_1$q4k_full_sib_young5_alc___0) & is.na(famhist_1$q4l_full_sib_old1_alc___3) & is.na(famhist_1$q4l_full_sib_old2_alc___3) & is.na(famhist_1$q4l_full_sib_old3_alc___3) & is.na(famhist_1$q4l_full_sib_old4_alc___3) & is.na(famhist_1$q4l_full_sib_old5_alc___3), NA, famhist_1$alcohol)
famhist_1$alcohol <- ifelse(famhist_1$famhx_4_p == "0", 0, famhist_1$alcohol)

famhist_1$drugs <- 0
famhist_1$drugs <- rowSums(famhist_1[,c("fam_history_q5a_drugs___0", "fam_history_q5d_drugs___0", "q5k_full_sib_young1_drugs___0",
     "q5k_full_sib_young2_drugs___0", "q5k_full_sib_young3_drugs___0", "q5k_full_sib_young4_drugs___0", 
     "q5k_full_sib_young5_drugs___0",  "q5l_full_sib_old1_drugs___3", "q5l_full_sib_old2_drugs___3",
     "q5l_full_sib_old3_drugs___3", "q5l_full_sib_old4_drugs___3", "q5l_full_sib_old5_drugs___3")], na.rm=TRUE)
famhist_1$drugs <- ifelse(is.na(famhist_1$fam_history_q5d_drugs___0) & is.na(famhist_1$fam_history_q5a_drugs___0) & is.na(famhist_1$q5k_full_sib_young1_drugs___0) & is.na(famhist_1$q5k_full_sib_young2_drugs___0) & is.na(famhist_1$q5k_full_sib_young3_drugs___0) & is.na(famhist_1$q5k_full_sib_young4_drugs___0) & is.na(famhist_1$q5k_full_sib_young5_drugs___0) & is.na(famhist_1$q5l_full_sib_old1_drugs___3)
                            & is.na(famhist_1$q5l_full_sib_old2_drugs___3) & is.na(famhist_1$q5l_full_sib_old3_drugs___3) & is.na(famhist_1$q5l_full_sib_old4_drugs___3) & is.na(famhist_1$q5l_full_sib_old5_drugs___3), NA, famhist_1$drugs)
famhist_1$drugs <- ifelse(famhist_1$fam_history_5_yes_no == "0", 0, famhist_1$drugs)


famhist_1$depression <- 0
famhist_1$depression <- rowSums(famhist_1[,c("fam_history_q6a_depression", "fam_history_q6d_depression", "q6k_full_sib_young1_depression",
                                        "q6k_full_sib_young2_depression", "q6k_full_sib_young3_depression", "q6k_full_sib_young4_depression", 
                                        "q6k_full_sib_young5_depression",  "q6l_full_sib_old1_depression", "q6l_full_sib_old2_depression",
                                        "q6l_full_sib_old3_depression", "q6l_full_sib_old4_depression", "q6l_full_sib_old5_depression")], na.rm=TRUE)
famhist_1$depression <- ifelse(is.na(famhist_1$fam_history_q6d_depression) & is.na(famhist_1$fam_history_q6a_depression) & is.na(famhist_1$q6k_full_sib_young1_depression) & is.na(famhist_1$q6k_full_sib_young2_depression) & is.na(famhist_1$q6k_full_sib_young3_depression) & is.na(famhist_1$q6k_full_sib_young4_depression) & is.na(famhist_1$q6k_full_sib_young5_depression) & is.na(famhist_1$q6l_full_sib_old1_depression)
                                                & is.na(famhist_1$q6l_full_sib_old2_depression) & is.na(famhist_1$q6l_full_sib_old3_depression) & is.na(famhist_1$q6l_full_sib_old4_depression) & is.na(famhist_1$q6l_full_sib_old5_depression), NA, famhist_1$depression)
famhist_1$depression <- ifelse(famhist_1$fam_history_6_yes_no == "0", 0, famhist_1$depression)

famhist_1$mania <- 0
famhist_1$mania <- rowSums(famhist_1[,c("fam_history_q7a_mania", "fam_history_q7d_mania", "q7k_full_sib_young1_mania",
                                        "q7k_full_sib_young2_mania", "q7k_full_sib_young3_mania", "q7k_full_sib_young4_mania", 
                                        "q7k_full_sib_young5_mania",  "q7l_full_sib_old1_mania", "q7l_full_sib_old2_mania",
                                        "q7l_full_sib_old3_mania", "q7l_full_sib_old4_mania", "q7l_full_sib_old5_mania")], na.rm=TRUE)
famhist_1$mania <- ifelse(is.na(famhist_1$fam_history_q7d_mania) & is.na(famhist_1$fam_history_q7a_mania) & is.na(famhist_1$q7k_full_sib_young1_mania) & is.na(famhist_1$q7k_full_sib_young2_mania)
                               & is.na(famhist_1$q7k_full_sib_young3_mania) & is.na(famhist_1$q7k_full_sib_young4_mania) & is.na(famhist_1$q7k_full_sib_young5_mania) & is.na(famhist_1$q7l_full_sib_old1_mania)
                               & is.na(famhist_1$q7l_full_sib_old2_mania) & is.na(famhist_1$q7l_full_sib_old3_mania) & is.na(famhist_1$q7l_full_sib_old4_mania) & is.na(famhist_1$q7l_full_sib_old5_mania), NA, famhist_1$mania)
famhist_1$mania <- ifelse(famhist_1$fam_history_7_yes_no == "0", 0, famhist_1$mania)                                                                                                                                                                                                                                                                  

famhist_1$vision <- 0
famhist_1$vision <- rowSums(famhist_1[,c("fam_history_q8a_visions", "fam_history_q8d_visions", "q8k_full_sib_young1_visions",
                                        "q8k_full_sib_young2_visions", "q8k_full_sib_young3_visions", "q8k_full_sib_young4_visions", 
                                        "q8k_full_sib_young5_visions",  "q8l_full_sib_old1_visions", "q8l_full_sib_old2_visions",
                                        "q8l_full_sib_old3_visions", "q8l_full_sib_old4_visions", "q8l_full_sib_old5_visions")], na.rm=TRUE)
famhist_1$vision <- ifelse(is.na(famhist_1$fam_history_q8d_visions) & is.na(famhist_1$fam_history_q8a_visions) & is.na(famhist_1$q8k_full_sib_young1_visions) & is.na(famhist_1$q8k_full_sib_young2_visions)
                          & is.na(famhist_1$q8k_full_sib_young3_visions) & is.na(famhist_1$q8k_full_sib_young4_visions) & is.na(famhist_1$q8k_full_sib_young5_visions) & is.na(famhist_1$q8l_full_sib_old1_visions)
                       & is.na(famhist_1$q8l_full_sib_old2_visions) & is.na(famhist_1$q8l_full_sib_old3_visions) & is.na(famhist_1$q8l_full_sib_old4_visions) & is.na(famhist_1$q8l_full_sib_old5_visions), NA, famhist_1$vision)
famhist_1$vision <- ifelse(famhist_1$fam_history_8_yes_no == "0", 0, famhist_1$vision)                                                                                                                                                                                                                                                                  

                                                                                                                                                                                                                                         

famhist_1$trouble <- 0
famhist_1$trouble <- rowSums(famhist_1[,c("fam_history_q9a_trouble", "fam_history_q9d_trouble", "q9k_full_sib_young1_trouble",
                                         "q9k_full_sib_young2_trouble", "q9k_full_sib_young3_trouble", "q9k_full_sib_young4_trouble", 
                                         "q9k_full_sib_young5_trouble",  "q9l_full_sib_old1_trouble", "q9l_full_sib_old2_trouble",
                                         "q9l_full_sib_old3_trouble", "q9l_full_sib_old4_trouble", "q9l_full_sib_old5_trouble")], na.rm=TRUE)
famhist_1$trouble <- ifelse(is.na(famhist_1$fam_history_q9d_trouble) & is.na(famhist_1$fam_history_q9a_trouble) & is.na(famhist_1$q9k_full_sib_young1_trouble) & is.na(famhist_1$q9k_full_sib_young2_trouble)
                           & is.na(famhist_1$q9k_full_sib_young3_trouble) & is.na(famhist_1$q9k_full_sib_young4_trouble) & is.na(famhist_1$q9k_full_sib_young5_trouble) & is.na(famhist_1$q9l_full_sib_old1_trouble)
                           & is.na(famhist_1$q9l_full_sib_old2_trouble) & is.na(famhist_1$q9l_full_sib_old3_trouble) & is.na(famhist_1$q9l_full_sib_old4_trouble) & is.na(famhist_1$q9l_full_sib_old5_trouble), NA, famhist_1$trouble)
famhist_1$trouble <- ifelse(famhist_1$fam_history_9_yes_no == "0", 0, famhist_1$trouble)                                                                                                                                                                                                                                                                  


famhist_1$nerves <- 0
famhist_1$nerves <- rowSums(famhist_1[,c("fam_history_q10a_nerves", "fam_history_q10d_nerves", "q10k_full_sib_young1_nerves",
                                          "q10k_full_sib_young2_nerves", "q10k_full_sib_young3_nerves", "q10k_full_sib_young4_nerves", 
                                          "q10k_full_sib_young5_nerves",  "q10l_full_sib_old1_nerves", "q10l_full_sib_old2_nerves",
                                          "q10l_full_sib_old3_nerves", "q10l_full_sib_old4_nerves", "q10l_full_sib_old5_nerves")], na.rm=TRUE)
famhist_1$nerves <- ifelse(is.na(famhist_1$fam_history_q10d_nerves) & is.na(famhist_1$fam_history_q10a_nerves) & is.na(famhist_1$q10k_full_sib_young1_nerves) & is.na(famhist_1$q10k_full_sib_young2_nerves)
                           & is.na(famhist_1$q10k_full_sib_young3_nerves) & is.na(famhist_1$q10k_full_sib_young4_nerves) & is.na(famhist_1$q10k_full_sib_young5_nerves) & is.na(famhist_1$q10l_full_sib_old1_nerves)
                           & is.na(famhist_1$q10l_full_sib_old2_nerves) & is.na(famhist_1$q10l_full_sib_old3_nerves) & is.na(famhist_1$q10l_full_sib_old4_nerves) & is.na(famhist_1$q10l_full_sib_old5_nerves), NA, famhist_1$nerves)
famhist_1$nerves <- ifelse(famhist_1$fam_history_10_yes_no == "0", 0, famhist_1$nerves)


famhist_1$suicide <- 0
famhist_1$suicide <- rowSums(famhist_1[,c("fam_history_q13a_suicide", "fam_history_q13d_suicide", "q13k_full_sib_young1_suicide",
                                         "q13k_full_sib_young2_suicide", "q13k_full_sib_young3_suicide", "q13k_full_sib_young4_suicide", 
                                         "q13k_full_sib_young5_suicide",  "q13l_full_sib_old1_suicide", "q13l_full_sib_old2_suicide",
                                         "q13l_full_sib_old3_suicide", "q13l_full_sib_old4_suicide", "q13l_full_sib_old5_suicide")], na.rm=TRUE)
famhist_1$suicide <- ifelse(is.na(famhist_1$fam_history_q13d_suicide) & is.na(famhist_1$fam_history_q13a_suicide) & is.na(famhist_1$q13k_full_sib_young1_suicide) & is.na(famhist_1$q13k_full_sib_young2_suicide)
                           & is.na(famhist_1$q13k_full_sib_young3_suicide) & is.na(famhist_1$q13k_full_sib_young4_suicide) & is.na(famhist_1$q13k_full_sib_young5_suicide) & is.na(famhist_1$q13l_full_sib_old1_suicide)
                          & is.na(famhist_1$q13l_full_sib_old2_suicide) & is.na(famhist_1$q13l_full_sib_old3_suicide) & is.na(famhist_1$q13l_full_sib_old4_suicide) & is.na(famhist_1$q13l_full_sib_old5_suicide), NA, famhist_1$suicide)
famhist_1$suicide <- ifelse(famhist_1$fam_history_13_yes_no == "0", 0, famhist_1$suicide)


famhist <- famhist_1[ ,c("src_subject_id", "depression", "drugs", "alcohol","suicide", "nerves", "trouble", "vision", "mania")]
famhist$fam_mental_health <- rowSums(famhist[c("depression", "drugs", "alcohol", "suicide", "nerves", "trouble", "vision", "mania")], na.rm = TRUE)

famhist$fam_mental_health <- ifelse(is.na(famhist$depression) & is.na(famhist$drugs) & is.na(famhist$alcohol) & is.na(famhist$suicide)
                           & is.na(famhist$nerves) & is.na(famhist$trouble) & is.na(famhist$vision) & is.na(famhist$mania), NA, famhist$fam_mental_health)
                                                                                                                                                                                                                                                  

famhist <- famhist[ ,c("src_subject_id", "fam_mental_health")]
famhist <- unique(famhist, by="src_subject_id")

## ### CULTURE
# Parent Multi-group ethnic identity revised survey
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/culture-environment/")
meim <- read.csv("ce_p_meim.csv", header=TRUE)
meim <- meim[which(meim$eventname == "baseline_year_1_arm_1"), ]
meim[meim == 999] <- NA
meim[, 3:25] <- apply(meim[, 3:25], 2, function(x) as.numeric(x))

meim$ethnic_identity <- rowSums(meim[c("meim_1f_p", "meim_2f_p", "meim_3f_p", "meim_4f_p", "meim_5f_p", "meim_6f_p")])/6
meim <- meim[, c("src_subject_id", "ethnic_identity")]

# Parent Vancouver Index of Acculturation - Short Survey
via <- read.csv("ce_p_via.csv", header=TRUE)
via <- via[which(via$eventname == "baseline_year_1_arm_1"), ]

via[via == 999] <- NA
via[, 3:26] <- apply(via[, 3:26], 2, function(x) as.numeric(x))

via$heritage_culture <- rowSums(via[c("vancouver_q2_p", "vancouver_q4_p", "vancouver_q6_p", "vancouver_q8_p", "vancouver_q10_p", "vancouver_q12_p", "vancouver_q14_p", "vancouver_q16_p")])/8
via$mainstream_culture <- rowSums(via[c("vancouver_q3_p", "vancouver_q5_p", "vancouver_q7_p", "vancouver_q9_p", "vancouver_q11_p", "vancouver_q13_p", "vancouver_q15_p", "vancouver_q17_p")])/8
via <- via[, c("src_subject_id", "heritage_culture", "mainstream_culture")]

## MACV - Parent Mexican American Cultural Values Scale Modified
macv <- read.csv("ce_p_macv.csv", header=TRUE)
macv <- macv[which(macv$eventname == "baseline_year_1_arm_1"), ]

macv[macv == 999] <- NA
macv[, 3:46] <- apply(macv[, 3:46], 2, function(x) as.numeric(x))

macv$family_support <- rowSums(macv[c("mex_american2_p", "mex_american7_p", "mex_american12_p", "mex_american21_p", "mex_american16_p", "mex_american26_p")])/6
macv$family_obligation <- rowSums(macv[c("mex_american3_p", "mex_american8_p", "mex_american13_p", "mex_american22_p", "mex_american17_p")])/5
macv$family_independence <- rowSums(macv[c("mex_american5_p", "mex_american10_p", "mex_american14_p", "mex_american19_p", "mex_american24_p")])/5
macv$family_referent <- rowSums(macv[c("mex_american4_p", "mex_american9_p", "mex_american18_p", "mex_american23_p", "mex_american27_p")])/5

macv <- macv[ ,c("src_subject_id", "family_support", "family_obligation", "family_independence", "family_referent")]

## life events - child report from a summary
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/mental-health/")

life <- read.csv("mh_y_le.csv", header=TRUE)
life <- life[which(life$eventname == "1_year_follow_up_y_arm_1"), ]

# weighted by how much the child was affected
life$ple_y_ss_affected_good_sum <- as.numeric(life$ple_y_ss_affected_good_sum)
life$ple_y_ss_affected_bad_sum <- as.numeric(life$ple_y_ss_affected_bad_sum)

life$good_events <- life$ple_y_ss_affected_good_sum
life$bad_events <- life$ple_y_ss_affected_bad_sum

life <- life[ , c("src_subject_id", "good_events", "bad_events")]

## number of friends from the Other Resilience survey

friends <- read.csv("mh_y_or.csv", header=TRUE)
friends <- friends[which(friends$eventname == "baseline_year_1_arm_1"), ]

friends$resiliency6b_y <- as.numeric(friends$resiliency6b_y)
friends$resiliency5b_y <- as.numeric(friends$resiliency5b_y)

friends$num_friends <- rowSums(friends[ , c("resiliency5b_y", "resiliency6b_y")])

friends <- friends[ , c("src_subject_id", "num_friends")]

# drug availability from the Parent community risk and protective factors - not available from the youth
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/substance-use/")
comm_risk <- read.csv("su_p_crpf.csv", header=TRUE)
comm_risk <- comm_risk[which(comm_risk$eventname == "baseline_year_1_arm_1"), ]
comm_risk[comm_risk == 4] <- NA
comm_risk[, 3:16] <- apply(comm_risk[, 3:16], 2, function(x) as.numeric(x))

comm_risk$sub_availability <- rowSums(comm_risk[ ,c("su_risk_p_1", "su_risk_p_2", "su_risk_p_3", "su_risk_p_4", "su_risk_p_5")], na.rm = TRUE)
comm_risk$sub_availability <- ifelse(is.na(comm_risk$su_risk_p_1) & is.na(comm_risk$su_risk_p_2) & is.na(comm_risk$su_risk_p_3) & is.na(comm_risk$su_risk_p_4) & is.na(comm_risk$su_risk_p_5), NA, comm_risk$sub_availability)

comm_risk <- comm_risk[ , c("src_subject_id", "sub_availability")]

### discrimination survey
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/culture-environment/")
discrim <- read.csv("ce_y_dm.csv", header=TRUE)
discrim <- discrim[which(discrim$eventname == "1_year_follow_up_y_arm_1"), ]
discrim[discrim == 999] <- NA
discrim[discrim == 777] <- NA
discrim[, 3:16] <- apply(discrim[, 3:16], 2, function(x) as.numeric(x))
discrim$na_count <- apply(discrim[, 3:16], 1, function(x) sum(!is.na(x))) # counting how many aren't NA to get mean
table(discrim$na_count)

discrim$discrim <- rowSums(discrim[ ,c("dim_matrix_q1", "dim_matrix_q2", "dim_matrix_q3", "dim_matrix_q4", "dim_matrix_q5", "dim_matrix_q6", "dim_matrix_q7")], na.rm = TRUE)/discrim$na_count
discrim$discrim <- ifelse(is.na(discrim$dim_matrix_q1) & is.na(discrim$dim_matrix_q2) & is.na(discrim$dim_matrix_q3) & is.na(discrim$dim_matrix_q4) & is.na(discrim$dim_matrix_q5) & is.na(discrim$dim_matrix_q6) & is.na(discrim$dim_matrix_q7), NA, discrim$discrim)

discrim <- discrim[ , c("src_subject_id", "discrim")]

# social development questionnaires all only have data for about 1000 children (abcd_socdev)


### PUTTING MEASURES TOGETHER
#### chiild vs parent report for conflict
environment <- merge(demo, safety, by="src_subject_id", all=TRUE)
environment <- merge(environment, resid_summary,by="src_subject_id", all=TRUE)
environment <- merge(environment, school,by="src_subject_id", all=TRUE)
environment <- merge(environment, parents,by="src_subject_id", all=TRUE)
environment <- merge(environment, conflict,by="src_subject_id", all=TRUE)
environment <- merge(environment, trauma,by="src_subject_id", all=TRUE)
environment <- merge(environment, monitoring,by="src_subject_id", all=TRUE)
environment <- merge(environment, develop,by="src_subject_id", all=TRUE)
environment <- merge(environment, tbi,by="src_subject_id", all=TRUE)
environment <- merge(environment, famhist,by="src_subject_id", all=TRUE)
environment <- merge(environment, via,by="src_subject_id", all=TRUE)
environment <- merge(environment, meim,by="src_subject_id", all=TRUE)
environment <- merge(environment, macv,by="src_subject_id", all=TRUE)
environment <- merge(environment, life,by="src_subject_id", all=TRUE)
environment <- merge(environment, friends,by="src_subject_id", all=TRUE)
environment <- merge(environment, comm_risk,by="src_subject_id", all=TRUE)
environment <- merge(environment, discrim,by="src_subject_id", all=TRUE)

monitoring <- NULL
resid <- NULL
demo <- NULL
safety <- NULL
school <- NULL
parents <- NULL
conflict <- NULL
trauma <- NULL
macv <- NULL
meim <- NULL
via <- NULL
tbi <- NULL
famhist <- NULL
develop <- NULL
life <- NULL
friends <- NULL
comm_risk <- NULL
discrim <- NULL

write.csv(environment , "/projects/jgallucci/abcd/environment.csv")


