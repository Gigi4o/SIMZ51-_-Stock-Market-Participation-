library(haven)
library(tidyverse)  
#___________________________________________________________________________________
#__________________ Load needed DFs form Survey Data _______________________

a_indresp <- read_sav("a_indresp.sav")

table(h_indresp$h_oprlg3)
table(a_indresp$a_ukborn)

#___________________________________________________________________________________
#__________________ Loading HH Variables _______________________

m_hhresp1 <- read_sav("m_hhresp.sav") %>% 
  select(m_hidp, m_cduse13, m_hhpc1, m_hhpc2, m_hhpc3, m_hhpc4, m_hhpc5, m_pcnet,
                     m_nvestrt1, m_nvestrt2, m_nvestrt3, m_nvestrt97, m_nvestrt96, m_fihhmnnet1_dv)

h_hhresp1 <- read_sav("h_hhresp.sav") %>%
  select(h_hidp, h_cduse13, h_hhpc1, h_hhpc2, h_hhpc3, h_hhpc4, h_hhpc5, h_pcnet,
                   h_nvestrt1, h_nvestrt2, h_nvestrt3, h_nvestrt97, h_nvestrt96, h_fihhmnnet1_dv)

d_hhresp1 <-read_sav("d_hhresp.sav") %>%
  select(d_hidp, d_cduse13, d_cduse10, d_pcnet,
                   d_nvestrt1, d_nvestrt2, d_nvestrt3, d_nvestrt97, d_nvestrt96, d_fihhmnnet1_dv)


#___________________________________________________________________________________
#__________________ Individual Variables: Loading and Re-coded _______________________

m_indresp1 <- read_sav("m_indresp.sav") %>%
  select(pidp, m_hidp, m_sex, m_dvage, m_jbstat,
                     m_health, m_mlstat, m_smlook, m_smpost, m_browse, m_email, m_onlinebuy, m_onlinebank, m_hiqual_dv)

h_indresp1 <-read_sav("h_indresp.sav") %>%
  select(pidp, h_hidp, h_sex, h_dvage, h_jbstat,
                    h_health, h_mlstat, h_oprlg3, h_hiqual_dv)

d_indresp1 <-read_sav("d_indresp.sav") %>%
  select(pidp, d_hidp, d_sex, d_dvage, d_jbstat, 
                    d_health, d_mlstat, d_oprlg3, d_hiqual_dv)

#___________________________________________________________________________________
#____________ Individual Variables: Loading and Re-coded - FROM ALT WAVES __________


c_indresp1 <- read_sav("c_indresp.sav") %>%
  select(pidp, c_hidp, c_pno, c_org, c_scopngbhh, c_scopngbha, c_scopngbhc, c_nbrcoh3, c_sex, 
         c_dvage, c_jbstat, c_health)

f_indresp1 <- read_sav("f_indresp.sav") %>%
  select(pidp, f_hidp, f_pno, f_org, f_scopngbhh, f_scopngbha, f_scopngbhc, f_nbrcoh3, f_sex, 
         f_dvage, f_jbstat, f_health)

i_indresp1 <- read_sav("i_indresp.sav") %>%
  select(pidp, i_hidp, i_pno, i_org, i_scopngbhh, i_scopngbha, i_scopngbhc, i_sex, 
         i_dvage, i_jbstat, i_health)

l_indresp1 <- read_sav("l_indresp.sav") %>%
  select(pidp, l_hidp, l_pno, l_org, l_scopngbhh, l_scopngbha, l_scopngbhc, l_sex, 
         l_dvage, l_jbstat, l_health, l_oprlg3)

l_hhresp1 <- read_sav("l_hhresp.sav") %>%
  select(l_hidp, l_nbrcoh3)


# quick Rework for "l_indresp1"
l_indresp1 <- full_join(l_indresp1, l_hhresp1, by = "l_hidp")



# Creating Joint Dfs 
# first merging same waves with the hidp - household indetifier

df_m <- full_join(m_indresp1, m_hhresp1, by = "m_hidp")
df_h <- full_join(h_indresp1, h_hhresp1, by = "h_hidp")
df_d <- full_join(d_indresp1, d_hhresp1, by = "d_hidp")

df_m <- df_m[!is.na(df_m$pidp), ]
df_h <- df_h[!is.na(df_h$pidp), ]
df_d <- df_d[!is.na(df_d$pidp), ]

colSums(is.na(df_m))

#join alt waves 
df_d <- full_join(df_d, c_indresp1, by = "pidp")
df_h <- full_join(df_h, f_indresp1, by = "pidp")
df_m <- full_join(df_m, l_indresp1, by = "pidp")



##### _________ Re-code each DF ____________
# Recoding all negtavie values as NAs 
df_m[df_m < 0] <- NA
df_h[df_h < 0] <- NA
df_d[df_d < 0] <- NA




# ______ Outcome Variable __________
# ______ creating SMP1 and SMP2 ________

m_invest_columns <- c("m_nvestrt1", "m_nvestrt2", "m_nvestrt3", "m_nvestrt97")

df_m$SMP_total <- ifelse(rowSums(df_m[m_invest_columns] == 1) > 0, 1, 0)
df_m$SMP_direct <- ifelse(df_m$m_nvestrt3 == 1, 1, 0)


h_invest_columns <- c("h_nvestrt1", "h_nvestrt2", "h_nvestrt3", "h_nvestrt97")

df_h$SMP_total <- ifelse(rowSums(df_h[h_invest_columns] == 1) > 0, 1, 0)
df_h$SMP_direct <- ifelse(df_h$h_nvestrt3 == 1, 1, 0)


d_invest_columns <- c("d_nvestrt1", "d_nvestrt2", "d_nvestrt3", "d_nvestrt97")

df_d$SMP_total <- ifelse(rowSums(df_d[d_invest_columns] == 1) > 0, 1, 0)
df_d$SMP_direct <- ifelse(df_d$d_nvestrt3 == 1, 1, 0)


table(df_h$SMP_direct, useNA = "ifany")

# ______ Predictor Variable __________
# ______ creating talks_with_neighbors ________

df_m$talks_with_neighbors <- ifelse(df_m$l_scopngbhh %in% c(1, 2), 1,
                                     ifelse(df_m$l_scopngbhh %in% c(3, 4, 5), 0, NA))

df_h$talks_with_neighbors <- ifelse(df_h$f_scopngbhh %in% c(1, 2), 1,
                                    ifelse(df_h$f_scopngbhh %in% c(3, 4, 5), 0, NA))

df_d$talks_with_neighbors <- ifelse(df_d$c_scopngbhh %in% c(1, 2), 1,
                                    ifelse(df_d$c_scopngbhh %in% c(3, 4, 5), 0, NA))


# ______ creating org_activity ________
# 
c_org <-  read_sav("c_indresp.sav") %>%
  select(pidp, c_orga1, c_orga1, c_orga2, c_orga3, c_orga4, c_orga5, c_orga6, c_orga7, c_orga8, c_orga9, c_orga10, c_orga11,
         c_orga12, c_orga13, c_orga14, c_orga15, c_orga16, c_orgat1, c_orgat2, c_orgat3, c_orgat4, c_orgat5, c_orgat6, 
         c_orgat7, c_orgat8, c_orgat9, c_orgat10, c_orgat11, c_orgat12, c_orgat13, c_orgat14, c_orgat15, c_orgat16)

f_org <-  read_sav("f_indresp.sav") %>%
  select(pidp, f_orga1, f_orga1, f_orga2, f_orga3, f_orga4, f_orga5, f_orga6, f_orga7, f_orga8, f_orga9, f_orga10, f_orga11,
         f_orga12, f_orga13, f_orga14, f_orga15, f_orga16, f_orgat1, f_orgat2, f_orgat3, f_orgat4, f_orgat5, f_orgat6, 
         f_orgat7, f_orgat8, f_orgat9, f_orgat10, f_orgat11, f_orgat12, f_orgat13, f_orgat14, f_orgat15, f_orgat16)

l_org <-  read_sav("l_indresp.sav") %>%
  select(pidp, l_orga1, l_orga1, l_orga2, l_orga3, l_orga4, l_orga5, l_orga6, l_orga7, l_orga8, l_orga9, l_orga10, l_orga11,
         l_orga12, l_orga13, l_orga14, l_orga15, l_orga16, l_orgat1, l_orgat2, l_orgat3, l_orgat4, l_orgat5, l_orgat6, 
         l_orgat7, l_orgat8, l_orgat9, l_orgat10, l_orgat11, l_orgat12, l_orgat13, l_orgat14, l_orgat15, l_orgat16)

# WAVE 13
# Sum the number of 1s in l_orga1 to l_orga16
l_org$total_ones <- rowSums(l_org[, paste0("l_orga", 1:16)] == 1, na.rm = TRUE)

# Create org_activity variable based on the total number of 1s
l_org$org_activity_1 <- ifelse(l_org$total_ones >= 1 & l_org$total_ones <= 3, 1,
                             ifelse(l_org$total_ones > 3, 2, 0))

# Sum the number of 1s in l_orgat1 to l_orgat16
l_org$total_ones1 <- rowSums(l_org[, paste0("l_orgat", 1:16)] == 1, na.rm = TRUE)

# Create org_activity variable based on the total number of 1s
l_org$org_activity_t <- ifelse(l_org$total_ones1 >= 1 & l_org$total_ones1 <= 3, 1,
                             ifelse(l_org$total_ones1 > 3, 2, 0))

# Create org_activity variable based on org_activity_1 and org_activity_2
l_org$org_activity <- ifelse(l_org$org_activity_1 == 1 | l_org$org_activity_t == 1, 1,
                             ifelse(l_org$org_activity_1 == 2 | l_org$org_activity_t == 2, 2, 0))

l_org_c <- l_org [, c("org_activity", "pidp")]
df_m <- full_join(df_m, l_org_c, by = "pidp")


# WAVE 8
# Sum the number of 1s in l_orga1 to l_orga16
f_org$total_ones <- rowSums(f_org[, paste0("f_orga", 1:16)] == 1, na.rm = TRUE)

# Create org_activity variable based on the total number of 1s
f_org$org_activity_1 <- ifelse(f_org$total_ones >= 1 & f_org$total_ones <= 3, 1,
                               ifelse(f_org$total_ones > 3, 2, 0))

# Sum the number of 1s in l_orgat1 to l_orgat16
f_org$total_ones1 <- rowSums(f_org[, paste0("f_orgat", 1:16)] == 1, na.rm = TRUE)

# Create org_activity variable based on the total number of 1s
f_org$org_activity_t <- ifelse(f_org$total_ones1 >= 1 & f_org$total_ones1 <= 3, 1,
                               ifelse(f_org$total_ones1 > 3, 2, 0))

# Create org_activity variable based on org_activity_1 and org_activity_2
f_org$org_activity <- ifelse(f_org$org_activity_1 == 1 | f_org$org_activity_t == 1, 1,
                             ifelse(f_org$org_activity_1 == 2 | f_org$org_activity_t == 2, 2, 0))

f_org_c <- f_org [, c("org_activity", "pidp")]
df_h <- full_join(df_h, f_org_c, by = "pidp")


# WAVE 4
# Sum the number of 1s in c_orga1 to c_orga16
c_org$total_ones <- rowSums(c_org[, paste0("c_orga", 1:16)] == 1, na.rm = TRUE)

# Create org_activity variable based on the total number of 1s
c_org$org_activity_1 <- ifelse(c_org$total_ones >= 1 & c_org$total_ones <= 3, 1,
                               ifelse(c_org$total_ones > 3, 2, 0))

# Sum the number of 1s in c_orgat1 to c_orgat16
c_org$total_ones1 <- rowSums(c_org[, paste0("c_orgat", 1:16)] == 1, na.rm = TRUE)

# Create org_activity variable based on the total number of 1s
c_org$org_activity_t <- ifelse(c_org$total_ones1 >= 1 & c_org$total_ones1 <= 3, 1,
                               ifelse(c_org$total_ones1 > 3, 2, 0))

# Create org_activity variable based on org_activity_1 and org_activity_2
c_org$org_activity <- ifelse(c_org$org_activity_1 == 1 | c_org$org_activity_t == 1, 1,
                             ifelse(c_org$org_activity_1 == 2 | c_org$org_activity_t == 2, 2, 0))

c_org_c <- c_org [, c("org_activity", "pidp")]
df_d <- full_join(df_d, c_org_c, by = "pidp")



# ___ CONTROLS FOR PREDICTORS ___________
# ______ creating local_advice ________

df_m$local_advice <- ifelse(df_m$l_scopngbhc %in% c(1, 2), 1,
                                    ifelse(df_m$l_scopngbhc %in% c(3, 4, 5), 0, NA))

df_h$local_advice <- ifelse(df_h$f_scopngbhc %in% c(1, 2), 1,
                                    ifelse(df_h$f_scopngbhc %in% c(3, 4, 5), 0, NA))

df_d$local_advice <- ifelse(df_d$c_scopngbhc %in% c(1, 2), 1,
                                    ifelse(df_d$c_scopngbhc %in% c(3, 4, 5), 0, NA))


# ______ creating belong_neighborhoob ________

df_m$belong_neighborhoob <- ifelse(df_m$l_scopngbha %in% c(1, 2), 1,
                            ifelse(df_m$l_scopngbha %in% c(3, 4, 5), 0, NA))

df_h$belong_neighborhoob <- ifelse(df_h$f_scopngbha %in% c(1, 2), 1,
                            ifelse(df_h$f_scopngbha %in% c(3, 4, 5), 0, NA))

df_d$belong_neighborhoob <- ifelse(df_d$c_scopngbha %in% c(1, 2), 1,
                            ifelse(df_d$c_scopngbha %in% c(3, 4, 5), 0, NA))


# ______ creating trust_neighborhood ________

df_m$trust_neighborhood <- ifelse(df_m$l_nbrcoh3 %in% c(1, 2), 1,
                                   ifelse(df_m$l_nbrcoh3 %in% c(3, 4, 5), 0, NA))

df_h$trust_neighborhood <- ifelse(df_h$f_nbrcoh3 %in% c(1, 2), 1,
                                   ifelse(df_h$f_nbrcoh3 %in% c(3, 4, 5), 0, NA))

df_d$trust_neighborhood <- ifelse(df_d$c_nbrcoh3 %in% c(1, 2), 1,
                                   ifelse(df_d$c_nbrcoh3 %in% c(3, 4, 5), 0, NA))



# ______ creating religiousness  ________

df_m$religiousness <- ifelse(df_m$l_oprlg3 %in% c(1, 2), 1,
                                  ifelse(df_m$l_oprlg3 %in% c(3, 4), 0, NA))

df_h$religiousness <- ifelse(df_h$h_oprlg3 %in% c(1, 2), 1,
                                  ifelse(df_h$h_oprlg3 %in% c(3, 4), 0, NA))

df_d$religiousness <- ifelse(df_d$d_oprlg3 %in% c(1, 2), 1,
                                  ifelse(df_d$d_oprlg3 %in% c(3, 4), 0, NA))




# ______ Predictor Variable - Soc-Media  __________
# ______ creating active_soc_media ________
df_m$active_soc_media <- ifelse(df_m$m_smlook %in% c(1, 2), 1,
                             ifelse(df_m$m_smlook %in% c(3, 4, 5, 6), 0, NA))


# ______ Controls for Soc-Media  __________
# ______ creating post_soc_media ________
df_m$post_soc_media <- ifelse(df_m$m_smpost %in% c(1, 2), 1,
                                ifelse(df_m$m_smpost %in% c(3, 4, 5, 6), 0, NA))

# ______ creating Browse_internet ________
df_m$browse_internet <- ifelse(df_m$m_browse %in% c(1, 2), 1,
                              ifelse(df_m$m_browse %in% c(3, 4, 5, 6), 0, NA))


# ______ creating use_email ________
df_m$use_email <- ifelse(df_m$m_email %in% c(1, 2), 1,
                               ifelse(df_m$m_email %in% c(3, 4, 5, 6), 0, NA))

# ______ creating buy_online ________
df_m$buy_online <- ifelse(df_m$m_onlinebuy %in% c(1, 2), 1,
                         ifelse(df_m$m_onlinebuy %in% c(3, 4, 5, 6), 0, NA))

# ______ creating bank_online ________
df_m$bank_online <- ifelse(df_m$m_onlinebank %in% c(1, 2), 1,
                          ifelse(df_m$m_onlinebank %in% c(3, 4, 5, 6), 0, NA))


# ______ Additionalt Control Variables  __________
# _______ Creating has_internet _____________

df_m$has_internet <- ifelse(df_m$m_pcnet == 1, 1,
                            ifelse(df_m$m_pcnet == 2, 0, NA))
df_h$has_internet <- ifelse(df_h$h_pcnet == 1, 1,
                            ifelse(df_h$h_pcnet == 2, 0, NA))
df_d$has_internet <- ifelse(df_d$d_pcnet == 1, 1,
                            ifelse(df_d$d_pcnet == 2, 0, NA))


# _________ Creating has_pc _____________

df_m$has_pc <- ifelse(rowSums(df_m[, c("m_hhpc1", "m_hhpc2", "m_hhpc3", "m_hhpc4", "m_hhpc5")]) >= 1, 1, 0)

df_h$has_pc <- ifelse(rowSums(df_h[, c("h_hhpc1", "h_hhpc2", "h_hhpc3", "h_hhpc4", "h_hhpc5")]) >= 1, 1, 0)

df_d$has_pc <- ifelse(df_d$d_cduse10 == 1, 1, 0)


# _________ Creating has_mobile_phone _____________

df_m$has_mobile_phone <- ifelse(df_m$m_cduse13 == 1, 1, 0)

df_h$has_mobile_phone <- ifelse(df_h$h_cduse13 == 1, 1, 0)

df_d$has_mobile_phone <- ifelse(df_d$d_cduse13 == 1, 1, 0)


# _________ Creating total_hh_income _____________

# Calculate quintile boundaries at first and then based on the quantiles creat factoral variable
quintiles_m <- quantile(df_m$m_fihhmnnet1_dv, probs = seq(0, 1, 0.2), na.rm = TRUE)
df_m$total_hh_income <- cut(df_m$m_fihhmnnet1_dv, breaks = quintiles_m, labels = FALSE)

quintiles_h <- quantile(df_h$h_fihhmnnet1_dv, probs = seq(0, 1, 0.2), na.rm = TRUE)
df_h$total_hh_income <- cut(df_h$h_fihhmnnet1_dv, breaks = quintiles_h, labels = FALSE)

quintiles_d <- quantile(df_d$d_fihhmnnet1_dv, probs = seq(0, 1, 0.2), na.rm = TRUE)
df_d$total_hh_income <- cut(df_d$d_fihhmnnet1_dv, breaks = quintiles_d, labels = FALSE)

table((df_d$total_hh_income), useNA = "ifany")
colSums(is.na(df_m))

#___________ Re-code "age" into the groups ____________

df_m$age <- cut(df_m$m_dvage, breaks = c(16, 25, 35, 45, 55, 65, Inf),
    labels = c("16-25", "26-35", "36-45", "46-55", "56-65", "65+"),
    include.lowest = TRUE)

df_h$age <- cut(df_h$h_dvage, breaks = c(16, 25, 35, 45, 55, 65, Inf),
                labels = c("16-25", "26-35", "36-45", "46-55", "56-65", "65+"),
                include.lowest = TRUE)

df_d$age <- cut(df_d$d_dvage, breaks = c(16, 25, 35, 45, 55, 65, Inf),
                labels = c("16-25", "26-35", "36-45", "46-55", "56-65", "65+"),
                include.lowest = TRUE)

#___________ Re-code "sex" ____________
df_m$sex <- ifelse(df_m$m_sex == 1, 1, 0)
df_h$sex <- ifelse(df_h$h_sex == 1, 1, 0)
df_d$sex <- ifelse(df_d$d_sex == 1, 1, 0)


# ______ Create variable "employment" ____________

df_m <- df_m %>% mutate(employment = case_when(
  m_jbstat %in% c(3, 5, 6, 7, 8, 9, 97) ~ 0, # Unemployed, maternity leave, family care, full-time student, sick, disabled, government training scheme, or other
  m_jbstat == 4 ~ 1,  # Retired
  m_jbstat == 1 ~ 2,  # Self-employed
  m_jbstat == 2 ~ 3   # Employed
))

df_h <- df_h %>% mutate(employment = case_when(
  h_jbstat %in% c(3, 5, 6, 7, 8, 9, 97) ~ 0, # Unemployed, maternity leave, family care, full-time student, sick, disabled, government training scheme, or other
  h_jbstat == 4 ~ 1,  # Retired
  h_jbstat == 1 ~ 2,  # Self-employed
  h_jbstat == 2 ~ 3   # Employed
))

df_d <- df_d %>% mutate(employment = case_when(
  d_jbstat %in% c(3, 5, 6, 7, 8, 9, 97) ~ 0, # Unemployed, maternity leave, family care, full-time student, sick, disabled, government training scheme, or other
  d_jbstat == 4 ~ 1,  # Retired
  d_jbstat == 1 ~ 2,  # Self-employed
  d_jbstat == 2 ~ 3   # Employed
))

# ______ Create variable "health" ____________

df_m$health <- ifelse(df_m$m_health == 1, 1, 0)
df_h$health <- ifelse(df_h$h_health == 1, 1, 0)
df_d$health <- ifelse(df_d$d_health == 1, 1, 0)

# ______ Create variable "education" ____________
df_m$education <- ifelse(df_m$m_hiqual_dv == 9, 0,
                         ifelse(df_m$m_hiqual_dv == 5, 1,
                                ifelse(df_m$m_hiqual_dv == 4, 2,
                                       ifelse(df_m$m_hiqual_dv == 3, 3,
                                              ifelse(df_m$m_hiqual_dv == 2, 4,
                                                     ifelse(df_m$m_hiqual_dv == 1, 5, NA))))))
df_h$education <- ifelse(df_h$h_hiqual_dv == 9, 0,
                         ifelse(df_h$h_hiqual_dv== 5, 1,
                                ifelse(df_h$h_hiqual_dv == 4, 2,
                                       ifelse(df_h$h_hiqual_dv == 3, 3,
                                              ifelse(df_h$h_hiqual_dv == 2, 4,
                                                     ifelse(df_h$h_hiqual_dv == 1, 5, NA))))))
df_d$education <- ifelse(df_d$d_hiqual_dv == 9, 0,
                         ifelse(df_d$d_hiqual_dv == 5, 1,
                                ifelse(df_d$d_hiqual_dv == 4, 2,
                                       ifelse(df_d$d_hiqual_dv == 3, 3,
                                              ifelse(df_d$d_hiqual_dv == 2, 4,
                                                     ifelse(df_d$d_hiqual_dv == 1, 5, NA))))))




# Merging the data for further analysis 

df_m$wave <- "3"
df_h$wave <- "2"
df_d$wave <- "1"


#creating subsets dfs with only the set of variables which will be used for analysis 
m_sub <- df_m [, c("pidp" ,"SMP_total", "SMP_direct", "talks_with_neighbors", "org_activity", "local_advice", "belong_neighborhoob", 
                   "trust_neighborhood", "religiousness", "active_soc_media", "post_soc_media", "browse_internet",
                   "use_email", "buy_online", "bank_online", "has_internet", "has_pc", "has_mobile_phone", "total_hh_income",
                   "age", "sex", "employment", "health", "education", "wave")]

h_sub <- df_h [, c("pidp", "SMP_total", "SMP_direct", "talks_with_neighbors", "org_activity", "local_advice", "belong_neighborhoob", 
                   "trust_neighborhood", "religiousness", "has_internet", "has_pc", "has_mobile_phone", "total_hh_income",
                   "age", "sex", "employment", "health", "education", "wave")]

d_sub <- df_d [, c("pidp" ,"SMP_total", "SMP_direct", "talks_with_neighbors", "org_activity", "local_advice", "belong_neighborhoob", 
                   "trust_neighborhood", "religiousness", "has_internet", "has_pc", "has_mobile_phone", "total_hh_income",
                   "age", "sex", "employment", "health", "education", "wave")]

comb_df <- plyr::rbind.fill(m_sub, h_sub, d_sub)

table(comb_df$wave)


#Refactor the variables which are essential
comb_df$org_activity <- factor(comb_df$org_activity)
comb_df$total_hh_income <- factor(comb_df$total_hh_income)
comb_df$age <- factor(comb_df$age)
comb_df$employment <- factor(comb_df$employment)
comb_df$wave <- factor(comb_df$wave)
comb_df$education <- factor(comb_df$education)
comb_df$pidp <- factor(comb_df$pidp)


df_m$employment <- factor(df_m$employment)




# _______ Descriptive Table  ___________

library(psych)

describe(comb_df)


# _______ Correlation Matrix  ___________

numeric_data <- comb_df_num[, sapply(comb_df_num, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(comb_df_num, use = "pairwise.complete.obs")

# Calculate the significance of the correlations
cor_test <- function(x){
  n <- nrow(x)
  r <- cor(x, use = "complete.obs")
  stderr <- 1/sqrt(n - 3)
  tstat <- r / stderr
  pval <- 2 * pt(abs(tstat), df = n - 2, lower.tail = FALSE)
  list(r = r, p = pval)
}

# Apply the function
result <- cor_test(numeric_data)
r <- result$r
p <- result$p

# Create a matrix of significance levels
sig <- ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))


# Format the correlation matrix with significance levels
formatted_cor <- apply(r, 2, function(x) formatC(x, format = "f", digits = 2))
cor_matrix_sig <- matrix(paste0(formatted_cor, sig), ncol = ncol(r))

# Set the column and row names as in the original data
dimnames(cor_matrix_sig) <- list(names(numeric_data), names(numeric_data))

print(cor_matrix_sig)

# Optional: Install and load the 'corrplot' package for visual correlation matrix plotting
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Plotting the correlation matrix
png("correlation_matrix.png", width = 800, height = 600)
corrplot(r, type = "upper", order = "hclust",
         p.mat = p, sig.level = c(0.01, 0.05),
         insig = "blank", method = "color",
         addCoef.col = "black", # Add correlation coefficients
         tl.col = "black", tl.srt = 45) # Text label color and rotation
dev.off()

# Load necessary library
library(corrplot)

# Prepare the plotting device
png("correlation_matrix.png", width = 800, height = 600)

# Plotting the correlation matrix with the corrected approach
corrplot(r, type = "upper", order = "hclust",
         p.mat = p, sig.level = 0.05,
         insig = "blank", method = "color",
         addCoef.col = "black", # Add correlation coefficients
         tl.col = "black", tl.srt = 45) # Text label color and rotation

# Close the device
dev.off()

graphics.off()  # This closes all active graphics devices


###########################
#Saving data
write.csv(comb_df, "comb_df.csv")

# _______ Creating Fixed Effects Model  ___________

summary(model)
install.packages("alpaca")
library(alpaca)

fm_1 <- feglm(SMP_total ~ talks_with_neighbors + org_activity | pidp + wave, comb_df)

fm_2 <- feglm(SMP_total ~ talks_with_neighbors + org_activity + 
                religiousness  + health + employment + age + total_hh_income | pidp + wave, comb_df)

fm_3 <- feglm(SMP_total ~ talks_with_neighbors + org_activity + + local_advice + belong_neighborhoob + trust_neighborhood +
                religiousness  + health + employment + age + total_hh_income | pidp + wave, comb_df)


fm_4 <- feglm(SMP_direct ~ talks_with_neighbors + org_activity | pidp + wave, comb_df)

fm_5 <- feglm(SMP_direct ~ talks_with_neighbors + org_activity + 
                religiousness  + health + employment + age + total_hh_income | pidp + wave, comb_df)

fm_6 <- feglm(SMP_direct ~ talks_with_neighbors + org_activity + + local_advice + belong_neighborhoob + trust_neighborhood +
                religiousness  + health + employment + age + total_hh_income | pidp + wave, comb_df)


summary(fm_6, type="clustered", cluster=~pidp)

pscl::pR2(fm_4)

# _______ Simple Logistic Effects Model  ___________

comb_df_clean <- na.omit(comb_df)

nrow(comb_df)
nrow(comb_df_clean)


install.packages("lme4")


m_soc <- glm(SMP_total ~ talks_with_neighbors + org_activity + active_soc_media, 
             family = binomial(link = "logit"), 
             data = comb_df)

m_soc_1 <- glm(SMP_total ~ talks_with_neighbors + org_activity + active_soc_media +
                 religiousness + education + health + employment + sex + age + total_hh_income, 
            family = binomial(link = "logit"), 
            data = comb_df)

m_soc_2 <- glm(SMP_total ~ talks_with_neighbors + org_activity + active_soc_media + 
                 local_advice + belong_neighborhoob + trust_neighborhood + post_soc_media +
                 browse_internet + use_email + has_internet + has_pc + has_mobile_phone +
                 religiousness + education + health + employment + sex + age + total_hh_income, 
               family = binomial(link = "logit"), 
               data = comb_df)

m_soc_3 <- glm(SMP_total ~ talks_with_neighbors + org_activity + active_soc_media + 
                 local_advice + belong_neighborhoob + trust_neighborhood + post_soc_media +
                 browse_internet + use_email + has_internet + has_pc + has_mobile_phone +
                 religiousness + education + health + employment + sex + age + total_hh_income + 
                 talks_with_neighbors*active_soc_media + org_activity*active_soc_media, 
               family = binomial(link = "logit"), 
               data = comb_df)

summary(m_soc_3)

#Direct 

m_soc_4 <- glm(SMP_direct ~ talks_with_neighbors + org_activity + active_soc_media, 
             family = binomial(link = "logit"), 
             data = comb_df)

m_soc_5 <- glm(SMP_direct ~ talks_with_neighbors + org_activity + active_soc_media +
                 religiousness + education + health + employment + sex + age + total_hh_income, 
               family = binomial(link = "logit"), 
               data = comb_df)

m_soc_6 <- glm(SMP_direct ~ talks_with_neighbors + org_activity + active_soc_media + 
                 local_advice + belong_neighborhoob + trust_neighborhood + post_soc_media +
                 browse_internet + use_email + has_internet + has_pc + has_mobile_phone +
                 religiousness + education + health + employment + sex + age + total_hh_income, 
               family = binomial(link = "logit"), 
               data = comb_df)

m_soc_7 <- glm(SMP_direct ~ talks_with_neighbors + org_activity + active_soc_media + 
                 local_advice + belong_neighborhoob + trust_neighborhood + post_soc_media +
                 browse_internet + use_email + has_internet + has_pc + has_mobile_phone +
                 religiousness + education + health + employment + sex + age + total_hh_income + 
                 talks_with_neighbors*active_soc_media + org_activity*active_soc_media, 
               family = binomial(link = "logit"), 
               data = comb_df)

summary(m_soc_7)

# Exponentiate the coefficients of each model to get odds ratios
m_soc$coefficients <- exp(coef(m_soc))
m_soc_1$coefficients <- exp(coef(m_soc_1))
m_soc_2$coefficients <- exp(coef(m_soc_2))
m_soc_3$coefficients <- exp(coef(m_soc_3))


m_soc_4$coefficients <- exp(coef(m_soc_4))
m_soc_5$coefficients <- exp(coef(m_soc_5))
m_soc_6$coefficients <- exp(coef(m_soc_6))
m_soc_7$coefficients <- exp(coef(m_soc_7))

stargazer(m_soc_3, type = 'text')
summary(m_soc_2)
# Generate a table with the odds ratios instead of log odds
stargazer(m_soc, m_soc_1, m_soc_2, m_soc_4, m_soc_5, m_soc_6, type = 'html', 
          title = "Odds Ratios from Logistic Regression Models",
          label = "table:odds_ratios",
          intercept.bottom = FALSE,  # Optionally hide intercept or display it
          covariate.labels = c(), # Customize variable labels
          out = "model_output1.html") # Optionally output to a text file

stargazer(m_soc, m_soc_1, m_soc_2, m_soc_4, m_soc_5, m_soc_6, type = 'text')

stargazer(m_soc_3, m_soc_7, type = 'html', 
          title = "",
          label = "table:odds_ratios",
          intercept.bottom = FALSE,  # Optionally hide intercept or display it
          covariate.labels = c(), # Customize variable labels
          out = "model_output2.html") # Optionally output to a text file



#Creating the sub sample of the observations for people who actively use social media

comb_df_soc <- comb_df[comb_df$active_soc_media == 1, ]

m_soc1 <- glm(SMP_total ~ talks_with_neighbors + org_activity + 
                 local_advice + belong_neighborhoob + trust_neighborhood + post_soc_media +
                 browse_internet + use_email + has_internet + has_pc + has_mobile_phone +
                 religiousness + education + health + employment + sex + age + total_hh_income, 
               family = binomial(link = "logit"), 
               data = comb_df_soc)

m_soc2 <- glm(SMP_direct ~ talks_with_neighbors + org_activity + 
                local_advice + belong_neighborhoob + trust_neighborhood + post_soc_media +
                browse_internet + use_email + has_internet + has_pc + has_mobile_phone +
                religiousness + education + health + employment + sex + age + total_hh_income, 
              family = binomial(link = "logit"), 
              data = comb_df_soc)

summary(m_soc2)

m_soc1$coefficients <- exp(coef(m_soc1))
m_soc2$coefficients <- exp(coef(m_soc2))

stargazer(m_soc1, m_soc2, type = 'html', 
          title = "",
          label = "table:odds_ratios",
          intercept.bottom = FALSE,  # Optionally hide intercept or display it
          covariate.labels = c(), # Customize variable labels
          out = "model_output3.html") # Optionally output to a text file

