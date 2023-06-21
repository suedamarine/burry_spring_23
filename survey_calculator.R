library(rcompanion)
library(tidyverse)
library(lubridate)

## import file - in future create new data.frame from CSV, using vectors to import count & mass into individual columns

temp_cockle <- read.csv("data/temp_cockle.csv")

# define 250grid stations that overlap 100grid
north_stations <- c(187, 188, 189, 207, 208, 209, 227, 228, 229)
south_stations <- c(391, 392, 393, 395, 396, 400, 401, 402, 478)

# number of stations visited in each grid
sampled_250 <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Grid =="250", na.rm = TRUE)
sampled_100n <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Block =="ZN", na.rm = TRUE)
sampled_100s <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Block =="ZS", na.rm = TRUE)
sampled_250_n <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Stn %in% north_stations, na.rm = TRUE)
sampled_250_s <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Stn %in% south_stations, na.rm = TRUE)

# number of samples not accessible and not saltmarsh
not_sampled <- sum(temp_cockle$Sampled == "N" & temp_cockle$Grid =="250" & temp_cockle$Substrata != "saltmarsh")

saltmarsh <- sum(temp_cockle$Substrata == "saltmarsh" & temp_cockle$Grid =="250", na.rm = TRUE)

channel <- sum(temp_cockle$Substrata == "channel" & temp_cockle$Grid =="250", na.rm = TRUE)

# start date
survey_dates <- temp_cockle %>%
  mutate(Date = dmy(Date)) %>%
  summarise(start = min(Date),
            end = max(Date))

# import and tidy count data
cockle_counts <- temp_cockle %>%
  select(Stn, Block, Grid, Sampled, Y0Count, Y1Count, Y2Count, Y3Count) %>% 
  mutate(total_count = Y0Count + Y1Count + Y2Count + Y3Count) %>% 
  pivot_longer(c('Y0Count', 'Y1Count', 'Y2Count', 'Y3Count', 'total_count'), names_to = "year_class", values_to = "count")

# summary table count
summary_counts <- cockle_counts %>% 
  filter(Grid %in% c ("250", "100") & !Block == "add") %>%
  mutate(count_sum = count*10*Grid^2) %>%
  group_by(year_class, Grid) %>%
  summarize(count_totals = sum(count_sum, na.rm = TRUE))

# write file to csv
write.csv(summary_counts, "tabs/summary_counts.csv")

# import and tidy mass data
cockle_mass <- temp_cockle %>%
  select(Stn, Block, Grid, Sampled, Y0Weight, Y1Weight, Y2Weight, Y3Weight, fifteen) %>%
  mutate(total_weight = Y0Weight + Y1Weight + Y2Weight+ Y3Weight) %>%
  pivot_longer(c('Y0Weight', 'Y1Weight', 'Y2Weight', 'Y3Weight', 'total_weight', 'fifteen'), names_to = "year_class", values_to = "mass")

# check that twenty is not greater than total
cockle_totals <- temp_cockle %>%
  select(Stn, Block, Y0Weight, Y1Weight, Y2Weight, Y3Weight, fifteen) %>%
  mutate(total = Y0Weight + Y1Weight + Y2Weight+ Y3Weight) %>%
  filter(fifteen > total)

# summary table mass
summary_mass <- cockle_mass %>%
  filter(Grid %in% c("250", "100") & !Block == "add") %>%
  mutate(mass_sum = mass*10/1000*Grid^2) %>%
  group_by(year_class, Grid) %>%
  summarise(mass_totals = sum(mass_sum, na.rm = TRUE))

# write file to csv
write.csv(summary_mass, "tabs/summary_mass.csv")

# groupwise means for counts

# groupwise means for main survey grids, filtered to remove NAs - remove Y0 during spring

g_250 <- cockle_counts %>% 
  filter(year_class %in% c("Y1Count", "Y2Count", "Y3Count", "total_count") & !Block == "add") %>%
  filter(Grid %in% c("250", "100")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_g_250 <- groupwiseMean(count ~ year_class + Grid, data = g_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_250 <- count_conf_g_250 %>%  
  mutate(count_lower =  Bca.lower * 10 * Grid^2 * n) %>%
  mutate(count_upper = Bca.upper * 10 * Grid^2 * n) %>%
  mutate(total_mean = Mean * 10 * Grid^2*n)

# arrange for easier reading + filter required fields
count_intervals_250_filtered <- count_intervals_250 %>%
  select(-c(Mean, Boot.mean, Conf.level, Bca.lower, Bca.upper)) %>%
  arrange(Grid, match(year_class, c("Y0Count", "Y1Count", "Y2Count", "Y3Count", "total_count"))) %>%
  mutate(across(3:6, round, 0)) %>%
  select(c(year_class, Grid, n, total_mean, count_lower, count_upper))

# write file to csv
write.csv(count_intervals_250_filtered, "tabs/count_intervals_250_filtered.csv")

# filter 100m grids and group by block
g_250_z <- cockle_counts %>% 
  filter(year_class %in% c("Y1Count", "Y2Count", "Y3Count", "total_count")) %>%
  filter(Block %in% c("ZN", "ZS")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_g_250_z <- groupwiseMean(count ~ year_class + Grid + Block, data = g_250_z, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_250_z <- count_conf_g_250_z %>%  
  mutate(count_lower =  Bca.lower * 10 * Grid^2 * n) %>%
  mutate(count_upper = Bca.upper * 10 * Grid^2 * n) %>%
  mutate(total_mean = Mean * 10 * Grid^2*n)

# arrange for easier reading + filter required fields
count_intervals_250z_filtered <- count_intervals_250_z %>%
  select(-c(Mean, Boot.mean, Conf.level, Bca.lower, Bca.upper)) %>%
  arrange(Grid, match(year_class, c("Y0Count", "Y1Count", "Y2Count", "Y3Count", "total_count"))) %>%
  mutate(across(4:6, round, 0)) %>%
  select(c(year_class, Block, Grid, n, total_mean, count_lower, count_upper))

count_intervals_250z_filtered

# write file to csv
write.csv(count_intervals_250z_filtered, "tabs/count_intervals_250z_filtered.csv")

# groupwise means for north counts, filtered to remove NAs
n_250_c <- cockle_counts %>%
  filter(Stn %in% north_stations) %>%
  filter(Sampled == "Y") %>%
  filter(year_class %in% c("Y1Count", "Y2Count", "Y3Count", "total_count")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_n_250 <- groupwiseMean(count ~ year_class + Grid, data = n_250_c, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_n250 <- count_conf_n_250 %>%  
  mutate(count_lower =  Bca.lower * 10 * Grid^2 * n) %>%
  mutate(count_upper = Bca.upper * 10 * Grid^2 * n) %>%
  mutate(total_mean = Mean * 10 * Grid^2*n) %>%
  arrange(Grid, match(year_class, c("Y0Count", "Y1Count", "Y2Count", "Y3Count", "total_count"))) %>%
  mutate(across(4:6, round, 0)) %>%
  select(c(year_class, Grid, n, total_mean, count_lower, count_upper))

# write file to csv
write.csv(count_intervals_n250, "tabs/count_intervals_n250.csv")

# groupwise means for south counts, filtered to remove NAs
s_250_c <- cockle_counts %>%
  filter(Stn %in% south_stations) %>%
  filter(Sampled == "Y") %>%
  filter(year_class %in% c("Y1Count", "Y2Count", "total_count")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_s_250 <- groupwiseMean(count ~ year_class + Grid, data = s_250_c, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_s250 <- count_conf_s_250 %>%  
  mutate(count_lower =  Bca.lower * 10 * Grid^2 * n) %>%
  mutate(count_upper = Bca.upper * 10 * Grid^2 * n) %>%
  mutate(total_mean = Mean * 10 * Grid^2*n) %>%
  arrange(Grid, match(year_class, c("Y0Count", "Y1Count", "Y2Count", "Y3Count", "total_count"))) %>%
  mutate(across(4:6, round, 0)) %>%
  select(c(year_class, Grid, n, total_mean, count_lower, count_upper))

# write file to csv
write.csv(count_intervals_s250, "tabs/count_intervals_s250.csv")

# groupwise means for mass

# groupwise means for main survey grids, filtered to remove NAs
g_250_m <- cockle_mass %>% 
  filter(year_class %in% c("Y1Weight", "Y2Weight", "Y3Weight", "total_weight", "fifteen") & !Block == "add") %>%
  filter(Grid %in% c("250", "100")) %>%
  filter(!is.na(mass))

# perform the groupwisemean selecting 10000 replicates and Bca
mass_conf_g_250 <- groupwiseMean(mass ~ year_class + Grid, data = g_250_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
mass_intervals_250 <- mass_conf_g_250 %>%  
  mutate(mass_lower =  Bca.lower * 0.01 * Grid^2 * n) %>%
  mutate(mass_upper = Bca.upper * 0.01 * Grid^2 * n) %>%
  mutate(total_mean = Mean * 0.01 * Grid^2 * n) %>%
  arrange(Grid, match(year_class, c("Y0Weight", "Y1Weight", "Y2Weight", "Y3Weight", "fifteen", "total_weight"))) %>%
  select(-c(Mean, Boot.mean, Conf.level, Bca.lower, Bca.upper)) %>%
  mutate(mass_lower = mass_lower/1000,
         mass_upper = mass_upper/1000,
         total_mean = total_mean/1000) %>%
  mutate(across(3:6, round, 0)) %>%
  select(c(year_class, Grid, n, total_mean, mass_lower, mass_upper))

  
  # write file to csv
  write.csv(mass_intervals_250, "tabs/mass_intervals_250.csv")

  # filter 100m grids and group by block
  g_250_zm <- cockle_mass %>% 
    filter(year_class %in% c("Y1Weight", "Y2Weight", "Y3Weight", "total_weight")) %>%
    filter(Block %in% c("ZN", "ZS")) %>%
    filter(!is.na(mass))

  # perform the groupwisemean selecting 10000 replicates and Bca
  mass_conf_g_250z <- groupwiseMean(mass ~ year_class + Block + Grid, data = g_250_zm, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

  # calculate confidence intervals
  mass_intervals_250z <- mass_conf_g_250z %>%  
    mutate(mass_lower =  Bca.lower * 0.01 * Grid^2 * n) %>%
    mutate(mass_upper = Bca.upper * 0.01 * Grid^2 * n) %>%
    mutate(total_mean = Mean * 0.01 * Grid^2 * n) %>%
    arrange(Block, match(year_class, c("Y0Weight", "Y1Weight", "Y2Weight", "Y3Weight", "twenty", "total_weight"))) %>%
    select(-c(Mean, Boot.mean, Conf.level, Bca.lower, Bca.upper)) %>%
    mutate(mass_lower = mass_lower/1000,
           mass_upper = mass_upper/1000,
           total_mean = total_mean/1000) %>%
    mutate(across(3:6, round, 0)) %>%
    select(c(year_class, Block, Grid, n, total_mean, mass_lower, mass_upper))
  
  # write file to csv
  write.csv(mass_intervals_250z, "tabs/mass_intervals_250z.csv")    

  # groupwise means for north mass, filtered to remove NAs
  n_250_m <- cockle_mass %>%
    filter(Stn %in% north_stations) %>%
    filter(Sampled == "Y") %>%
    filter(year_class %in% c("Y1Weight", "Y2Weight", "Y3Weight", "total_weight")) %>%
    filter(!is.na(mass))

  # perform the groupwisemean selecting 10000 replicates and Bca
  mass_conf_n_250 <- groupwiseMean(mass ~ year_class + Grid, data = n_250_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

  # calculate confidence intervals
  mass_intervals_n250 <- mass_conf_n_250 %>%  
    mutate(mass_lower =  Bca.lower * 0.01 * Grid^2 * n) %>%
    mutate(mass_upper = Bca.upper * 0.01 * Grid^2 * n) %>%
    mutate(total_mean = Mean * 0.01 * Grid^2 * n) %>%
    arrange(Grid, match(year_class, c("Y0Weight", "Y1Weight", "Y2Weight", "Y3Weight", "twenty", "total_weight")))%>%
    select(-c(Mean, Boot.mean, Conf.level, Bca.lower, Bca.upper)) %>%
    mutate(mass_lower = mass_lower/1000,
           mass_upper = mass_upper/1000,
           total_mean = total_mean/1000) %>%
    mutate(across(3:6, round, 0)) %>%
    select(c(year_class, Grid, n, total_mean, mass_lower, mass_upper))
  
  # write file to csv
  write.csv(mass_intervals_n250, "tabs/mass_intervals_n250.csv")
  
  # groupwise means for south mass, filtered to remove NAs
  s_250_m <- cockle_mass %>%
    filter(Stn %in% south_stations) %>%
    filter(Sampled == "Y") %>%
    filter(year_class %in% c("Y1Weight", "Y2Weight", "total_weight")) %>%
    filter(!is.na(mass))  

  # perform the groupwisemean selecting 10000 replicates and Bca
  mass_conf_s_250 <- groupwiseMean(mass ~ year_class + Grid, data = s_250_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

  # calculate confidence intervals
  mass_intervals_s250 <- mass_conf_s_250 %>%  
    mutate(mass_lower =  Bca.lower * 0.01 * Grid^2 * n) %>%
    mutate(mass_upper = Bca.upper * 0.01 * Grid^2 * n) %>%
    mutate(total_mean = Mean * 0.01 * Grid^2 * n) %>%
    arrange(Grid, match(year_class, c("Y0Weight", "Y1Weight", "Y2Weight", "Y3Weight", "twenty", "total_weight"))) %>%
    select(-c(Mean, Boot.mean, Conf.level, Bca.lower, Bca.upper)) %>%
    mutate(mass_lower = mass_lower/1000,
           mass_upper = mass_upper/1000,
           total_mean = total_mean/1000) %>%
    mutate(across(3:6, round, 0)) %>%
    select(c(year_class, Grid, n, total_mean, mass_lower, mass_upper))
    
  # write file to csv
  write.csv(mass_intervals_s250, "tabs/mass_intervals_s250.csv")
  
  # mass percentage change
  mass_23 <- c(11338125, 0, 6576875, 3935000, 826250)
  mass_22 <- c(9995000, 0, 5704375, 2933125, 1357500)
  mass_21 <- c(6755000, 0, 3735625, 1328750, 1690625)
  count_23 <- c(6.455E+09, 0, 5041250000, 1288125000, 125625000)
  count_22 <- c(6241250000, 0, 5265625000, 738750000, 236875000)
  count_21 <- c(5762500000, 0, 4.98E+09, 461875000, 320625000)
  
  mass_changes <- (mass_23 - mass_22) / mass_22 * 100
  count_changes <- (count_23 - count_22) / count_22 * 100
  
  # winter survival
  
  winter_survival <- tibble(cohort_22 = c(9722500000, 2.6E+09, 266250000 + 150625000),
                            cohort_23 = c(5041250000, 1288125000, 125625000)) %>%
    mutate(survival = (cohort_23/cohort_22) *100)
  
  # write file to csv
  write.csv(winter_survival, "tabs/winter_survival.csv")
  
  count_proportions <- c(as.double(summary_counts[6,3]), as.double(summary_counts[8,3]),
                         as.double(summary_counts[10,3])) / as.double(summary_counts[2,3])
  
  
  mass_proportions <- c(as.double(summary_mass[8,3]), as.double(summary_mass[10,3]),
                         as.double(summary_mass[12,3])) / as.double(summary_mass[4,3])
    
  