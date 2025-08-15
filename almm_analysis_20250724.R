# ALMM analysis for INFANCY 
# Ran Wei
# July & August 2025


# packages ----------------------------------------------------------------
library(psych)
library(dplyr)
library(pwr)
library(apaTables)
library(modelsummary)
library(pandoc)
library(interactions) 
library(patchwork)
library(emmeans)
library(naniar)

# importing data -----------------------------------------------------------
almmlabsurvey_20250815 <- read.csv("/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/almmlabsurvey_20250815.csv", header = TRUE)
# almmlabsurvey_20250815.csv = all behavioral and survey data, both cohorts
music_timesperweek <- read.csv("/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/music_timesperweek.csv", header = TRUE)
emq_labsurvey_allitems <- read.csv("/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/emq_labsurvey_allitems.csv", header = TRUE)


labwithmaap_20250726$X <- NULL
almmlabsurvey_20250815$X <- NULL
almmlabsurvey_20250815$...1 <- NULL
music_timesperweek$X <- NULL

# Table 1 descriptive stats: full sample, monolingual, and multilingual ------------------------------------------------------

almmmono <- almmlabsurvey_20250815[almmlabsurvey_20250815$bilingual == 0, ]
dim(almmmono)
almmmulti <- almmlabsurvey_20250815[almmlabsurvey_20250815$bilingual == 1, ]
dim(almmmulti)

# full sample
mean(almmlabsurvey$age)
sd(almmlabsurvey$age)

table(almmlabsurvey$female)
mean(almmlabsurvey$female)
table(almmlabsurvey$bilingual)
mean(almmlabsurvey$bilingual)
table(almmlabsurvey$music)
mean(almmlabsurvey$music)
32/106

summary(almmlabsurvey$education_year)
sd(almmlabsurvey$education_year)
table(almmlabsurvey$education_year)
3/106
22/106
81/106

summary(almmlabsurvey$music_infant_total_times)
sd(almmlabsurvey$music_infant_total_times)
summary(almmlabsurvey$music_current_total_times) 
sd(almmlabsurvey$music_current_total_times)
summary(almmlabsurvey$music_total_times)
mean(almmlabsurvey$music_total_times)
sd(almmlabsurvey$music_total_times)
summary(almmlabsurvey$non_music_total_times)
sd(almmlabsurvey$non_music_total_times)

summary(almmlabsurvey$cdi_raw)
sd(almmlabsurvey$cdi_raw)
summary(almmlabsurvey$lui_raw)
sd(almmlabsurvey$lui_raw, na.rm = TRUE)
summary(almmlabsurvey$emq_g)
sd(almmlabsurvey$emq_g, na.rm = TRUE)
summary(almmlabsurvey$emq_pa)
sd(almmlabsurvey$emq_pa, na.rm = TRUE)

# monolingual cohort
mean(almmmono$age)
sd(almmmono$age)
range(almmmono$age)

table(almmmono$female)
mean(almmmono$female)
table(almmmono$music)
mean(almmmono$music)

summary(almmmono$education_year)
sd(almmmono$education_year)
table(almmmono$education_year)
2/74
16/74
56/74

summary(almmmono$music_infant_total_times)
sd(almmmono$music_infant_total_times)
summary(almmmono$music_current_total_times) 
sd(almmmono$music_current_total_times)
summary(almmmono$music_total_times)
sd(almmmono$music_total_times)
summary(almmmono$non_music_total_times)
sd(almmmono$non_music_total_times)

summary(almmmono$cdi_raw)
sd(almmmono$cdi_raw)
summary(almmmono$lui_raw)
sd(almmmono$lui_raw, na.rm = TRUE)
summary(almmmono$emq_g)
sd(almmmono$emq_g, na.rm = TRUE)
summary(almmmono$emq_pa)
sd(almmmono$emq_pa, na.rm = TRUE)

# multilingual cohort
mean(almmmulti$age)
sd(almmmulti$age)
range(almmmulti$age)

table(almmmulti$female)
mean(almmmulti$female)

summary(almmmulti$education_year)
sd(almmmulti$education_year)
table(almmmulti$education_year)
1/32
6/32
25/32

table(almmmulti$music)
mean(almmmulti$music)
18/32

summary(almmmulti$music_infant_total_times)
sd(almmmulti$music_infant_total_times)
summary(almmmulti$music_current_total_times) 
sd(almmmulti$music_current_total_times)
summary(almmmulti$music_total_times)
mean(almmmulti$music_total_times)
sd(almmmulti$music_total_times)
summary(almmmulti$non_music_total_times)
mean(almmmulti$non_music_total_times)
sd(almmmulti$non_music_total_times)

summary(almmmulti$cdi_raw)
sd(almmmulti$cdi_raw)
summary(almmmulti$lui_raw)
sd(almmmulti$lui_raw, na.rm = TRUE)
summary(almmmulti$emq_g)
sd(almmmulti$emq_g, na.rm = TRUE)
summary(almmmulti$emq_pa)
sd(almmmulti$emq_pa, na.rm = TRUE)


# missing data -----------------------------------------------------------
sum(is.na(almmlabsurvey$music_infant_total))
sum(is.na(almmlabsurvey$music_toddler_total))
sum(is.na(almmlabsurvey$cdi_raw))
sum(is.na(almmlabsurvey$lui_raw)) # 5 missing 
sum(is.na(almmlabsurvey$emq_pa))
sum(is.na(almmlabsurvey$emq_g))


# t-tests and chi-square tests comparing monolingual vs. multilingual groups -------------------------------------------------

t.test(almmlabsurvey$age~almmlabsurvey$bilingual)
t.test(almmlabsurvey$education_year~almmlabsurvey$bilingual)
t.test(almmlabsurvey$cdi_raw~almmlabsurvey$bilingual) #***
t.test(almmlabsurvey$lui_raw~almmlabsurvey$bilingual) #*
t.test(almmlabsurvey$emq_g~almmlabsurvey$bilingual)
t.test(almmlabsurvey$emq_pa~almmlabsurvey$bilingual) #*

emq_pa_bilingual <- lm(emq_pa ~ age + education_year + bilingual, data = almmlabsurvey) 
summary(emq_pa_bilingual)

t.test(almmlabsurvey$music_total_times~almmlabsurvey$bilingual) 
t.test(almmlabsurvey$music_infant_total_times~almmlabsurvey$bilingual) 
t.test(almmlabsurvey$music_current_total_times~almmlabsurvey$bilingual) 
t.test(almmlabsurvey$non_music_total_times~almmlabsurvey$bilingual) #*

table(almmmono$female)
table(almmmulti$female)
mono_male_female <- c(38, 36)
multi_male_female <- c(11, 21)
conttable_sex_monomulti <- data.frame(mono_male_female, multi_male_female)
chisq.test(conttable_sex_monomulti) # ns

table(almmmono$music)
table(almmmulti$music)
mono_nomusicclass_musicclass <- c(53, 21)
multi_nomusicclass_musicclass <- c(21, 11)
conttable_musicclass_monomulti <- data.frame(mono_nomusicclass_musicclass, multi_nomusicclass_musicclass)
chisq.test(conttable_musicclass_monomulti) # ns


# calculating Cronbach's alpha of music survey --------------------------------------------

mus_columns <- names(almmlabsurvey_20250815)[
  grepl("mus", names(almmlabsurvey_20250815))]

mus_columns <- mus_columns[1:(length(mus_columns) - 7)]
mus_columns <- c("id", mus_columns)
music_raw <- almmlabsurvey_20250815[, mus_columns, drop = FALSE]
music_raw <- music_raw[, !grepl("_times$", names(music_raw))]

dim(music_raw)
dim(music_timesperweek)

music_raw_alpha <- music_raw
music_raw_alpha$id <- NULL
music_timesperweek_alpha <- music_timesperweek
music_timesperweek_alpha$id <- NULL

alpha_result_raw <- alpha(music_raw_alpha)
alpha_result_raw$total$raw_alpha
alpha_result_times <- alpha(music_timesperweek, check.keys = TRUE)
alpha_result_times$total$raw_alpha
# sensitivity check: similar alpha if calculated with raw score 


# Power analysis for interaction term ----------------------------------------------------------
# Example: small-to-moderate effect size f² = 0.05
# u = number of predictors being tested (e.g., 1 if you're testing just the interaction)
# f2 = effect size
# power = desired power level (e.g., 0.80)
# sig.level = alpha level (e.g., 0.05)

pwr.f2.test(u = 3,           # number of predictors being tested
            f2 = 0.1,       # Cohen's f² effect size
            sig.level = 0.05,
            power = 0.80)
3 + 109 + 1 #n = u + v + 1 = 113


# calculating Cronbach's alpha for EMQ ---------------------------------------------------

emq_labsurvey_allitems$id <- NULL
emq_labsurvey_allitems$X <- NULL
alpha_result_raw_emq <- alpha(emq_labsurvey_allitems)
alpha_result_raw_emq$total$raw_alpha

# RQ 1: descriptive stats of music survey - full sample -------------------

summary(almmlabsurvey$music_infant_total_times)
sd(almmlabsurvey$music_infant_total_times)
summary(almmlabsurvey$music_current_total_times) 
sd(almmlabsurvey$music_current_total_times)
summary(almmlabsurvey$non_music_total_times)
sd(almmlabsurvey$non_music_total_times)

# were music activities outside the home disrupted by COVID?
t.test(almmlab$mus4.1itookmybabytoconcertstohearlivemusic_times, almmsurvey$mus4.1itookmybabytoconcertstohearlivemusic_times)
t.test(almmlab$mus2.3attendmusicclasseslessons_times, almmsurvey$mus2.3attendmusicclasseslessons_times)


# RQ 1: correlates of music experience ------------------------------------

cor.test(almmlabsurvey$music_infant_total_times, almmlabsurvey$education_year)
cor.test(almmlabsurvey$music_current_total_times, almmlabsurvey$education_year)
cor.test(almmlabsurvey$music_total_times, almmlabsurvey$education_year)
cor.test(almmlabsurvey$non_music_total_times, almmlabsurvey$education_year)

cor.test(almmlabsurvey$music_current_total_times, almmlabsurvey$age)
cor.test(almmlabsurvey$music_total_times, almmlabsurvey$age)
cor.test(almmlabsurvey$non_music_total_times, almmlabsurvey$age)

t.test(almmlabsurvey$music_infant_total_times~almmlabsurvey$bilingual)
t.test(almmlabsurvey$music_current_total_times~almmlabsurvey$bilingual)
t.test(almmlabsurvey$music_total_times~almmlabsurvey$bilingual)
t.test(almmlabsurvey$non_music_total_times~almmlabsurvey$bilingual)


# RQ 2: monolingual versus DLL: developmental outcomes ---------------------------
t.test(almmlabsurvey$cdi_raw ~ almmlabsurvey$bilingual)
t.test(almmlabsurvey$lui_raw ~ almmlabsurvey$bilingual)
t.test(almmlabsurvey$emq_g~almmlabsurvey$bilingual)
t.test(almmlabsurvey$emq_pa~almmlabsurvey$bilingual)

cor.test(almmlabsurvey$child_exposure, almmlabsurvey$cdi_raw, method = "spearman")
cor.test(almmlabsurvey$child_exposure, almmlabsurvey$lui_raw, method = "spearman")
cor.test(almmlabsurvey$child_exposure, almmlabsurvey$emq_pa, method = "spearman")
cor.test(almmlabsurvey$child_exposure, almmlabsurvey$emq_g, method = "spearman")

t.test(almmlabsurvey$cdi_raw ~ almmlabsurvey$music)
t.test(almmlabsurvey$lui_raw ~ almmlabsurvey$music)
t.test(almmlabsurvey$emq_g~almmlabsurvey$music)
t.test(almmlabsurvey$emq_pa~almmlabsurvey$music)

t.test(almmlabsurvey$cdi_raw ~ almmlabsurvey$female)
t.test(almmlabsurvey$lui_raw ~ almmlabsurvey$female)
t.test(almmlabsurvey$emq_g~almmlabsurvey$female)
t.test(almmlabsurvey$emq_pa~almmlabsurvey$female)


# RQ2: correlation table --------------------------------------------------

vars_for_corr <- almmlabsurvey[, c("age", "education_year", "music_infant_total_times",
                                   "music_current_total_times", "music_total_times",
                                   "non_music_total_times", "cdi_raw", "lui_raw",
                                   "emq_g", "emq_pa")]  

apa.cor.table(vars_for_corr, table.number = 1)
apa.cor.table(vars_for_corr, filename = "/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/correlation_table.doc")


# RQ 2: linear regressions without interactions------------------------------------------------

cdi_infancy <- lm(cdi_raw ~ age + education_year + bilingual + music_infant_total_times, data = almmlabsurvey) 
cdi_concurrent <- lm(cdi_raw ~ age + education_year + bilingual + music_current_total_times, data = almmlabsurvey) 
cdi_cumulative <- lm(cdi_raw ~ age + education_year + bilingual + music_total_times, data = almmlabsurvey) 
cdi_nonmusic <- lm(cdi_raw ~ age + education_year + bilingual + non_music_total_times, data = almmlabsurvey) 
summary(cdi_nonmusic)

lui_infancy <- lm(lui_raw ~ age + education_year + bilingual + music_infant_total_times, data = almmlabsurvey) 
lui_concurrent <- lm(lui_raw ~ age + education_year + bilingual + music_current_total_times, data = almmlabsurvey) 
lui_cumulative <- lm(lui_raw ~ age + education_year + bilingual + music_total_times, data = almmlabsurvey) 
lui_nonmusic <- lm(lui_raw ~ age + education_year + bilingual + non_music_total_times, data = almmlabsurvey) 
summary(lui_nonmusic)

emq_g_infancy <- lm(emq_g ~ age + education_year + bilingual + music_infant_total_times, data = almmlabsurvey) 
emq_g_concurrent <- lm(emq_g ~ age + education_year + bilingual + music_current_total_times, data = almmlabsurvey) 
emq_g_cumulative <- lm(emq_g ~ age + education_year + bilingual + music_total_times, data = almmlabsurvey) 
emq_g_nonmusic <- lm(emq_g ~ age + education_year + bilingual + non_music_total_times, data = almmlabsurvey) 
summary(emq_g_nonmusic)

emq_pa_infancy <- lm(emq_pa ~ age + education_year + bilingual + music_infant_total_times, data = almmlabsurvey) 
emq_pa_concurrent <- lm(emq_pa ~ age + education_year + bilingual + music_current_total_times, data = almmlabsurvey) 
emq_pa_cumulative <- lm(emq_pa ~ age + education_year + bilingual + music_total_times, data = almmlabsurvey) 
emq_pa_nonmusic <- lm(emq_pa ~ age + education_year + bilingual + non_music_total_times, data = almmlabsurvey) 
summary(emq_pa_nonmusic)

stars <- c(`***` = .001, `**` = .01, `*` = .05, `~` = .1)
modelsummary(
  list("Model 1" = cdi_infancy, "Model 2" = cdi_concurrent, "Model 3" = cdi_cumulative,
       "Model 4" = lui_infancy, "Model 5" = lui_concurrent, "Model 6" = lui_cumulative,
       "Model 7" = emq_g_infancy, "Model 8" = emq_g_concurrent, "Model 9" = emq_g_cumulative,
       "Model 10" = emq_pa_infancy, "Model 11" = emq_pa_concurrent, "Model 12" = emq_pa_cumulative),
  statistic = "({std.error})",  # Show standard errors in parentheses
  stars = stars,
  fmt = 2,                      # Round all numeric values to 2 decimal places
  output = "/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/table4_regression.docx"
)

# RQ 3: linear regressions with interaction terms------------------------------------------------

cdi_infancy_int <- lm(cdi_raw ~ age + education_year + bilingual * music_infant_total_times, data = almmlabsurvey) 
cdi_concurrent_int <- lm(cdi_raw ~ age + education_year + bilingual * music_current_total_times, data = almmlabsurvey) 
cdi_cumulative_int <- lm(cdi_raw ~ age + education_year + bilingual * music_total_times, data = almmlabsurvey) 
summary(cdi_concurrent_int)

lui_infancy_int <- lm(lui_raw ~ age + education_year + bilingual * music_infant_total_times, data = almmlabsurvey) 
lui_concurrent_int <- lm(lui_raw ~ age + education_year + bilingual * music_current_total_times, data = almmlabsurvey) 
lui_cumulative_int <- lm(lui_raw ~ age + education_year + bilingual * music_total_times, data = almmlabsurvey) 

emq_g_infancy_int <- lm(emq_g ~ age + education_year + bilingual * music_infant_total_times, data = almmlabsurvey) 
emq_g_concurrent_int <- lm(emq_g ~ age + education_year + bilingual * music_current_total_times, data = almmlabsurvey) 
emq_g_cumulative_int <- lm(emq_g ~ age + education_year + bilingual * music_total_times, data = almmlabsurvey) 

emq_pa_infancy_int <- lm(emq_pa ~ age + education_year + bilingual * music_infant_total_times, data = almmlabsurvey) 
emq_pa_concurrent_int <- lm(emq_pa ~ age + education_year + bilingual * music_current_total_times, data = almmlabsurvey) 
emq_pa_cumulative_int <- lm(emq_pa ~ age + education_year + bilingual * music_total_times, data = almmlabsurvey) 

stars <- c(`***` = .001, `**` = .01, `*` = .05, `~` = .1)
modelsummary(
  list("Model 1" = cdi_infancy_int, "Model 2" = cdi_concurrent_int, "Model 3" = cdi_cumulative_int,
       "Model 4" = lui_infancy_int, "Model 5" = lui_concurrent_int, "Model 6" = lui_cumulative_int,
       "Model 7" = emq_g_infancy_int, "Model 8" = emq_g_concurrent_int, "Model 9" = emq_g_cumulative_int,
       "Model 10" = emq_pa_infancy_int, "Model 11" = emq_pa_concurrent_int, "Model 12" = emq_pa_cumulative_int),
  statistic = "({std.error})",  # Show standard errors in parentheses
  stars = stars,
  fmt = 2,                      # Round all numeric values to 2 decimal places
  output = "/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/table4_regression_int.docx"
)


# RQ 3: simple slopes analysis and visualization --------------------------

sim_slopes(cdi_infancy_int, pred = music_infant_total_times, modx = bilingual)
sim_slopes(cdi_cumulative_int, pred = music_total_times, modx = bilingual)

sim_slopes(lui_infancy_int, pred = music_infant_total_times, modx = bilingual)
sim_slopes(lui_concurrent_int, pred = music_current_total_times, modx = bilingual)
sim_slopes(lui_cumulative_int, pred = music_total_times, modx = bilingual)

# getting more precise estimates:
# estimate the marginal effect (simple slope) of music activity at each level of bilingual
emtrends(lui_infancy_int, var = "music_infant_total_times", specs = "bilingual")
emtrends(lui_cumulative_int, var = "music_total_times", specs = "bilingual")



# RQ 3: visualizing the interaction effects -------------------------------

p1 <- interact_plot(cdi_infancy_int,
              pred = music_infant_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual Toddlers", "Multilingual Toddlers"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Infancy Music Activity",
              y.label = "Vocabulary",
              line.thickness = 1.2)

p2 <- interact_plot(cdi_cumulative_int,
              pred = music_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual Toddlers", "Multilingual Toddlers"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Cumulative Music Activity",
              y.label = "Vocabulary",
              line.thickness = 1.2)

p3 <- interact_plot(lui_infancy_int,
              pred = music_infant_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual Toddlers", "Multilingual Toddlers"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Infancy Music Activity",
              y.label = "Pragmatic Language Skill",
              line.thickness = 1.2)

p4 <- interact_plot(lui_concurrent_int,
              pred = music_current_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual", "DLL"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Concurrent Music Activity",
              y.label = "Pragmatic Language Skill",
              line.thickness = 1.2)

p5 <- interact_plot(lui_cumulative_int,
              pred = music_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual", "DLL"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Cumulative Music Activity",
              y.label = "Pragmatic Language Skill",
              line.thickness = 1.2)

# Combine in a grid: 2 columns × 3 rows (one empty space)
combined_plot <- (p1 | p2 | plot_spacer()) /
  (p3 | p4 | p5) 
combined_plot


# Supplementary Information -----------------------------------------------

# Table S1: re-fitting the models controlling for non-music activities  ----------------------------------------------

cdi_infancy_int_nonmusic <- lm(cdi_raw ~ age + education_year + non_music_total_times + bilingual * music_infant_total_times, data = almmlabsurvey) 
summary(cdi_infancy_int_nonmusic)
cdi_concurrent_int_nonmusic <- lm(cdi_raw ~ age + education_year + non_music_total_times + bilingual * music_current_total_times, data = almmlabsurvey) 
summary(cdi_concurrent_int_nonmusic)
cdi_cumulative_int_nonmusic <- lm(cdi_raw ~ age + education_year + non_music_total_times + bilingual * music_total_times, data = almmlabsurvey) 
summary(cdi_cumulative_int_nonmusic)

lui_infancy_int_nonmusic <- lm(lui_raw ~ age + education_year + non_music_total_times + bilingual * music_infant_total_times, data = almmlabsurvey) 
summary(lui_infancy_int_nonmusic)
lui_concurrent_int_nonmusic <- lm(lui_raw ~ age + education_year + non_music_total_times + bilingual * music_current_total_times, data = almmlabsurvey) 
summary(lui_concurrent_int_nonmusic)
lui_cumulative_int_nonmusic <- lm(lui_raw ~ age + education_year + non_music_total_times + bilingual * music_total_times, data = almmlabsurvey) 
summary(lui_cumulative_int_nonmusic)

emq_g_infancy_int_nonmusic <- lm(emq_g ~ age + education_year + non_music_total_times + bilingual * music_infant_total_times, data = almmlabsurvey) 
emq_g_concurrent_int_nonmusic <- lm(emq_g ~ age + education_year + non_music_total_times + bilingual * music_current_total_times, data = almmlabsurvey) 
emq_g_cumulative_int_nonmusic <- lm(emq_g ~ age + education_year + non_music_total_times + bilingual * music_total_times, data = almmlabsurvey) 

emq_pa_infancy_int_nonmusic <- lm(emq_pa ~ age + education_year + non_music_total_times + bilingual * music_infant_total_times, data = almmlabsurvey) 
emq_pa_concurrent_int_nonmusic <- lm(emq_pa ~ age + education_year + non_music_total_times + bilingual * music_current_total_times, data = almmlabsurvey) 
emq_pa_cumulative_int_nonmusic <- lm(emq_pa ~ age + education_year + non_music_total_times + bilingual * music_total_times, data = almmlabsurvey)

stars <- c(`***` = .001, `**` = .01, `*` = .05, `~` = .1)
modelsummary(
  list("Model 1" = cdi_infancy_int_nonmusic, "Model 2" = cdi_concurrent_int_nonmusic, "Model 3" = cdi_cumulative_int_nonmusic,
       "Model 4" = lui_infancy_int_nonmusic, "Model 5" = lui_concurrent_int_nonmusic, "Model 6" = lui_cumulative_int_nonmusic,
       "Model 7" = emq_g_infancy_int_nonmusic, "Model 8" = emq_g_concurrent_int_nonmusic, "Model 9" = emq_g_cumulative_int_nonmusic,
       "Model 10" = emq_pa_infancy_int_nonmusic, "Model 11" = emq_pa_concurrent_int_nonmusic, "Model 12" = emq_pa_cumulative_int_nonmusic),
  statistic = "({std.error})",  # Show standard errors in parentheses
  stars = stars,
  fmt = 2,                      # Round all numeric values to 2 decimal places
  output = "/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/tableS2_regression_int_nonmusic.docx"
)

# Table S2: descriptive stats by cohort  ------------------------------------------------------

almmlab <- almmlabsurvey_20250815[almmlabsurvey_20250815$lab == 1, ]
almmsurvey <- almmlabsurvey_20250815[almmlabsurvey_20250815$lab == 0, ]
almmlabsurvey <- almmlabsurvey_20250815

# pre-pandemic cohort
mean(almmlab$age)
sd(almmlab$age)
range(almmlab$age)

table(almmlab$female)
mean(almmlab$female)
table(almmlab$bilingual)
mean(almmlab$bilingual)
table(almmlab$music)
mean(almmlab$music)
14/30

summary(almmlab$education_year)
sd(almmlab$education_year)
table(almmlab$education_year)
1/30
5/30
24/30

summary(almmlab$music_infant_total_times)
sd(almmlab$music_infant_total_times)
summary(almmlab$music_current_total_times) 
sd(almmlab$music_current_total_times)
summary(almmlab$music_total_times)
sd(almmlab$music_total_times)
summary(almmlab$non_music_total_times)
mean(almmlab$non_music_total_times)
sd(almmlab$non_music_total_times)

summary(almmlab$cdi_raw)
sd(almmlab$cdi_raw)
summary(almmlab$lui_raw)
sd(almmlab$lui_raw, na.rm = TRUE)
summary(almmlab$emq_g)
sd(almmlab$emq_g, na.rm = TRUE)
summary(almmlab$emq_pa)
sd(almmlab$emq_pa, na.rm = TRUE)

# remote cohort
mean(almmsurvey$age)
sd(almmsurvey$age)
range(almmsurvey$age)

table(almmsurvey$female)
mean(almmsurvey$female)
table(almmsurvey$bilingual)
mean(almmsurvey$bilingual)

summary(almmsurvey$education_year)
sd(almmsurvey$education_year)
table(almmsurvey$education_year)
2/76
17/76
57/76

table(almmsurvey$music)
mean(almmsurvey$music)
18/76

summary(almmsurvey$music_infant_total_times)
sd(almmsurvey$music_infant_total_times)
summary(almmsurvey$music_current_total_times) 
sd(almmsurvey$music_current_total_times)
summary(almmsurvey$music_total_times)
mean(almmsurvey$music_total_times)
sd(almmsurvey$music_total_times)
summary(almmsurvey$non_music_total_times)
mean(almmsurvey$non_music_total_times)
sd(almmsurvey$non_music_total_times)

summary(almmsurvey$cdi_raw)
sd(almmsurvey$cdi_raw)
summary(almmsurvey$lui_raw)
sd(almmsurvey$lui_raw, na.rm = TRUE)
summary(almmsurvey$emq_g)
sd(almmsurvey$emq_g, na.rm = TRUE)
summary(almmsurvey$emq_pa)
sd(almmsurvey$emq_pa, na.rm = TRUE)

# Table S2: t-tests and chi-square tests comparing cohorts -------------------------------------------------

t.test(almmlabsurvey$age~almmlabsurvey$lab)
t.test(almmlabsurvey$bilingual~almmlabsurvey$lab) #*
t.test(almmlabsurvey$education_year~almmlabsurvey$lab)
t.test(almmlabsurvey$cdi_raw~almmlabsurvey$lab)
t.test(almmlabsurvey$lui_raw~almmlabsurvey$lab)
t.test(almmlabsurvey$emq_g~almmlabsurvey$lab)
t.test(almmlabsurvey$emq_pa~almmlabsurvey$lab)

t.test(almmlabsurvey$music_total_times~almmlabsurvey$lab) #~
t.test(almmlabsurvey$music_infant_total_times~almmlabsurvey$lab) 
t.test(almmlabsurvey$music_current_total_times~almmlabsurvey$lab) #*
t.test(almmlabsurvey$non_music_total_times~almmlabsurvey$lab) 

lab_nobilingual_bilingual <- c(16, 14)
survey_nobilingual_bilingual <- c(58, 18)
conttable_bilingual <- data.frame(lab_nobilingual_bilingual, survey_nobilingual_bilingual)
chisq.test(conttable_bilingual) #* 

lab_nomusicclass_musicclass <- c(16, 14)
survey_nomusicclass_musicclass <- c(58, 18)
conttable_musicclass <- data.frame(lab_nomusicclass_musicclass, survey_nomusicclass_musicclass)
chisq.test(conttable_musicclass) #* 

table(almmlab$female)
table(almmsurvey$female)
lab_male_female <- c(12, 18)
survey_male_female <- c(37, 39)
conttable_sex <- data.frame(lab_male_female, survey_male_female)
chisq.test(conttable_sex) # ns


# Table S3: are multilingual exposure or lower vocabulary driving the interaction effects --------

lui_infancy_int_mono <- lm(lui_raw ~ age + education_year + cdi_raw * music_infant_total_times, data = almmmono) 
summary(lui_infancy_int_mono)
lui_concurrent_int_mono <- lm(lui_raw ~ age + education_year + cdi_raw * music_current_total_times, data = almmmono) 
summary(lui_concurrent_int_mono)
lui_cumulative_int_mono <- lm(lui_raw ~ age + education_year + cdi_raw * music_total_times, data = almmmono) 
summary(lui_cumulative_int_mono)

stars <- c(`***` = .001, `**` = .01, `*` = .05, `~` = .1)
modelsummary(
  list("Model 1" = lui_infancy_int_mono, "Model 2" = lui_concurrent_int_mono, "Model 3" = lui_cumulative_int_mono),
  statistic = "({std.error})",  # Show standard errors in parentheses
  stars = stars,
  fmt = 2,                      # Round all numeric values to 2 decimal places
  output = "/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/tableS3_regression_int.docx"
)
# Manually reported additional decimal places for small unstandardized coefficients (|B| < .01 when rounded) and 
# standard errors (SE < .01 when rounded) to improve interpretability.



