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
labwithmaap_20250726 <- read.csv("/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/labwithmaap_20250726.csv", header = TRUE)
almmlabsurvey_20250726 <- read.csv("/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/almmlabsurvey_20250726.csv", header = TRUE)
music_timesperweek <- read.csv("/Users/ranwei/Dropbox (Personal)/ALMM/ALMM data/music_timesperweek.csv", header = TRUE)

labwithmaap_20250726$X <- NULL
almmlabsurvey_20250726$X <- NULL
music_timesperweek$X <- NULL

# almmlabsurvey_20250726.csv = all behavioral and survey data, both cohorts
# labwithmaap_20250726.csv = all behavioral, survey, and eye tracking data, lab cohort, long format

# Table 1 descriptive stats  ------------------------------------------------------

almmlab <- almmlabsurvey_20250726[almmlabsurvey_20250726$lab == 1, ]
almmsurvey <- almmlabsurvey_20250726[almmlabsurvey_20250726$lab == 0, ]
almmlabsurvey <- almmlabsurvey_20250726

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


# missing data -----------------------------------------------------------
sum(is.na(almmlabsurvey$music_infant_total))
sum(is.na(almmlabsurvey$music_toddler_total))
sum(is.na(almmlabsurvey$cdi_raw))
sum(is.na(almmlabsurvey$lui_raw)) # 5 missing 
sum(is.na(almmlabsurvey$emq_pa))
sum(is.na(almmlabsurvey$emq_g))


# t-tests and chi-square tests comparing groups -------------------------------------------------
t.test(almmlabsurvey$age~almmlabsurvey$lab)
t.test(almmlabsurvey$female~almmlabsurvey$lab)
t.test(almmlabsurvey$bilingual~almmlabsurvey$lab) #*
t.test(almmlabsurvey$education_year~almmlabsurvey$lab)
t.test(almmlabsurvey$cdi_raw~almmlabsurvey$lab)
t.test(almmlabsurvey$lui_raw~almmlabsurvey$lab)
t.test(almmlabsurvey$emq_g~almmlabsurvey$lab)
t.test(almmlabsurvey$emq_pa~almmlabsurvey$lab)

t.test(almmlabsurvey$music_total_times~almmlabsurvey$lab) #~
t.test(almmlabsurvey$music_infant_total_times~almmlabsurvey$lab) 
t.test(almmlabsurvey$music_current_total_times~almmlabsurvey$lab) #*
t.test(almmlabsurvey$non_music_total_times~almmlabsurvey$lab) #~

lab_nobilingual_bilingual <- c(16, 14)
survey_nobilingual_bilingual <- c(58, 18)
conttable_bilingual <- data.frame(lab_nobilingual_bilingual, survey_nobilingual_bilingual)
chisq.test(conttable_bilingual) #* 

lab_nomusicclass_musicclass <- c(16, 14)
survey_nomusicclass_musicclass <- c(58, 18)
conttable_musicclass <- data.frame(lab_nomusicclass_musicclass, survey_nomusicclass_musicclass)
chisq.test(conttable_musicclass) #* 

# calculating cronbach's alpha of music survey --------------------------------------------

mus_columns <- names(almmlabsurvey_20250726)[
  grepl("mus", names(almmlabsurvey_20250726))]

mus_columns <- mus_columns[1:(length(mus_columns) - 7)]
mus_columns <- c("id", mus_columns)
music_raw <- almmlabsurvey_20250726[, mus_columns, drop = FALSE]
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


# Power analysis for interaction term ----------------------------------------------------------
install.packages("pwr")
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


# TO DO: calculating EMQ alpha ---------------------------------------------------



# RQ 1: descriptive stats of music survey - full sample -------------------

summary(almmlabsurvey$music_infant_total_times)
sd(almmlabsurvey$music_infant_total_times)
summary(almmlabsurvey$music_current_total_times) 
sd(almmlabsurvey$music_current_total_times)
summary(almmlabsurvey$non_music_total_times)
sd(almmlabsurvey$non_music_total_times)

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
              modx.labels = c("Monolingual", "DLL"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Infancy Music Activity",
              y.label = "Vocabulary",
              line.thickness = 1.2)

p2 <- interact_plot(cdi_cumulative_int,
              pred = music_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual", "DLL"),
              colors = c("#A26769", "#D9BF77"),
              x.label = "Cumulative Music Activity",
              y.label = "Vocabulary",
              line.thickness = 1.2)

p3 <- interact_plot(lui_infancy_int,
              pred = music_infant_total_times,
              modx = bilingual,
              plot.points = TRUE,        # <- adds data points
              interval = TRUE,
              modx.labels = c("Monolingual", "DLL"),
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

