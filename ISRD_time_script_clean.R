
############################################################################
#Analytic code for:
#Unstructured Spare Time as an International Predictor of Adolescent Crime
############################################################################

rm(list=ls())

options(scipen=999)

# List of required packages
packages <- c(
  "haven", "here", "lavaan", "lavaanPlot", "DiagrammeRsvg", "rsvg", "magick", 
  "purrr", "forcats", "broom", "vcd", "AER", "rsq", "lme4", "car", 
  "ggplot2", "ggpubr", "boot", "sandwich", "lmtest", "psych", 
  "sjPlot", "tidyr", "dplyr"
)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

#Load data
ISRD4 <- read_sav(here("data/ISRD4-SQ_21.Sav"), encoding="latin1")

#Check sample size by country
table(ISRD4$scountry)
sum(table(ISRD4$scountry))
mean(table(ISRD4$scountry))
min(table(ISRD4$scountry))
max(table(ISRD4$scountry))

#Check survey mode by country
ISRD4 %>%
  group_by(scountry, srvmode) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(scountry, desc(count)) %>%
  print(n = 23)

#Check languages by country
ISRD4 %>%
  group_by(scountry, srvlang) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(scountry, desc(count)) %>%
  print(n = 45)

#Check start and end dates by country
ISRD4 %>%
  group_by(scountry) %>%
  summarise(
    min_date = min(srvdate, na.rm = TRUE),
    max_date = max(srvdate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(scountry) %>%
  print(n = 21)

#Check number schools by country
ISRD4 %>%
  group_by(scountry) %>%
  summarise(unique_schools = n_distinct(school), .groups = "drop") %>%
  arrange(scountry) %>%
  print(n = 21)

#Rename Bosnia (name too long)
ISRD4 <- ISRD4 %>%
  mutate(scountry = ifelse(scountry == "Bosnia and Herzegovina", "Bosnia Herz.", scountry))

#Explore spare time
##Each activity below assumed 2 hours a day each time reported
#Meal with family: table(ISRD4$freeact1)
#Hang around in streets, shopping centres: table(ISRD4$freeact2)
#Study or do homework: table(ISRD4$freeact3)
#Parties in evening: table(ISRD4$freeact4)
#Have job: table(ISRD4$freeact5)
#Truancy (assumed 6.5 hours):  table(ISRD4$truancy_ia)
##Each activity assumed 10min (0.17) each time reported
#Look up info for school or work: table(ISRD4$webact01)
#Play games: table(ISRD4$webact02)
#Darknet: table(ISRD4$webact03)
#Social media: table(ISRD4$webact04)
#Sites for adults: table(ISRD4$webact06)
#Gamble: table(ISRD4$webact07)
#Online for something else: ISRD4$webact08 (not used)

#Recode spare time
ISRD4 <- ISRD4 %>%
  mutate(across(c("freeact1", "freeact2", "freeact3", "freeact4", "freeact5"),
                ~ recode(as.numeric(.), 
                         `1` = 0, #never
                         `2` = 2, #once a week (1 x 2)
                         `3` = 5, #two-three times a week (2.5 x 2)
                         `4` = 10, #four-six times a week (5 x 2)
                         `5` = 14, #every day (7 x 2)
                         .default = NaN))) %>%
  mutate(across(c("webact01","webact02","webact03","webact04","webact06","webact07","webact08"),
                ~ recode(as.numeric(.), 
                         `1` = 0, #never
                         `2` = 0.33, #rarely: 2 times a week (2 x 0.17)
                         `3` = 0.66, #few times a week: 4 times (4 x 0.17)
                         `4` = 4.66, #few times a day: 4 times (4 x 7 x 0.17)
                         `5` = 18.66, #once an hour (24(-8) x 7 x 0.17)
                         `6` = 55.99, #few times an hour: 3 times (3 x 24(-8) x 7 x 0.17)
                         .default = NaN))) %>%
  mutate(truancy_ia = ifelse(truancyp == 0, 0, truancy_ia),
         truancy_ia = ifelse(truancy_ia > 200, NA, truancy_ia),
         truancy = truancy_ia * 6.5 / 52) %>% #number of times * 6.5 / number of weeks each year
  mutate(unstr_st_outside = freeact2 + freeact4 + truancy,
         unstr_st_home = webact02 + webact03 + webact04 + webact06 + webact07,
         str_st_home = webact01 + freeact3 + freeact1,
         non_st = freeact5 +
           (5 * 6.5) + #days school: 5 days a week * 6.5
           (7 * 8) #sleep: 7 days a week * 8
  )

#Check density plots
plot(density(ISRD4$unstr_st_outside[!is.na(ISRD4$unstr_st_outside)] / 7))
plot(density(ISRD4$unstr_st_home[!is.na(ISRD4$unstr_st_home)] / 7))
plot(density(ISRD4$str_st_home[!is.na(ISRD4$str_st_home)] / 7))
plot(density(ISRD4$non_st[!is.na(ISRD4$non_st)] / 7))

#Standardise time to max 24 hours
ISRD4 <- ISRD4 %>%
  mutate(total = unstr_st_outside + unstr_st_home + str_st_home + non_st,
         unstr_st_outside = (unstr_st_outside * 24) / total,
         unstr_st_home = (unstr_st_home * 24) /  total,
         unstr_st = unstr_st_outside + unstr_st_home,
         str_st_home = (str_st_home * 24) / total,
         non_st = (non_st  * 24) / total,
         total24 = unstr_st_outside + unstr_st_home + str_st_home + non_st)

#Check density plots
plot(density(ISRD4$unstr_st_outside[!is.na(ISRD4$unstr_st_outside)]))
plot(density(ISRD4$unstr_st_home[!is.na(ISRD4$unstr_st_home)]))
plot(density(ISRD4$unstr_st[!is.na(ISRD4$unstr_st)]))
plot(density(ISRD4$str_st_home[!is.na(ISRD4$str_st_home)]))
plot(density(ISRD4$non_st[!is.na(ISRD4$non_st)]))

#summary statistics time use
summary(ISRD4$unstr_st_outside)
sd(ISRD4$unstr_st_outside, na.rm = TRUE)
summary(ISRD4$unstr_st_home)
sd(ISRD4$unstr_st_home, na.rm = TRUE)
summary(ISRD4$str_st_home)
sd(ISRD4$str_st_home, na.rm = TRUE)
summary(ISRD4$non_st)
sd(ISRD4$non_st, na.rm = TRUE)

#Describe differences in time use across countries
countries_st <- ISRD4 %>%
  group_by(scountry) %>%
  summarise(unstr_st_outside_mean = mean(unstr_st_outside, na.rm = T),
            unstr_st_home_mean = mean(unstr_st_home, na.rm = T),
            unstr_st_mean = mean(unstr_st, na.rm = T),
            str_st_home_mean = mean(str_st_home, na.rm = T),
            non_st_mean = mean(non_st, na.rm = T),
            total24_mean = mean(total24, na.rm = T),
            unstr_st_outside_sd = sd(unstr_st_outside, na.rm = T),
            unstr_st_home_sd = sd(unstr_st_home, na.rm = T),
            unstr_st_sd = sd(unstr_st, na.rm = T),
            str_st_home_sd = sd(str_st_home, na.rm = T),
            non_st_sd = sd(non_st, na.rm = T)
  )

#Explore self-report offending
##DAMAGE
#Graffiti table(ISRD4$graf_ie)
#Vandalism table(ISRD4$vand_ie)
##PROPERTY
#Shoplifting table(ISRD4$shop_ie)
#Burglary table(ISRD4$burg_ie)
#Vehicle theft table(ISRD4$cart_ie)
##VIOLENCE
#Robbery table(ISRD4$exto_ie)
#Group fight table(ISRD4$gfig_ie)
#Assault table(ISRD4$aslt_ie)
##DRUGS
#Sold drugs table(ISRD4$drud_ie)
##CYBER
#Shared images table(ISRD4$shin_ie)
#Cyber hate table(ISRD4$cyhc_ie)
#Cyber fraud table(ISRD4$cyfr_ie)
#Hacking table(ISRD4$hack_ie)

#Recode  self-report offending
ISRD4 <- ISRD4 %>%
  mutate(across(c("graf_ie", "vand_ie", "shop_ie", "burg_ie",
                  "cart_ie", "exto_ie", "gfig_ie", "aslt_ie",
                  "drud_ie", "shin_ie", "cyhc_ie", "cyfr_ie",
                  "hack_ie", "graflyi", "vandlyi", "shoplyi", "burglyi",
                  "cartlyi", "extolyi", "gfiglyi", "asltlyi",
                  "drudlyi", "shinlyi", "cyhclyi", "cyfrlyi",
                  "hacklyi"),
                ~ recode(as.numeric(.x), 
                         `9996` = NA_real_,
                         `9997` = NA_real_,
                         `9999` = NA_real_,
                         `99996` = NA_real_,
                         `99997` = NA_real_,
                         `99999` = NA_real_))) %>%
  mutate(graf_ie_rec = ifelse(is.na(graf_ie), 0, graf_ie),
         vand_ie_rec = ifelse(is.na(vand_ie), 0, vand_ie),
         shop_ie_rec = ifelse(is.na(shop_ie), 0, shop_ie),
         burg_ie_rec = ifelse(is.na(burg_ie), 0, burg_ie),
         cart_ie_rec = ifelse(is.na(cart_ie), 0, cart_ie),
         exto_ie_rec = ifelse(is.na(exto_ie), 0, exto_ie),
         gfig_ie_rec = ifelse(is.na(gfig_ie), 0, gfig_ie),
         aslt_ie_rec = ifelse(is.na(aslt_ie), 0, aslt_ie),
         drud_ie_rec = ifelse(is.na(drud_ie), 0, drud_ie),
         shin_ie_rec = ifelse(is.na(shin_ie), 0, shin_ie),
         cyhc_ie_rec = ifelse(is.na(cyhc_ie), 0, cyhc_ie),
         cyfr_ie_rec = ifelse(is.na(cyfr_ie), 0, cyfr_ie),
         hack_ie_rec = ifelse(is.na(hack_ie), 0, hack_ie)
  ) %>%
  mutate(off_damage = graf_ie_rec + vand_ie_rec,
         off_damage = ifelse(is.na(graf_ie) & is.na(vand_ie), NA, off_damage),
         off_property = shop_ie_rec + burg_ie_rec + cart_ie_rec,
         off_property = ifelse(is.na(shop_ie) & is.na(burg_ie) & is.na(cart_ie), 
                               NA, off_property),
         off_violence = exto_ie_rec + gfig_ie_rec + aslt_ie_rec,
         off_violence = ifelse(is.na(exto_ie) & is.na(gfig_ie) & is.na(aslt_ie),
                               NA, off_violence),
         off_drugs = drud_ie_rec,
         off_drugs = ifelse(is.na(drud_ie), NA, off_drugs),
         off_cyber = shin_ie_rec + cyhc_ie_rec + cyfr_ie_rec + hack_ie_rec,
         off_cyber = ifelse(is.na(shin_ie) & is.na(cyhc_ie) & is.na(cyfr_ie) & is.na(hack_ie),
                            NA, off_cyber),
         offending = off_damage + off_property + off_violence + off_drugs + off_cyber,
         offending = ifelse(is.na(off_damage) & is.na(off_property) & is.na(off_violence) &
                              is.na(off_drugs) & is.na(off_cyber), NA, offending))

hist(ISRD4$offending, breaks = 100)
table(ISRD4$offending)
table(is.na(ISRD4$offending))

#Count number of observations marked as outliers
original_vars <- c("graflyi", "vandlyi", "shoplyi", "burglyi",
                   "cartlyi", "extolyi", "gfiglyi", "asltlyi",
                   "drudlyi", "shinlyi", "cyhclyi", "cyfrlyi",
                   "hacklyi")

adjusted_vars <- gsub("lyi$", "_ie", original_vars)

na_outlier_rows <- rep(FALSE, nrow(ISRD4))

for (i in seq_along(original_vars)) {
  condition <- is.na(ISRD4[[adjusted_vars[i]]]) & !is.na(ISRD4[[original_vars[i]]])
  na_outlier_rows <- na_outlier_rows | condition
}

sum(na_outlier_rows)
sum(na_outlier_rows) / nrow(ISRD4) * 100

#Calculate binary measure of offending
ISRD4 <- ISRD4 %>%
  mutate(off_damage_bin = ifelse(off_damage > 0 & !is.na(off_damage), 1, NA),
         off_damage_bin = ifelse(off_damage == 0 & !is.na(off_damage), 0, off_damage_bin),
         off_property_bin = ifelse(off_property > 0 & !is.na(off_property), 1, NA),
         off_property_bin = ifelse(off_property == 0 & !is.na(off_property), 0, off_property_bin),
         off_violence_bin = ifelse(off_violence > 0 & !is.na(off_violence), 1, NA),
         off_violence_bin = ifelse(off_violence == 0 & !is.na(off_violence), 0, off_violence_bin),
         off_drugs_bin = ifelse(off_drugs > 0 & !is.na(off_drugs), 1, NA),
         off_drugs_bin = ifelse(off_drugs == 0 & !is.na(off_drugs), 0, off_drugs_bin),
         off_cyber_bin = ifelse(off_cyber > 0 & !is.na(off_cyber), 1, NA),
         off_cyber_bin = ifelse(off_cyber == 0 & !is.na(off_cyber), 0, off_cyber_bin),
         offending_bin = ifelse(offending > 0 & !is.na(offending), 1, NA),
         offending_bin = ifelse(offending == 0 & !is.na(offending), 0, offending_bin))
hist(ISRD4$offending_bin)
table(is.na(ISRD4$offending_bin))

#Describe differences in offending use across countries
countries_crime <- ISRD4 %>%
  group_by(scountry) %>%
  summarise(
    off_damage = mean(off_damage, na.rm = TRUE),
    off_drugs = mean(off_drugs, na.rm = TRUE),
    off_violence = mean(off_violence, na.rm = TRUE),
    off_cyber = mean(off_cyber, na.rm = TRUE),
    offending_mean = mean(offending, na.rm = TRUE),
    offending_sd = sd(offending, na.rm = TRUE),
    off_damage_bin = mean(off_damage_bin, na.rm = TRUE),
    off_drugs_bin = mean(off_drugs_bin, na.rm = TRUE),
    off_violence_bin = mean(off_violence_bin, na.rm = TRUE),
    off_cyber_bin = mean(off_cyber_bin, na.rm = TRUE),
    offending_bin_mean = mean(offending_bin, na.rm = TRUE),
    offending_bin_sd = sd(offending_bin, na.rm = TRUE)
  )

#Merge country-level estimates
countries <- countries_st %>%
  left_join(countries_crime, by = "scountry")
write.csv(countries, here('plots/countries.csv'))

#Groups of offenders
ISRD4 <- ISRD4 %>%
  filter(!is.na(unstr_st) & !is.na(offending)) %>%  # Remove NAs
  group_by(scountry) %>%
  mutate(
    offending_group = case_when(
      offending == 0 ~ "Non-offender",
      offending > 0 & offending <= quantile(offending, 0.95, na.rm = TRUE) ~ "Low frequency",
      offending > quantile(offending, 0.95, na.rm = TRUE) ~ "High frequency"
    )
  ) %>%
  ungroup() %>%
  mutate(offending_group = factor(offending_group, levels = c("Non-offender", "Low frequency", "High frequency")))

#Pairwise t-tests adjusted for multiple testing
ISRD4 %>%
  group_by(offending_group) %>%
  summarise(mean = mean(unstr_st, na.rm = T))
ISRD4 %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Argentina") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Austria") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Bosnia Herz.") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Brazil") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Colombia") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Czech Republic") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Denmark") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Estonia") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Finland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Iceland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Lithuania") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Mexico") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Norway") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Poland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Slovenia") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Spain") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Sweden") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Sweden") %>%
  group_by(offending_group) %>%
  summarise(mean = mean(unstr_st, na.rm = T))
ISRD4 %>%
  filter(scountry == "Switzerland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "UK") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "USA") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "USA") %>%
  group_by(offending_group) %>%
  summarise(mean = mean(unstr_st, na.rm = T))
ISRD4 %>%
  filter(scountry == "Venezuela") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group, p.adjust.method = "BH")}

#Plot boxplots of offending by country
ggplot(ISRD4, aes(x = offending_group, y = unstr_st, fill = offending_group)) +
  geom_boxplot() +
  facet_wrap(~ scountry, scales = "free", nrow = 6, ncol = 4) +  # Set 6 rows, 4 columns
  labs(title = "",
       x = "",
       y = "Unstructured spare time") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("#E66100", "#5D3A9B", "#40B0A6")) +
  scale_x_discrete(labels = c("Non-offender" = "NO", "Low frequency" = "LF", "High frequency" = "HF")) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5), 
    axis.text.y = element_text(size = 10), 
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = NULL))

ggsave(here('plots/boxplot_by_country.jpg'), width = 8, height = 12)

#Replicate with groups defined by percentiles across pooled national samples
quantile(ISRD4$offending, 0.95, na.rm = TRUE)

ISRD4 <- ISRD4 %>%
  filter(!is.na(unstr_st) & !is.na(offending)) %>%  # Remove NAs
  mutate(
    offending_group_pooled = case_when(
      offending == 0 ~ "Non-offender",
      offending > 0 & offending <= quantile(offending, 0.95, na.rm = TRUE) ~ "Low frequency",
      offending > quantile(offending, 0.95, na.rm = TRUE) ~ "High frequency"
    )
  ) %>%
  ungroup() %>%
  mutate(offending_group_pooled = factor(offending_group_pooled, levels = c("Non-offender", "Low frequency", "High frequency")))

ISRD4 %>%
  group_by(offending_group_pooled) %>%
  summarise(mean = mean(unstr_st, na.rm = T))
ISRD4 %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Argentina") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Austria") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Bosnia Herz.") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Brazil") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Colombia") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Czech Republic") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Denmark") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Estonia") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Finland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Iceland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Lithuania") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Mexico") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Norway") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Poland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Slovenia") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Spain") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Sweden") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "Sweden") %>%
  group_by(offending_group_pooled) %>%
  summarise(mean = mean(unstr_st, na.rm = T))
ISRD4 %>%
  filter(scountry == "Switzerland") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "UK") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "USA") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}
ISRD4 %>%
  filter(scountry == "USA") %>%
  group_by(offending_group_pooled) %>%
  summarise(mean = mean(unstr_st, na.rm = T))
ISRD4 %>%
  filter(scountry == "Venezuela") %>%
  {pairwise.t.test(.$unstr_st, .$offending_group_pooled, p.adjust.method = "BH")}

ggplot(ISRD4, aes(x = offending_group_pooled, y = unstr_st, fill = offending_group_pooled)) +
  geom_boxplot() +
  facet_wrap(~ scountry, scales = "free", nrow = 6, ncol = 4) +  # Set 6 rows, 4 columns
  labs(title = "",
       x = "",
       y = "Unstructured spare time") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("#E66100", "#5D3A9B", "#40B0A6")) +
  scale_x_discrete(labels = c("Non-offender" = "NO", "Low frequency" = "LF", "High frequency" = "HF")) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),  
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5), 
    axis.text.y = element_text(size = 10), 
    axis.title = element_text(size = 12, face = "bold"),  
    strip.text = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 14),  
    legend.title = element_text(size = 12, face = "bold"),  
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = NULL))

ggsave(here('plots/boxplot_by_country_pooled.jpg'), width = 8, height = 12)

#Explore self-control measures
#ISRD4$selfctr1 I act on the spur of the moment without stopping to think
#ISRD4$selfctr2 I do whatever brings me pleasure here and now, even at the cost of some future goal
#ISRD4$selfctr3 I’m more concerned with what happens to me in the short run than in the long run
#ISRD4$selfctr4 I like to test myself every now and then by doing something a little risky
#ISRD4$selfctr5 Sometimes I will take a risk just for the fun of it
#ISRD4$selfctr6 Excitement and adventure are more important to me than security

#Recode NAs
ISRD4 <- ISRD4 %>%
  mutate(across(c(selfctr1, selfctr2, selfctr3, selfctr4, selfctr5, selfctr6),
                ~ recode(as.numeric(.x),
                         `7` = NA_real_,
                         `9` = NA_real_)))

#Recode 5s to 3s in Czech Republic (coding error)
ISRD4 <- ISRD4 %>%
  mutate(across(
    .cols = c(selfctr1, selfctr2, selfctr3, selfctr4, selfctr5, selfctr6),
    .fns = ~ case_when(
      scountry == "Czech Republic" & .x == 5 ~ 3,
      scountry == "Czech Republic" & .x == 3 ~ 4,
      scountry == "Czech Republic" & .x == 4 ~ 5,
      TRUE ~ .x
    )
  ))

#Estimate CFA model of self control
cfa_selfctr <- 'cfa_selfctr = ~ selfctr1 + selfctr2 + selfctr3 + selfctr4 + selfctr5 + selfctr6'

#Run CFA model (missingness option to full information maximum likelihood (FIML))
cfa_selfctr_model <- cfa(cfa_selfctr, missing = "fiml", data = ISRD4)

#Summary of CFA models
summary(cfa_selfctr_model, fit.measures = TRUE)
# CFI and TLI need to be larger than 0.9 (they are not!) 
# CFI=0.894; TLI=0.824; RMSEA=0.144

#Estimate CFA model of self control with fewer items
cfa_selfctr2 <- 'cfa_selfctr = ~ selfctr1 + selfctr2 + selfctr3 + selfctr4'

#Run CFA model (missingness option to full information maximum likelihood (FIML))
cfa_selfctr2_model <- cfa(cfa_selfctr2, missing = "fiml", data = ISRD4)

#Summary of CFA models
summary(cfa_selfctr2_model, fit.measures = TRUE)
# CFI and TLI need to be larger than 0.9 (they are!) 
# CFI=0.997; TLI=0.990; RMSEA=0.029

# Custom labels
selfctr_labels <- c(
  cfa_selfctr = "Self-control",
  selfctr1 = "I act on the spur of the moment\nwithout stopping to think",
  selfctr2 = "I do whatever brings me pleasure here and now,\neven at the cost of some future goal",
  selfctr3 = "I’m more concerned with what happens to me\nin the short run than in the long run",
  selfctr4 = "I like to test myself every now and then\nby doing something a little risky"
)

# Extract model fit statistics
selfctr_indices <- fitMeasures(cfa_selfctr2_model, c("cfi", "tli", "rmsea"))
selfctr_text <- sprintf("Model fit: CFI = %.3f   TLI = %.3f   RMSEA = %.3f",
                        selfctr_indices["cfi"], selfctr_indices["tli"], selfctr_indices["rmsea"])

# Generate the CFA diagram
selfctr_graph <- lavaanPlot(
  model = cfa_selfctr2_model,
  stand = TRUE,
  coefs = TRUE,
  covs = FALSE,
  stars = c("regression"),
  labels = selfctr_labels,
  node_options = list(shape = "box", fontname = "Helvetica", color = "black"),
  edge_options = list(color = "black", fontname = "Helvetica", fontsize = 12),
  graph_options = list(rankdir = "LR")
)

# Export to SVG, then render as PNG
selfctr_svg_code <- DiagrammeRsvg::export_svg(selfctr_graph)
rsvg::rsvg_png(charToRaw(selfctr_svg_code), file = "plots/selfcontrol_CFA_body.png", width = 2400, height = 1600)

# Read diagram and flatten to white background (removes black box artefacts)
selfctr_img_diagram <- image_read("plots/selfcontrol_CFA_body.png") %>%
  image_background("white", flatten = TRUE)

# Add large "A" in top-left corner
selfctr_img_diagram_labeled <- image_annotate(
  selfctr_img_diagram,
  text = "A",
  size = 100,
  gravity = "northwest",
  location = "+60+40",
  font = "Helvetica",
  color = "black",
  weight = 700
)

# Create separate image for model fit measures
selfctr_img_text <- image_blank(width = 2400, height = 150, color = "white") %>%
  image_annotate(
    text = selfctr_text,
    size = 75,
    gravity = "center",
    font = "Helvetica",
    color = "black"
  )

# Combine diagram and fit stats vertically
img_final <- image_append(c(selfctr_img_diagram_labeled, selfctr_img_text), stack = TRUE)

# Save as final JPG
image_write(img_final, path = "plots/selfcontrol_CFA.jpg", format = "jpeg", quality = 100)

#Calculate latent scores
ISRD4$cfa_selfctr <- as.data.frame(lavPredict(cfa_selfctr2_model))$cfa_selfctr

#Convert latent scores to 0-1
ISRD4 <- ISRD4 %>%
  mutate(cfa_selfctr = (cfa_selfctr - min(cfa_selfctr, na.rm = TRUE)) / 
           (max(cfa_selfctr, na.rm = TRUE) - min(cfa_selfctr, na.rm = TRUE)) * 10)

#Explore delinquent peers
#friends stolen table(ISRD4$delpsln_a)
#friends burglary ISRD4$delpbun_a
#friends assault ISRD4$delpasn_a
#friends online images ISRD4$delpsin_a
#friends hacking ISRD4$delphan_a

#Recode  delinquent peers
ISRD4 <- ISRD4 %>%
  mutate(across(c("delpsln_a", "delpbun_a", "delpasn_a", "delpsin_a", "delphan_a"),
                ~ recode(as.numeric(.x), 
                         `9996` = NA_real_,
                         `9997` = NA_real_,
                         `9999` = NA_real_,
                         `99996` = NA_real_,
                         `99997` = NA_real_,
                         `99999` = NA_real_))) %>%
  mutate(delpsln_a_rec = ifelse(is.na(delpsln_a), 0, delpsln_a),
         delpbun_a_rec = ifelse(is.na(delpbun_a), 0, delpbun_a),
         delpasn_a_rec = ifelse(is.na(delpasn_a), 0, delpasn_a),
         delpsin_a_rec = ifelse(is.na(delpsin_a), 0, delpsin_a),
         delphan_a_rec = ifelse(is.na(delphan_a), 0, delphan_a)) %>%
  mutate(delinq_peers = delpsln_a_rec + delpbun_a_rec + delpasn_a_rec +
           delpsin_a_rec + delphan_a_rec,
         delinq_peers = ifelse(is.na(delpsln_a) & is.na(delpbun_a) & is.na(delpasn_a) &
                                 is.na(delpsin_a) & is.na(delphan_a), NA, delinq_peers),
         delinq_peers_bin = ifelse(delinq_peers > 0 & !is.na(delinq_peers), 1, delinq_peers))

#Explore measures of exposure to crime
#crime in neighbourhood table(ISRD4$nhinciv1)
#There is a lot of stealing in my school table(ISRD4$schinci1)
#There is a lot of fighting in my school table(ISRD4$schinci2)
#Many things are broken or vandalized in my school table(ISRD4$schinci3)
#There is a lot of drug use in my school table(ISRD4$schinci4)

#Recode NAs
ISRD4 <- ISRD4 %>%
  mutate(across(c("nhinciv1", "nhinciv2", "nhinciv3", "schinci1", "schinci2", "schinci3", "schinci4"),
                ~ recode(as.numeric(.x),
                         `9` = NA_real_,
                         `7` = NA_real_)))

#Estimate CFA model of community exposure to crime
cfa_comexp <- 'cfa_comexp = ~ nhinciv1 + schinci1 + schinci2 + schinci3 + schinci4'

#Run CFA model (missingness option to full information maximum likelihood (FIML))
cfa_comexp_model <- cfa(cfa_comexp, missing = "fiml", data = ISRD4)

#Summary of CFA models
summary(cfa_comexp_model, fit.measures = TRUE)
# CFI and TLI need to be larger than 0.9 (they are!) 
# CFI=0.995; TLI=0.990; RMSEA=0.033

# Custom labels
comexp_labels <- c(
  cfa_comexp = "Exposure to\ncrime",
  nhinciv1 = "There is a lot of crime in my neighbourhood",
  schinci1 = "There is a lot of stealing in my school",
  schinci2 = "There is a lot of fighting in my school",
  schinci3 = "Many things are broken or vandalized in my school",
  schinci4 = "There is a lot of drug use in my school"
)

# Extract model fit statistics
comexp_indices <- fitMeasures(cfa_comexp_model, c("cfi", "tli", "rmsea"))
comexp_text <- sprintf("Model fit: CFI = %.3f   TLI = %.3f   RMSEA = %.3f",
                       comexp_indices["cfi"], comexp_indices["tli"], comexp_indices["rmsea"])

# Generate the CFA diagram
comexp_graph <- lavaanPlot(
  model = cfa_comexp_model,
  stand = TRUE,
  coefs = TRUE,
  covs = FALSE,
  stars = c("regression"),
  labels = comexp_labels,
  node_options = list(shape = "box", fontname = "Helvetica", color = "black"),
  edge_options = list(color = "black", fontname = "Helvetica", fontsize = 12),
  graph_options = list(rankdir = "LR")
)

# Export to SVG, then render as PNG
comexp_svg_code <- DiagrammeRsvg::export_svg(comexp_graph)
rsvg::rsvg_png(charToRaw(comexp_svg_code), file = "plots/comexp_CFA_body.png", width = 2400, height = 1600)

# Read diagram and flatten to white background (removes black box artefacts)
comexp_img_diagram <- image_read("plots/comexp_CFA_body.png") %>%
  image_background("white", flatten = TRUE)

# Add large "C" in top-left corner
comexp_img_diagram_labeled <- image_annotate(
  comexp_img_diagram,
  text = "C",
  size = 100,
  gravity = "northwest",
  location = "+60+40",
  font = "Helvetica",
  color = "black",
  weight = 700
)

# Create separate image for model fit measures
comexp_img_text <- image_blank(width = 2400, height = 150, color = "white") %>%
  image_annotate(
    text = comexp_text,
    size = 75,
    gravity = "center",
    font = "Helvetica",
    color = "black"
  )

# Combine diagram and fit stats vertically
img_final <- image_append(c(comexp_img_diagram_labeled, comexp_img_text), stack = TRUE)

# Save as final JPG
image_write(img_final, path = "plots/comexp_CFA.jpg", format = "jpeg", quality = 100)

#Calculate latent scores
ISRD4$cfa_comexp <- as.data.frame(lavPredict(cfa_comexp_model))$cfa_comexp

#Convert latent scores to 0-1
ISRD4 <- ISRD4 %>%
  mutate(cfa_comexp = (cfa_comexp - min(cfa_comexp, na.rm = TRUE)) / 
           (max(cfa_comexp, na.rm = TRUE) - min(cfa_comexp, na.rm = TRUE)) * 10)

#Explore measures of parental control
#emotional support table(ISRD4$fambond3)
#know where I am when I go out ISRD4$aduknow1
#know what I am doing when I go out ISRD4$aduknow2 (not used)
#know what friends I am with ISRD4$aduknow3
#know what I do online ISRD4$aduknow4

#Recode NAs
ISRD4 <- ISRD4 %>%
  mutate(across(c("fambond3", "aduknow1", "aduknow2", "aduknow3", "aduknow4"),
                ~ recode(as.numeric(.x),
                         `9` = NA_real_,
                         `7` = NA_real_)))

#Estimate CFA model of parental control
cfa_parecnt <- 'cfa_parecnt = ~ fambond3 + aduknow1 + aduknow3 + aduknow4'

#Run CFA model (missingness option to full information maximum likelihood (FIML))
cfa_parecnt_model <- cfa(cfa_parecnt, missing = "fiml", data = ISRD4)

#Summary of CFA models
summary(cfa_parecnt_model, fit.measures = TRUE)
# CFI and TLI need to be larger than 0.9 (they are!) 
# CFI=0.978; TLI=0.935; RMSEA=0.079

# Custom labels
parecnt_labels <- c(
  cfa_parecnt = "Parental\ncontrol",
  fambond3 = "I can easily get emotional support\nand care from my parents",
  aduknow1 = "An adult at home knows where I am\nwhen I go out",
  aduknow3 = "An adult at home knows what I am\ndoing when I go out",
  aduknow4 = "An adult at home knows what I do\non the Internet"
)

# Extract model fit statistics
parecnt_indices <- fitMeasures(cfa_parecnt_model, c("cfi", "tli", "rmsea"))
parecnt_text <- sprintf("Model fit: CFI = %.3f   TLI = %.3f   RMSEA = %.3f",
                        parecnt_indices["cfi"], parecnt_indices["tli"], parecnt_indices["rmsea"])

# Generate the CFA diagram
parecnt_graph <- lavaanPlot(
  model = cfa_parecnt_model,
  stand = TRUE,
  coefs = TRUE,
  covs = FALSE,
  stars = c("regression"),
  labels = parecnt_labels,
  node_options = list(shape = "box", fontname = "Helvetica", color = "black"),
  edge_options = list(color = "black", fontname = "Helvetica", fontsize = 12),
  graph_options = list(rankdir = "LR")
)

# Export to SVG, then render as PNG
parecnt_svg_code <- DiagrammeRsvg::export_svg(parecnt_graph)
rsvg::rsvg_png(charToRaw(parecnt_svg_code), file = "plots/parecnt_CFA_body.png", width = 2400, height = 1600)

# Read diagram and flatten to white background (removes black box artefacts)
parecnt_img_diagram <- image_read("plots/parecnt_CFA_body.png") %>%
  image_background("white", flatten = TRUE)

# Add large "C" in top-left corner
parecnt_img_diagram_labeled <- image_annotate(
  parecnt_img_diagram,
  text = "C",
  size = 100,
  gravity = "northwest",
  location = "+60+40",
  font = "Helvetica",
  color = "black",
  weight = 700
)

# Create separate image for model fit measures
parecnt_img_text <- image_blank(width = 2400, height = 150, color = "white") %>%
  image_annotate(
    text = parecnt_text,
    size = 75,
    gravity = "center",
    font = "Helvetica",
    color = "black"
  )

# Combine diagram and fit stats vertically
img_final <- image_append(c(parecnt_img_diagram_labeled, parecnt_img_text), stack = TRUE)

# Save as final JPG
image_write(img_final, path = "plots/parecnt_CFA.jpg", format = "jpeg", quality = 100)

#Calculate latent scores
ISRD4$cfa_parecnt <- as.data.frame(lavPredict(cfa_parecnt_model))$cfa_parecnt

#Convert latent scores to 0-1
ISRD4 <- ISRD4 %>%
  mutate(cfa_parecnt = (cfa_parecnt - min(cfa_parecnt, na.rm = TRUE)) / 
           (max(cfa_parecnt, na.rm = TRUE) - min(cfa_parecnt, na.rm = TRUE)) * 10)

#Explore morality measures
#lie, disobey or talk back ISRD4$prosoc1 (not used)
#insult because of race table(ISRD4$prosoc2)
#damage property ISRD4$prosoc3
#share intimate images ISRD4$prosoc9
#steal something small ISRD4$prosoc5 (not used)
#hacking ISRD4$prosoc10
#hit someone ISRD4$prosoc7
#robbery ISRD4$prosoc8

#Recode NAs
ISRD4 <- ISRD4 %>%
  mutate(across(c("prosoc1", "prosoc2", "prosoc3", "prosoc9", "prosoc5",
                  "prosoc10", "prosoc7", "prosoc8"),
                ~ recode(as.numeric(.x),
                         `9` = NA_real_,
                         `7` = NA_real_)))

#Estimate CFA model of morality
cfa_moral <- 'cfa_moral = ~ prosoc2 + prosoc3 + prosoc9 + 
                            prosoc10 + prosoc7 + prosoc8'


#Run CFA model (missingness option to full information maximum likelihood (FIML))
cfa_moral_model <- cfa(cfa_moral, missing = "fiml", data = ISRD4)

#Summary of CFA models
summary(cfa_moral_model, fit.measures = TRUE)
# CFI and TLI need to be larger than 0.9 (they are!) 
# CFI=0.985; TLI=0.974; RMSEA=0.058

# Custom labels
moral_labels <- c(
  cfa_moral = "Morality",
  prosoc2 = "How wrong do you think is it for someone of your age to...\nKnowingly insult someone because of their race, ethnicity or\nnationality, religion, gender identity, sexual orientation, or\nfor similar reasons",
  prosoc3 = "...Purposely damage or destroy someone else’s property",
  prosoc9 = "...Share online an intimate photo or video of someone that\nhe or she did not want others to see",
  prosoc10 = "...Hack or break into a private account or computer to acquire\ndata, get control of an account, or destroy data",
  prosoc7 = "...Hit someone with the idea of hurting that person",
  prosoc8 = "...Use a weapon or force to get money or things from\nother people"
)

# Extract model fit statistics
moral_indices <- fitMeasures(cfa_moral_model, c("cfi", "tli", "rmsea"))
moral_text <- sprintf("Model fit: CFI = %.3f   TLI = %.3f   RMSEA = %.3f",
                      moral_indices["cfi"], moral_indices["tli"], moral_indices["rmsea"])

# Generate the CFA diagram
moral_graph <- lavaanPlot(
  model = cfa_moral_model,
  stand = TRUE,
  coefs = TRUE,
  covs = FALSE,
  stars = c("regression"),
  labels = moral_labels,
  node_options = list(shape = "box", fontname = "Helvetica", color = "black"),
  edge_options = list(color = "black", fontname = "Helvetica", fontsize = 12),
  graph_options = list(rankdir = "LR")
)

# Export to SVG, then render as PNG
moral_svg_code <- DiagrammeRsvg::export_svg(moral_graph)
rsvg::rsvg_png(charToRaw(moral_svg_code), file = "plots/moral_CFA_body.png", width = 2400, height = 1600)

# Read diagram and flatten to white background (removes black box artefacts)
moral_img_diagram <- image_read("plots/moral_CFA_body.png") %>%
  image_background("white", flatten = TRUE)

# Add large "D" in top-left corner
moral_img_diagram_labeled <- image_annotate(
  moral_img_diagram,
  text = "D",
  size = 100,
  gravity = "northwest",
  location = "+60+40",
  font = "Helvetica",
  color = "black",
  weight = 700
)

# Create separate image for model fit measures
moral_img_text <- image_blank(width = 2400, height = 150, color = "white") %>%
  image_annotate(
    text = moral_text,
    size = 75,
    gravity = "center",
    font = "Helvetica",
    color = "black"
  )

# Combine diagram and fit stats vertically
img_final <- image_append(c(moral_img_diagram_labeled, moral_img_text), stack = TRUE)

# Save as final JPG
image_write(img_final, path = "plots/moral_CFA.jpg", format = "jpeg", quality = 100)

#Calculate latent scores
ISRD4$cfa_moral <- as.data.frame(lavPredict(cfa_moral_model))$cfa_moral

#Convert latent scores to 0-1
ISRD4 <- ISRD4 %>%
  mutate(cfa_moral = (cfa_moral - min(cfa_moral, na.rm = TRUE)) / 
           (max(cfa_moral, na.rm = TRUE) - min(cfa_moral, na.rm = TRUE)) * 10)

#Explore credibility/openness
#would you be sincere in case of shoplifting table(ISRD4$openness)

#Explore sex
#table(ISRD4$male)

#Explore age
#table(ISRD4$age)
#table(ISRD4$age_im)

#Explore born in country
#table(ISRD4$birthpc)

#Explore perceived family deprivation
#table(ISRD4$deprfam)

# Descriptive statistics
vars <- c(
  "offending_bin", "offending", "unstr_st_outside", "unstr_st_home", "str_st_home",
  "delinq_peers_bin", "cfa_selfctr", "cfa_comexp", "cfa_parecnt", "cfa_moral",
  "openness", "male", "age_im", "birthpc", "deprfam",
  "off_damage", "off_damage_bin", "off_property", "off_property_bin",
  "off_violence", "off_violence_bin", "off_drugs", "off_drugs_bin",
  "off_cyber", "off_cyber_bin"
)

summarise_var <- function(var_name) {
  x <- ISRD4[[var_name]]
  data.frame(
    variable = var_name,
    min = fivenum(x)[1],
    q1 = fivenum(x)[2],
    median = fivenum(x)[3],
    q3 = fivenum(x)[4],
    max = fivenum(x)[5],
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}

map_dfr(vars, summarise_var)

#Estimate logit models
variables_null_model <- c("offending_bin", "unstr_st_outside", "unstr_st_home", "str_st_home",
                          "delinq_peers_bin", "cfa_selfctr", "cfa_comexp",
                          "cfa_parecnt", "cfa_moral", "openness", "male", "age_im", 
                          "birthpc", "deprfam", "scountry")
ISRD4_complete_null_model <- ISRD4[complete.cases(ISRD4[variables_null_model]), ]
null_model_bin <- glm(offending_bin ~ 1, family = "binomial", data = ISRD4_complete_null_model) #null model

ISRD4 <- ISRD4 %>%
  mutate(across(c(unstr_st_outside, unstr_st_home, str_st_home,
                  cfa_selfctr, cfa_comexp, cfa_parecnt,
                  cfa_moral, openness, age_im, deprfam),
                ~ scale(.) / 2, .names = "z_{.col}"))

model_off_bin <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam +
                           factor(scountry),
                         family = "binomial",
                         data = ISRD4)

#Print model estimates
summary(model_off_bin)
rsq(model_off_bin, type='v')
rsq(model_off_bin, type='n')
rsq(model_off_bin, type='kl')
rsq(model_off_bin, type='sse')
performance::r2_tjur(model_off_bin)
anova(null_model_bin, model_off_bin, test = "Chisq")
#tidy(model_off_bin, conf.int = TRUE, exponentiate = TRUE) %>%
#  print(n = 35)

# Clustered SEs by school_id
ISRD4$school_id <- as.factor(ISRD4$school_id)
model_off_bin_clust_se <- vcovCL(model_off_bin, cluster = ~school_id)
coeftest(model_off_bin, model_off_bin_clust_se)

#Estimate logistic regression models for each country
model_off_bin_arg <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Argentina",])

model_off_bin_aus <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Austria",])

model_off_bin_bos <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Bosnia Herz.",])

model_off_bin_bra <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Brazil",])

model_off_bin_col <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Colombia",])

model_off_bin_cze <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Czech Republic",])

model_off_bin_den <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Denmark",])

model_off_bin_est <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Estonia",])

model_off_bin_fin <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Finland",])

model_off_bin_ice <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Iceland",])

model_off_bin_lit <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Lithuania",])

model_off_bin_mex <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Mexico",])

model_off_bin_nor <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Norway",])

model_off_bin_pol <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Poland",])

model_off_bin_slo <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Slovenia",])

model_off_bin_spa <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Spain",])

model_off_bin_swe <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Sweden",])

model_off_bin_swi <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Switzerland",])

model_off_bin_uk <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "binomial",
                        data = ISRD4[ISRD4$scountry == "UK",])

model_off_bin_usa <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "USA",])

model_off_bin_ven <- glm(offending_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                           delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                           z_cfa_parecnt + z_cfa_moral + z_openness + 
                           male + z_age_im + birthpc + z_deprfam,
                         family = "binomial",
                         data = ISRD4[ISRD4$scountry == "Venezuela",])

#Predefined list of models for each country
models <- list(
  Argentina = model_off_bin_arg,
  Austria = model_off_bin_aus,
  Bosnia = model_off_bin_bos,
  Brazil = model_off_bin_bra,
  Colombia = model_off_bin_col,
  Czech = model_off_bin_cze,
  Denmark = model_off_bin_den,
  Estonia = model_off_bin_est,
  Finland = model_off_bin_fin,
  Iceland = model_off_bin_ice,
  Lithuania = model_off_bin_lit,
  Mexico = model_off_bin_mex,
  Norway = model_off_bin_nor,
  Poland = model_off_bin_pol,
  Slovenia = model_off_bin_slo,
  Spain = model_off_bin_spa,
  Sweden = model_off_bin_swe,
  Switzerland = model_off_bin_swi,
  UK = model_off_bin_uk,
  USA = model_off_bin_usa,
  Venezuela = model_off_bin_ven,
  Total = model_off_bin
)


#List of variables
variables <- c("z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home")

#Function to extract 95% odds ratios and pvalues
odds_ratios <- data.frame()

#(with clustered standard errors)
for (country in names(models)) {
  model <- models[[country]]
  
  # Clustered standard errors at the school level
  clustered_se <- vcovCL(model, cluster = ~school_id)
  test_results <- coeftest(model, vcov. = clustered_se)
  
  # Get coefficients, clustered SEs and p-values
  estimates <- test_results[variables, "Estimate"]
  se_clustered <- test_results[variables, "Std. Error"]
  p_values <- test_results[variables, "Pr(>|z|)"]
  
  # Calculate odds ratios and confidence intervals
  or <- exp(estimates)
  lower_ci <- exp(estimates - 1.96 * se_clustered)
  upper_ci <- exp(estimates + 1.96 * se_clustered)
  
  # Collect results
  temp_data <- data.frame(
    Country = country,
    Variable = variables,
    OddsRatio = or,
    LowerCI = lower_ci,
    UpperCI = upper_ci,
    PValue = p_values,
    Significance = p_values < 0.05
  )
  odds_ratios <- rbind(odds_ratios, temp_data)
}

#Create a new color column in your dataframe for point coloring based on significance
odds_ratios$PointColor <- ifelse(odds_ratios$Significance, as.character(odds_ratios$Variable), "grey")

#Ordering countries by OddsRatio
order_vector <- odds_ratios %>%
  filter(Variable == "z_unstr_st_outside") %>%
  arrange(OddsRatio) %>%
  pull(Country)

#Use factor to reorder Country
odds_ratios$Country <- factor(odds_ratios$Country, levels = order_vector)

#Custom labeling function to bold "Total"
custom_labels <- function(labels) {
  sapply(labels, function(label) {
    if (label == "Total") {
      bquote(bold(.(label)))
    } else {
      label
    }
  })
}

#Now plot using ggplot2, specifying separate aesthetics for points and error bars
glm_plot <- ggplot(odds_ratios, aes(x = OddsRatio, y = Country)) +
  geom_point(aes(color = PointColor), position = position_dodge(width = 0.25), size = 2) +  # Points colored based on significance
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, color = Variable), height = 0.1, position = position_dodge(width = 0.25)) +  # Error bars colored
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("grey", "#40B0A6", "#5D3A9B", "#E66100"),
                     labels = c("p≥0.05", "Structured spare time at home", "Unstructured spare time at home", "Unstructured out-of-home spare time"),
                     name = "",
                     guide = guide_legend(nrow = 2)) +
  theme_minimal() +
  labs(x = "Odds Ratio (95% CI)", y = "", title = "Binary logit models") +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 12),
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 12)
  ) +
  scale_y_discrete(labels = custom_labels(levels(odds_ratios$Country)))

#Table to compare effect sizes with other key variables
#(with clustered standard errors)
extract_and_classify <- function(model) {
  if (is.null(model)) {
    return(setNames(
      rep("Model not fitted", 6),
      c("z_unstr_st_outside", "z_cfa_selfctr", "z_cfa_collef", "z_cfa_comexp", "z_cfa_parecnt", "z_cfa_moral")
    ))
  }
  
  # Compute clustered SEs and p-values
  clustered_se <- sandwich::vcovCL(model, cluster = ~school_id)
  test_results <- lmtest::coeftest(model, vcov. = clustered_se)
  
  variables <- c("z_unstr_st_outside", "z_cfa_selfctr", "z_cfa_comexp", "z_cfa_parecnt", "z_cfa_moral", "delinq_peers_bin")
  
  # Check if all variables are in the model
  if (!all(variables %in% rownames(test_results))) {
    missing_vars <- variables[!variables %in% rownames(test_results)]
    return(setNames(rep("Variable not in model", length(missing_vars)), missing_vars))
  }
  
  pvals <- test_results[variables, "Pr(>|z|)"]
  adj_pvals <- pvals
  ref_effect_size <- abs(test_results["z_unstr_st_outside", "Estimate"])
  
  effects <- sapply(variables[-1], function(var) {
    coef <- test_results[var, "Estimate"]
    adj_pval <- adj_pvals[names(pvals) == var]
    effect_size <- abs(coef)
    
    if (adj_pval > 0.05) {
      "non-significant"
    } else {
      effect_strength <- if (effect_size > ref_effect_size) "stronger" else "weaker"
      
      if (coef > 0) {
        paste("positive significant", "(", effect_strength, "than outdoor spare time)")
      } else {
        paste("negative significant", "(", effect_strength, "than outdoor spare time)")
      }
    }
  }, USE.NAMES = TRUE)
  
  return(effects)
}

#Apply function to each model and bind results into a data frame
results_bin <- map_df(models, extract_and_classify, .id = "Country")
write.csv(results_bin, here('plots/results_bin.csv'))

#Estimate logit models for each crime type
##off_damage_bin, off_property_bin, off_violence_bin, off_drugs_bin, off_cyber_bin

#damage
variables_null_model_damage <- c("off_damage_bin", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                 "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                 "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                 "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_damage <- ISRD4[complete.cases(ISRD4[variables_null_model_damage]), ]
null_model_bin_damage <- glm(off_damage_bin ~ 1, family = "binomial", data = ISRD4_complete_null_model_damage) #null model

model_off_bin_damage <- glm(off_damage_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                              delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                              z_cfa_parecnt + z_cfa_moral + z_openness + 
                              male + z_age_im + birthpc + z_deprfam +
                              factor(scountry),
                            family = "binomial",
                            data = ISRD4)

#Check multicollinearity
vif(model_off_bin_damage)

#Print model estimates
summary(model_off_bin_damage)
rsq(model_off_bin_damage, type='v')
rsq(model_off_bin_damage, type='n')
rsq(model_off_bin_damage, type='kl')
rsq(model_off_bin_damage, type='sse')
anova(null_model_bin_damage, model_off_bin_damage, test = "Chisq")

#property
variables_null_model_property <- c("off_property_bin", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                   "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                   "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                   "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_property <- ISRD4[complete.cases(ISRD4[variables_null_model_property]), ]
null_model_bin_property <- glm(off_property_bin ~ 1, family = "binomial", data = ISRD4_complete_null_model_property) #null model

model_off_bin_property <- glm(off_property_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                                delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                                z_cfa_parecnt + z_cfa_moral + z_openness + 
                                male + z_age_im + birthpc + z_deprfam +
                                factor(scountry),
                              family = "binomial",
                              data = ISRD4)

#Check multicollinearity
vif(model_off_bin_property)

#Print model estimates
summary(model_off_bin_property)
rsq(model_off_bin_property, type='v')
rsq(model_off_bin_property, type='n')
rsq(model_off_bin_property, type='kl')
rsq(model_off_bin_property, type='sse')
anova(null_model_bin_property, model_off_bin_property, test = "Chisq")

#violence
variables_null_model_violence <- c("off_violence_bin", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                   "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                   "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                   "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_violence <- ISRD4[complete.cases(ISRD4[variables_null_model_violence]), ]
null_model_bin_violence <- glm(off_violence_bin ~ 1, family = "binomial", data = ISRD4_complete_null_model_violence) #null model

model_off_bin_violence <- glm(off_violence_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                                delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                                z_cfa_parecnt + z_cfa_moral + z_openness + 
                                male + z_age_im + birthpc + z_deprfam +
                                factor(scountry),
                              family = "binomial",
                              data = ISRD4)

#Check multicollinearity
vif(model_off_bin_violence)

#Print model estimates
summary(model_off_bin_violence)
rsq(model_off_bin_violence, type='v')
rsq(model_off_bin_violence, type='n')
rsq(model_off_bin_violence, type='kl')
rsq(model_off_bin_violence, type='sse')
anova(null_model_bin_violence, model_off_bin_violence, test = "Chisq")

#drugs
variables_null_model_drugs <- c("off_drugs_bin", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_drugs <- ISRD4[complete.cases(ISRD4[variables_null_model_drugs]), ]
null_model_bin_drugs <- glm(off_drugs_bin ~ 1, family = "binomial", data = ISRD4_complete_null_model_drugs) #null model

model_off_bin_drugs <- glm(off_drugs_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                             delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                             z_cfa_parecnt + z_cfa_moral + z_openness + 
                             male + z_age_im + birthpc + z_deprfam +
                             factor(scountry),
                           family = "binomial",
                           data = ISRD4)

#Check multicollinearity
vif(model_off_bin_drugs)

#Print model estimates
summary(model_off_bin_drugs)
rsq(model_off_bin_drugs, type='v')
rsq(model_off_bin_drugs, type='n')
rsq(model_off_bin_drugs, type='kl')
rsq(model_off_bin_drugs, type='sse')
anova(null_model_bin_drugs, model_off_bin_drugs, test = "Chisq")

#cyber
variables_null_model_cyber <- c("off_cyber_bin", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_cyber <- ISRD4[complete.cases(ISRD4[variables_null_model_cyber]), ]
null_model_bin_cyber <- glm(off_cyber_bin ~ 1, family = "binomial", data = ISRD4_complete_null_model_cyber) #null model

model_off_bin_cyber <- glm(off_cyber_bin ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                             delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                             z_cfa_parecnt + z_cfa_moral + z_openness + 
                             male + z_age_im + birthpc + z_deprfam +
                             factor(scountry),
                           family = "binomial",
                           data = ISRD4)

#Check multicollinearity
vif(model_off_bin_cyber)

#Print model estimates
summary(model_off_bin_cyber)
rsq(model_off_bin_cyber, type='v')
rsq(model_off_bin_cyber, type='n')
rsq(model_off_bin_cyber, type='kl')
rsq(model_off_bin_cyber, type='sse')
anova(null_model_bin_cyber, model_off_bin_cyber, test = "Chisq")

#Check dependent variable distributions
rootogram(goodfit(ISRD4$offending))
Ord_plot(ISRD4$offending)
distplot(ISRD4$offending, type="poisson")
distplot(ISRD4$offending, type="nbinom")
#offending: poisson

#Convert DV to integer
ISRD4 <- ISRD4 %>%
  mutate(offending.i = as.integer(offending)
  )

#Estimate poisson model
model_off <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                   delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                   z_cfa_parecnt + z_cfa_moral + z_openness + 
                   male + z_age_im + birthpc + z_deprfam,
                 family = "poisson",
                 data = ISRD4)

#Check under or overdispersion
deviance(model_off)/model_off$df.residual
dispersiontest(model_off)
#signs of overdispersion

#Estimate quasi-poisson model
variables_null_model <- c("offending.i", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                          "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                          "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                          "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model <- ISRD4[complete.cases(ISRD4[variables_null_model]), ]
model_off_qp_null <- glm(offending.i ~ 1, family = "quasipoisson", 
                         data = ISRD4_complete_null_model)

model_off_qp <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                      delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                      z_cfa_parecnt + z_cfa_moral + z_openness + 
                      male + z_age_im + birthpc + z_deprfam +
                      factor(scountry),
                    family = "quasipoisson",
                    data = ISRD4)

#Check multicollinearity
vif(model_off_qp)

#Print model estimates
summary(model_off_qp)
rsq(model_off_qp, type='v')
rsq(model_off_qp, type='kl')
rsq(model_off_qp, type='sse')
performance::r2_tjur(model_off_bin)
anova(model_off_qp_null, model_off_qp, test = "Chisq")
#tidy(model_off_qp, conf.int = TRUE, exponentiate = TRUE) %>%
#  print(n = 35)

# Clustered SEs by school_id
model_off_qp_clust_se <- vcovCL(model_off_qp, cluster = ~school_id)
coeftest(model_off_qp, model_off_qp_clust_se)

#Estimate quasi-poisson models for each country
model_off_qp_arg <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Argentina",])

model_off_qp_aus <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Austria",])

model_off_qp_bos <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Bosnia Herz.",])

model_off_qp_bra <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Brazil",])

model_off_qp_col <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Colombia",])

model_off_qp_cze <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Czech Republic",])

model_off_qp_den <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Denmark",])

model_off_qp_est <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Estonia",])

model_off_qp_fin <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Finland",])

model_off_qp_ice <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Iceland",])

model_off_qp_lit <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Lithuania",])

model_off_qp_mex <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Mexico",])

model_off_qp_nor <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Norway",])

model_off_qp_pol <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Poland",])

model_off_qp_slo <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Slovenia",])

model_off_qp_spa <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Spain",])

model_off_qp_swe <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Sweden",])

model_off_qp_swi <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Switzerland",])

model_off_qp_uk <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                         delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                         z_cfa_parecnt + z_cfa_moral + z_openness + 
                         male + z_age_im + birthpc + z_deprfam,
                       family = "quasipoisson",
                       data = ISRD4[ISRD4$scountry == "UK",])

model_off_qp_usa <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "USA",])

model_off_qp_ven <- glm(offending.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                          delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                          z_cfa_parecnt + z_cfa_moral + z_openness + 
                          male + z_age_im + birthpc + z_deprfam,
                        family = "quasipoisson",
                        data = ISRD4[ISRD4$scountry == "Venezuela",])

#Predefined list of models for each country
models <- list(
  Argentina = model_off_qp_arg,
  Austria = model_off_qp_aus,
  Bosnia = model_off_qp_bos,
  Brazil = model_off_qp_bra,
  Colombia = model_off_qp_col,
  Czech = model_off_qp_cze,
  Denmark = model_off_qp_den,
  Estonia = model_off_qp_est,
  Finland = model_off_qp_fin,
  Iceland = model_off_qp_ice,
  Lithuania = model_off_qp_lit,
  Mexico = model_off_qp_mex,
  Norway = model_off_qp_nor,
  Poland = model_off_qp_pol,
  Slovenia = model_off_qp_slo,
  Spain = model_off_qp_spa,
  Sweden = model_off_qp_swe,
  Switzerland = model_off_qp_swi,
  UK = model_off_qp_uk,
  USA = model_off_qp_usa,
  Venezuela = model_off_qp_ven,
  Total = model_off_qp
)

#List of variables
variables <- c("z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home")

#Function to extract 95% odds ratios
odds_ratios <- data.frame()

#(with clustered standard errors)
for (country in names(models)) {
  model <- models[[country]]
  
  # Skip iteration if model is NULL
  if (is.null(model)) next
  
  # Use clustered standard errors at the school level
  clustered_se <- vcovCL(model, cluster = ~school_id)
  test_results <- coeftest(model, vcov. = clustered_se)
  
  # Check if all required variables are present
  if (!all(variables %in% rownames(test_results))) {
    missing_vars <- variables[!variables %in% rownames(test_results)]
    warning(sprintf("Variables missing in model for %s: %s", country, paste(missing_vars, collapse = ", ")))
    next
  }
  
  # Extract estimates and clustered SEs
  estimates <- test_results[variables, "Estimate"]
  se_clustered <- test_results[variables, "Std. Error"]
  p_values <- test_results[variables, "Pr(>|z|)"]
  
  # Calculate odds ratios and CIs
  or <- exp(estimates)
  lower_ci <- exp(estimates - 1.96 * se_clustered)
  upper_ci <- exp(estimates + 1.96 * se_clustered)
  
  # Collect results
  temp_data <- data.frame(
    Country = country,
    Variable = variables,
    OddsRatio = or,
    LowerCI = lower_ci,
    UpperCI = upper_ci,
    PValue = p_values,
    Significance = p_values < 0.05
  )
  
  odds_ratios <- rbind(odds_ratios, temp_data)
}

#Create a new color column for point coloring based on significance
odds_ratios$PointColor <- ifelse(odds_ratios$Significance, as.character(odds_ratios$Variable), "grey")

#Ordering countries by OddsRatio
order_vector <- odds_ratios %>%
  filter(Variable == "z_unstr_st_outside") %>%
  arrange(OddsRatio) %>%
  pull(Country)  # pull()

#Reorder Country based on order_vector
odds_ratios$Country <- factor(odds_ratios$Country, levels = order_vector)

#Now plot using ggplot2, specifying separate aesthetics for points and error bars
qp_plot <- ggplot(odds_ratios, aes(x = OddsRatio, y = Country)) +
  geom_point(aes(color = PointColor), position = position_dodge(width = 0.25), size = 2) +  # Points colored based on significance
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, color = Variable), height = 0.1, position = position_dodge(width = 0.25)) +  # Error bars colored by variable
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("grey", "#40B0A6", "#5D3A9B", "#E66100"),
                     labels = c("p≥0.05", "Structured spare time at home", "Unstructured spare time at home", "Unstructured out-of-home spare time"),
                     name = "",
                     guide = guide_legend(nrow = 2)) +
  theme_minimal() +
  labs(x = "Incident Rate Ratio (95% CI)", y = "", title = "Quasi-Poisson models") +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 12),
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 12)
  ) +
  scale_y_discrete(labels = custom_labels(levels(odds_ratios$Country)))
qp_plot

#Print both plots together
ggarrange(glm_plot, qp_plot, ncol = 2,
          common.legend = TRUE, legend = "bottom")

ggsave(here('plots/models_by_country.jpg'), width = 7.5, height = 5)

#Table to compare effect sizes with other key variables
#(with clustered standard errors)
extract_and_classify <- function(model) {
  if (is.null(model)) {
    return(setNames(
      rep("Model not fitted", 6),
      c("z_unstr_st_outside", "z_cfa_selfctr", "z_cfa_collef", "z_cfa_comexp", "z_cfa_parecnt", "z_cfa_moral")
    ))
  }
  
  # Compute clustered standard errors at school level
  clustered_se <- sandwich::vcovCL(model, cluster = ~school_id)
  test_results <- lmtest::coeftest(model, vcov. = clustered_se)
  
  variables <- c("z_unstr_st_outside", "z_cfa_selfctr", "z_cfa_comexp", "z_cfa_parecnt", "z_cfa_moral", "delinq_peers_bin")
  
  # Check if all variables are in the model
  if (!all(variables %in% rownames(test_results))) {
    missing_vars <- variables[!variables %in% rownames(test_results)]
    return(setNames(rep("Variable not in model", length(missing_vars)), missing_vars))
  }
  
  # Extract p-values and coefficients
  pvals <- test_results[variables, "Pr(>|z|)"]
  adj_pvals <- pvals
  ref_effect_size <- abs(test_results["z_unstr_st_outside", "Estimate"])
  
  effects <- sapply(variables[-1], function(var) {  # Exclude 'unstr_st_outside'
    coef <- test_results[var, "Estimate"]
    adj_pval <- adj_pvals[names(pvals) == var]
    effect_size <- abs(coef)
    
    if (adj_pval > 0.05) {
      "non-significant"
    } else {
      effect_strength <- if (effect_size > ref_effect_size) "stronger" else "weaker"
      direction <- if (coef > 0) "positive" else "negative"
      paste(direction, "significant", "(", effect_strength, "than outdoor spare time)")
    }
  }, USE.NAMES = TRUE)
  
  return(effects)
}

#Apply function to each model and bind results into a data frame
results_qp <- map_df(models, extract_and_classify, .id = "Country")
write.csv(results_qp, here('plots/results_qp.csv'))

#Estimate quasi poisson models for each crime type
##off_damage_bin, off_property_bin, off_violence_bin, off_drugs_bin, off_cyber_bin

#Convert DV to integer
ISRD4 <- ISRD4 %>%
  mutate(off_damage.i = as.integer(off_damage),
         off_property.i = as.integer(off_property),
         off_violence.i = as.integer(off_violence),
         off_drugs.i = as.integer(off_drugs),
         off_cyber.i = as.integer(off_cyber))

#damage
variables_null_model_damage <- c("off_damage.i", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                 "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                 "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                 "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_damage <- ISRD4[complete.cases(ISRD4[variables_null_model_damage]), ]
model_off_qp_null_damage <- glm(off_damage.i ~ 1, family = "quasipoisson", 
                                data = ISRD4_complete_null_model_damage)

model_off_qp_damage <- glm(off_damage.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                             delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                             z_cfa_parecnt + z_cfa_moral + z_openness + 
                             male + z_age_im + birthpc + z_deprfam +
                             factor(scountry),
                           family = "quasipoisson",
                           data = ISRD4)

#Check multicollinearity
vif(model_off_qp_damage)

#Print model estimates
summary(model_off_qp_damage)
rsq(model_off_qp_damage, type='v')
rsq(model_off_qp_damage, type='kl')
rsq(model_off_qp_damage, type='sse')
performance::r2_tjur(model_off_bin_damage)
anova(model_off_qp_null_damage, model_off_qp_damage, test = "Chisq")

#property
variables_null_model_property <- c("off_property.i", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                   "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                   "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                   "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_property <- ISRD4[complete.cases(ISRD4[variables_null_model_property]), ]
model_off_qp_null_property <- glm(off_property.i ~ 1, family = "quasipoisson", 
                                  data = ISRD4_complete_null_model_property)

model_off_qp_property <- glm(off_property.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                               delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                               z_cfa_parecnt + z_cfa_moral + z_openness + 
                               male + z_age_im + birthpc + z_deprfam +
                               factor(scountry),
                             family = "quasipoisson",
                             data = ISRD4)

#Check multicollinearity
vif(model_off_qp_property)

#Print model estimates
summary(model_off_qp_property)
rsq(model_off_qp_property, type='v')
rsq(model_off_qp_property, type='kl')
rsq(model_off_qp_property, type='sse')
performance::r2_tjur(model_off_bin_property)
anova(model_off_qp_null_property, model_off_qp_property, test = "Chisq")

#violence
variables_null_model_violence <- c("off_violence.i", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                   "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                   "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                   "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_violence <- ISRD4[complete.cases(ISRD4[variables_null_model_violence]), ]
model_off_qp_null_violence <- glm(off_violence.i ~ 1, family = "quasipoisson", 
                                  data = ISRD4_complete_null_model_violence)

model_off_qp_violence <- glm(off_violence.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                               delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                               z_cfa_parecnt + z_cfa_moral + z_openness + 
                               male + z_age_im + birthpc + z_deprfam +
                               factor(scountry),
                             family = "quasipoisson",
                             data = ISRD4)

#Check multicollinearity
vif(model_off_qp_violence)

#Print model estimates
summary(model_off_qp_violence)
rsq(model_off_qp_violence, type='v')
rsq(model_off_qp_violence, type='kl')
rsq(model_off_qp_violence, type='sse')
performance::r2_tjur(model_off_bin_violence)
anova(model_off_qp_null_violence, model_off_qp_violence, test = "Chisq")

#drugs
variables_null_model_drugs <- c("off_drugs.i", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_drugs <- ISRD4[complete.cases(ISRD4[variables_null_model_drugs]), ]
model_off_qp_null_drugs <- glm(off_drugs.i ~ 1, family = "quasipoisson", 
                               data = ISRD4_complete_null_model_drugs)

model_off_qp_drugs <- glm(off_drugs.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                            delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                            z_cfa_parecnt + z_cfa_moral + z_openness + 
                            male + z_age_im + birthpc + z_deprfam +
                            factor(scountry),
                          family = "quasipoisson",
                          data = ISRD4)

#Check multicollinearity
vif(model_off_qp_drugs)

#Print model estimates
summary(model_off_qp_drugs)
rsq(model_off_qp_drugs, type='v')
rsq(model_off_qp_drugs, type='kl')
rsq(model_off_qp_drugs, type='sse')
performance::r2_tjur(model_off_bin_drugs)
anova(model_off_qp_null_drugs, model_off_qp_drugs, test = "Chisq")

#cyber
variables_null_model_cyber <- c("off_cyber.i", "z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home",
                                "delinq_peers_bin", "z_cfa_selfctr", "z_cfa_comexp",
                                "z_cfa_parecnt", "z_cfa_moral", "z_openness", "male", "z_age_im", 
                                "birthpc", "z_deprfam", "scountry")
ISRD4_complete_null_model_cyber <- ISRD4[complete.cases(ISRD4[variables_null_model_cyber]), ]
model_off_qp_null_cyber <- glm(off_cyber.i ~ 1, family = "quasipoisson", 
                               data = ISRD4_complete_null_model_cyber)

model_off_qp_cyber <- glm(off_cyber.i ~ z_unstr_st_outside + z_unstr_st_home + z_str_st_home +
                            delinq_peers_bin + z_cfa_selfctr + z_cfa_comexp +
                            z_cfa_parecnt + z_cfa_moral + z_openness + 
                            male + z_age_im + birthpc + z_deprfam +
                            factor(scountry),
                          family = "quasipoisson",
                          data = ISRD4)

#Check multicollinearity
vif(model_off_qp_cyber)

#Print model estimates
summary(model_off_qp_cyber)
rsq(model_off_qp_cyber, type='v')
rsq(model_off_qp_cyber, type='kl')
rsq(model_off_qp_cyber, type='sse')
performance::r2_tjur(model_off_bin_cyber)
anova(model_off_qp_null_cyber, model_off_qp_cyber, test = "Chisq")

# List of models for each crime type (logit and quasi-Poisson)
logit_models <- list(
  Damage = model_off_bin_damage,
  Property = model_off_bin_property,
  Violence = model_off_bin_violence,
  Drugs = model_off_bin_drugs,
  Cyber = model_off_bin_cyber
)

qp_models <- list(
  Damage = model_off_qp_damage,
  Property = model_off_qp_property,
  Violence = model_off_qp_violence,
  Drugs = model_off_qp_drugs,
  Cyber = model_off_qp_cyber
)

variables <- c("z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home")

# Function to extract odds ratios
#(with clustered standard errors)
extract_odds_ratios <- function(model_list, model_type = "logit") {
  library(sandwich)
  library(lmtest)
  
  odds_ratios <- data.frame()
  
  for (crime in names(model_list)) {
    model <- model_list[[crime]]
    
    if (is.null(model)) next
    
    # Compute clustered standard errors at the school level
    clustered_se <- vcovCL(model, cluster = ~school_id)
    test_results <- coeftest(model, vcov. = clustered_se)
    
    # Skip if required variables are missing
    if (!all(variables %in% rownames(test_results))) {
      missing_vars <- variables[!variables %in% rownames(test_results)]
      warning(sprintf("Variables missing in model for %s: %s", crime, paste(missing_vars, collapse = ", ")))
      next
    }
    
    estimates <- test_results[variables, "Estimate"]
    se_clustered <- test_results[variables, "Std. Error"]
    p_values <- test_results[variables, "Pr(>|z|)"]
    
    or <- exp(estimates)
    lower_ci <- exp(estimates - 1.96 * se_clustered)
    upper_ci <- exp(estimates + 1.96 * se_clustered)
    
    odds_ratios <- rbind(odds_ratios, data.frame(
      CrimeType = crime,
      Variable = variables,
      OddsRatio = or,
      LowerCI = lower_ci,
      UpperCI = upper_ci,
      PValue = p_values,
      Significance = p_values < 0.05,
      Model = model_type
    ))
  }
  
  return(odds_ratios)
}

# Apply to both sets of models
logit_df <- extract_odds_ratios(logit_models, model_type = "Binary logit models")
qp_df <- extract_odds_ratios(qp_models, model_type = "Quasi-Poisson models")

# Combine
combined_df <- rbind(logit_df, qp_df)

# Label time use variables
combined_df$Variable <- factor(combined_df$Variable,
                               levels = c("z_unstr_st_outside", "z_unstr_st_home", "z_str_st_home"),
                               labels = c("Unstructured ST outside", "Unstructured ST at home", "Structured ST at home"))

# Reorder CrimeType by effect size of Unstructured ST outside
order_levels <- logit_df %>%
  filter(Variable == "z_unstr_st_outside") %>%
  arrange(OddsRatio) %>%
  pull(CrimeType)

combined_df$CrimeType <- factor(combined_df$CrimeType, levels = order_levels)
combined_df$Model <- factor(combined_df$Model, levels = c("Binary logit models", "Quasi-Poisson models"))

# Colour assignment: grey if not significant
combined_df$Colour <- ifelse(combined_df$PValue < 0.05,
                             as.character(combined_df$Variable),
                             "Not Significant")

# Colour palette
colour_palette <- c(
  "Unstructured ST outside" = "#E66100",
  "Unstructured ST at home" = "#5D3A9B",
  "Structured ST at home" = "#40B0A6",
  "Not Significant" = "grey70"
)

# Colour labels
colour_labels <- c(
  "Not Significant" = "p ≥ 0.05",
  "Structured ST at home" = "Structured spare time at home",
  "Unstructured ST at home" = "Unstructured spare time at home",
  "Unstructured ST outside" = "Unstructured out-of-home spare time"
)

# Plot
ggplot(combined_df, aes(x = OddsRatio, y = CrimeType, color = Colour)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~Model) +
  labs(x = "Effect size (OR for prevalence, IRR for incidence; 95% CI)", y = "Crime type", title = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11)
  ) +
  scale_color_manual(values = colour_palette,
                     labels = colour_labels,
                     guide = guide_legend(nrow = 2))

ggsave(here('plots/models_by_crimetype.jpg'), width = 7.5, height = 5)
write.csv(combined_df, here("plots/odds_ratio_crime_types.csv"))

#Formula for both logit and quasi-Poisson models using unstandardised predictors
formula_offending_bin <- offending_bin ~ unstr_st_outside + unstr_st_home + str_st_home +
  delinq_peers_bin + cfa_selfctr + cfa_comexp + cfa_parecnt + cfa_moral + openness +
  male + age_im + birthpc + deprfam

formula_offending_i <- offending.i ~ unstr_st_outside + unstr_st_home + str_st_home +
  delinq_peers_bin + cfa_selfctr + cfa_comexp + cfa_parecnt + cfa_moral + openness +
  male + age_im + birthpc + deprfam

#Countries to loop over
countries <- c(
  "Argentina", "Austria", "Bosnia Herz.", "Brazil", "Colombia", "Czech Republic",
  "Denmark", "Estonia", "Finland", "Iceland", "Lithuania", "Mexico", "Norway",
  "Poland", "Slovenia", "Spain", "Sweden", "Switzerland", "UK", "USA", "Venezuela"
)

# Initialise model lists
models_bin_unstd <- list()
models_qp_unstd <- list()

# Loop through each country and fit models
for (country in countries) {
  message("Fitting models for: ", country)
  
  data_country <- ISRD4[ISRD4$scountry == country, ]
  
  # Logistic regression
  models_bin_unstd[[country]] <- glm(formula_offending_bin,
                                     family = "binomial",
                                     data = data_country)
  
  # Quasi-Poisson regression
  models_qp_unstd[[country]] <- glm(formula_offending_i,
                                    family = "quasipoisson",
                                    data = data_country)
}

# Updated list of models (Logit and Quasi-Poisson)
models_bin <- list(
  Argentina = models_bin_unstd$Argentina,
  Austria = models_bin_unstd$Austria,
  "Bosnia Herz." = models_bin_unstd$`Bosnia Herz.`,
  Brazil = models_bin_unstd$Brazil,
  Colombia = models_bin_unstd$Colombia,
  "Czech Republic" = models_bin_unstd$`Czech Republic`,
  Denmark = models_bin_unstd$Denmark,
  Estonia = models_bin_unstd$Estonia,
  Finland = models_bin_unstd$Finland,
  Iceland = models_bin_unstd$Iceland,
  Lithuania = models_bin_unstd$Lithuania,
  Mexico = models_bin_unstd$Mexico,
  Norway = models_bin_unstd$Norway,
  Poland = models_bin_unstd$Poland,
  Slovenia = models_bin_unstd$Slovenia,
  Spain = models_bin_unstd$Spain,
  Sweden = models_bin_unstd$Sweden,
  Switzerland = models_bin_unstd$Switzerland,
  UK = models_bin_unstd$UK,
  USA = models_bin_unstd$USA,
  Venezuela = models_bin_unstd$Venezuela
)

models_qp <- list(
  Argentina = models_qp_unstd$Argentina,
  Austria = models_qp_unstd$Austria,
  "Bosnia Herz." = models_qp_unstd$`Bosnia Herz.`,
  Brazil = models_qp_unstd$Brazil,
  Colombia = models_qp_unstd$Colombia,
  "Czech Republic" = models_qp_unstd$`Czech Republic`,
  Denmark = models_qp_unstd$Denmark,
  Estonia = models_qp_unstd$Estonia,
  Finland = models_qp_unstd$Finland,
  Iceland = models_qp_unstd$Iceland,
  Lithuania = models_qp_unstd$Lithuania,
  Mexico = models_qp_unstd$Mexico,
  Norway = models_qp_unstd$Norway,
  Poland = models_qp_unstd$Poland,
  Slovenia = models_qp_unstd$Slovenia,
  Spain = models_qp_unstd$Spain,
  Sweden = models_qp_unstd$Sweden,
  Switzerland = models_qp_unstd$Switzerland,
  UK = models_qp_unstd$UK,
  USA = models_qp_unstd$USA,
  Venezuela = models_qp_unstd$Venezuela
)

# Function to perform bootstrapping for binary models
bootstrap_effects_bin_all <- function(model, data, max_reduction = 20, n_boot = 500) {
  proportion_pred_offending <- function(data, indices, reduction) {
    data_boot <- data[indices, ]
    data_boot$unstr_st_outside <- data_boot$unstr_st_outside * (1 - reduction / 100)
    predicted_offending <- predict(model, newdata = data_boot, type = "response")
    binary_predicted_offending <- ifelse(predicted_offending >= 0.5, 1, 0)
    return(mean(binary_predicted_offending, na.rm = TRUE))
  }
  
  baseline_offending <- predict(model, newdata = data, type = "response")
  binary_baseline_offending <- ifelse(baseline_offending >= 0.5, 1, 0)
  proportion_baseline_offending <- mean(binary_baseline_offending, na.rm = TRUE)
  
  results <- data.frame(Reduction = numeric(0), Percent_Reduction_Offending = numeric(0), Country = character(0))
  
  for (reduction in 1:max_reduction) {
    cat("Simulating reduction:", reduction, "%\n")
    boot_results <- boot(data, proportion_pred_offending, R = n_boot, reduction = reduction)
    mean_proportion_predicted_offending <- mean(boot_results$t)
    percent_reduction_offending <- -100 * (proportion_baseline_offending - mean_proportion_predicted_offending) / proportion_baseline_offending
    results <- rbind(results, data.frame(Reduction = reduction, Percent_Reduction_Offending = percent_reduction_offending, Country = data$scountry[1]))
  }
  
  return(results)
}

# Function to perform bootstrapping for quasi-Poisson models
bootstrap_effects_qp_all <- function(model, data, max_reduction = 20, n_boot = 500) {
  sum_pred_offending <- function(data, indices, reduction) {
    data_boot <- data[indices, ]
    data_boot$unstr_st_outside <- data_boot$unstr_st_outside * (1 - reduction / 100)
    predicted_offending <- predict(model, newdata = data_boot, type = "response")
    return(sum(predicted_offending, na.rm = TRUE))
  }
  
  baseline_offending <- predict(model, newdata = data, type = "response")
  sum_baseline_offending <- sum(baseline_offending, na.rm = TRUE)
  
  results <- data.frame(Reduction = numeric(0), Percent_Reduction_Offending = numeric(0), Country = character(0))
  
  for (reduction in 1:max_reduction) {
    cat("Simulating reduction:", reduction, "%\n")
    boot_results <- boot(data, sum_pred_offending, R = n_boot, reduction = reduction)
    mean_predicted_offending <- mean(boot_results$t)
    percent_reduction_offending <- -100 * (sum_baseline_offending - mean_predicted_offending) / sum_baseline_offending
    results <- rbind(results, data.frame(Reduction = reduction, Percent_Reduction_Offending = percent_reduction_offending, Country = data$scountry[1]))
  }
  
  return(results)
}

# Combine and simulate results for all binary models
all_results_boots_bin_all <- data.frame()
for (country in names(models_bin)) {
  cat("Processing country:", country, "\n")
  model <- models_bin[[country]]
  data <- ISRD4[ISRD4$scountry == country, ]
  country_results <- bootstrap_effects_bin_all(model, data)
  all_results_boots_bin_all <- rbind(all_results_boots_bin_all, country_results)
}

# Combine and simulate results for all quasi-Poisson models
all_results_boots_qp_all <- data.frame()
for (country in names(models_qp)) {
  cat("Processing country:", country, "\n")
  model <- models_qp[[country]]
  data <- ISRD4[ISRD4$scountry == country, ]
  country_results <- bootstrap_effects_qp_all(model, data)
  all_results_boots_qp_all <- rbind(all_results_boots_qp_all, country_results)
}

# Add baseline (0% reduction) row
sim_0 <- data.frame(Reduction = rep(0, 21), 
                    Percent_Reduction_Offending = rep(0, 21), 
                    Country = names(models_bin))

all_results_boots_bin_all <- rbind(all_results_boots_bin_all, sim_0)
all_results_boots_qp_all <- rbind(all_results_boots_qp_all, sim_0)

#Save results
write.csv(all_results_boots_bin_all, here('plots/all_results_boots_bin_all.csv'))
write.csv(all_results_boots_qp_all, here('plots/all_results_boots_qp_all.csv'))

#all_results_boots_bin_all <- read.csv(here('plots/all_results_boots_bin_all.csv'))
#all_results_boots_bin_all <- all_results_boots_bin_all %>% select(-X)

#all_results_boots_qp_all <- read.csv(here('plots/all_results_boots_qp_all.csv'))
#all_results_boots_qp_all <- all_results_boots_qp_all %>% select(-X)

# Plotting

# Step 1: Filter and reshape to wide format
heatmap_data_bin <- all_results_boots_bin_all %>%
  filter(Country != "Total") %>%
  mutate(Reduction = as.integer(Reduction)) %>%
  pivot_wider(names_from = Reduction, values_from = Percent_Reduction_Offending)

heatmap_data_qp <- all_results_boots_qp_all %>%
  filter(Country != "Total") %>%
  mutate(Reduction = as.integer(Reduction)) %>%
  pivot_wider(names_from = Reduction, values_from = Percent_Reduction_Offending)

# Step 2: Define alphabetical country order
country_order_alpha <- heatmap_data_bin %>%
  distinct(Country) %>%
  arrange(Country) %>%
  pull(Country)

# Step 3: Convert back to long format, applying alphabetical row order
heatmap_long_bin <- heatmap_data_bin %>%
  pivot_longer(-Country, names_to = "Reduction", values_to = "Reduction_Value") %>%
  mutate(
    Reduction = as.integer(Reduction),
    Country = factor(Country, levels = rev(country_order_alpha))
  )

heatmap_long_qp <- heatmap_data_qp %>%
  pivot_longer(-Country, names_to = "Reduction", values_to = "Reduction_Value") %>%
  mutate(
    Reduction = as.integer(Reduction),
    Country = factor(Country, levels = rev(country_order_alpha))
  )

# Get the common range for both heatmaps
common_limits <- range(c(heatmap_long_bin$Reduction_Value, heatmap_long_qp$Reduction_Value), na.rm = TRUE)

# Apply same limits in both plots
heatmap_bin <- ggplot(heatmap_long_bin, aes(x = factor(Reduction), y = Country, fill = Reduction_Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0,
                       limits = common_limits,
                       name = "% change offending",
                       guide = guide_colourbar(direction = "horizontal", reverse = TRUE)) +
  labs(x = "% Reduction in Unstructured Spare Time", y = "",
       title = "Simulated Change in Offending Prevalence") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

heatmap_qp <- ggplot(heatmap_long_qp, aes(x = factor(Reduction), y = Country, fill = Reduction_Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0,
                       limits = common_limits,
                       name = "% change offending",
                       guide = guide_colourbar(direction = "horizontal", reverse = TRUE)) +
  labs(x = "% Reduction in Unstructured Spare Time", y = "",
       title = "Simulated Change in Offending Incidence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

# Combine plots
ggarrange(heatmap_bin, heatmap_qp, nrow = 2,
          common.legend = TRUE, legend = "bottom")

ggsave(here('plots/simulation_heatmap_by_country.jpg'), width = 6, height = 9)
