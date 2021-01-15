## Simulate death record data with multiple causes of death
require(tidyverse)
dir("./prep/raw_data")

# smoking
load("./prep/raw_data/Data_clean_smoking_20200930.RData")


risk_smoke <- left_join(
    select(df_p, age, sex, year, city, region, prev, prev_l, prev_u, nsam, csam),
    select(df_x, age, sex, year, city, region, mean_x, sd_x, x_pth0, x_pth100)
  ) %>%
  left_join(
    select(df_z, age, sex, year, city, region, mean_z, sd_z, z_pth0, z_pth100)
  ) %>%
  select(age, sex, year, city, region, nsam, csam, prev, prev_l, prev_u,
         mean_x, sd_x, x_pth0, x_pth100,
         mean_z, sd_z, z_pth0, z_pth100)


# drinking
load("./prep/raw_data/Data_clean_drinking_20200930.RData")

risk_drink <- left_join(
  select(df_p, age, sex, year, city, region, prev, prev_l, prev_u, nsam, csam),
  select(df_x, age, sex, year, city, region, mean_x, sd_x, x_pth0, x_pth100)
  )  %>%
  select(age, sex, year, city, region, nsam, csam, prev, prev_l, prev_u,
         mean_x, sd_x, x_pth0, x_pth100)


usethis::use_data(risk_smoke, overwrite = T)
usethis::use_data(risk_drink, overwrite = T)



# 3H
load("./prep/raw_data/Data_R3_20201015.RData")


risk_3H <- bind_rows(
  mutate(df_bp, risk = "BP"),
  mutate(df_gluc, risk = "Glucose"),
  mutate(df_LDL, risk = "LDL")
) %>%
  select(
    risk, age, sex, year, city, region, mean_x, sd_x, nsam
  )

usethis::use_data(risk_3H, overwrite = T)

