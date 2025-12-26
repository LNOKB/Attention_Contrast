library(readr)
library(dplyr)
library(stringr)

# merge data
files <- list.files("subdata", pattern = "\\.csv$", full.names = TRUE)

dat_detect <- files[str_detect(basename(files), "_detect_")] |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  mutate(

    response = case_when(
      key_resp.keys == "num_1" ~ 1,
      key_resp.keys == "num_3" ~ 0,
      TRUE ~ NA_real_
    ),
    
    cuetype = case_when(
      precue == 0 ~ 0,                                 # uncued
      precue == postcue ~ 1,                           # cued
      precue != 0 & precue != postcue ~ -1             # anti-cued
    ), 
    
    contrast = case_when(
      contrast == 12.1 ~ 0.8,
      contrast == 15.7 ~ 1.2,
      contrast == 19.6 ~ 1.8,
      contrast == 22.7 ~ 2.5,
      contrast == 25.5 ~ 3.2,
      contrast == 29.4 ~ 4.1,
      contrast == 35.3 ~ 6.6,
      contrast == 39.2 ~ 8.3,
      contrast == 42.7 ~ 10.5,
      TRUE ~ NA_real_
    ),
    
    effective_contrast = case_when(
      postcue == -1 & lefttarget  == 1 ~ contrast,
      postcue == -1 & lefttarget  == 0 ~ 0,
      postcue ==  1 & righttarget == 1 ~ contrast,
      postcue ==  1 & righttarget == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

write.csv(dat_detect, "dat_detect.csv", row.names = FALSE)

# dat_comp <- files[str_detect(basename(files), "_comp_")] |>
#   lapply(read_csv, show_col_types = FALSE) |>
#   bind_rows()
# write.csv(dat_comp, "dat_comp.csv")

dat_comp <- files[str_detect(basename(files), "_comp_")] |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  mutate(
    cuetype = case_when(
      precue == 0 ~ 0,
      precue == side ~ 1,
      precue != 0 & precue != side ~ -1,
      TRUE ~ NA_real_
    ),
    
    select_test = case_when(
      key_resp.keys == "left"  & side == -1 ~ 1,
      key_resp.keys == "left"  & side ==  1 ~ 0,
      key_resp.keys == "right" & side == -1 ~ 0,
      key_resp.keys == "right" & side ==  1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    contrast = case_when(
      contrast == 12.1 ~ 0.8,
      contrast == 15.7 ~ 1.2,
      contrast == 19.6 ~ 1.8,
      contrast == 22.7 ~ 2.5,
      contrast == 25.5 ~ 3.2,
      contrast == 29.4 ~ 4.1,
      contrast == 35.3 ~ 6.6,
      contrast == 39.2 ~ 8.3,
      contrast == 42.7 ~ 10.5,
      TRUE ~ NA_real_
    )
  )

write.csv(dat_comp, "dat_comp.csv", row.names = FALSE)
