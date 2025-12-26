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
    )
  )

write.csv(dat_comp, "dat_comp.csv", row.names = FALSE)
