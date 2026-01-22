library(readr)
library(dplyr)
library(stringr)
library(purrr)

files <- list.files("subdata", pattern = "\\.csv$", full.names = TRUE)
dat_detect_files <- files[str_detect(basename(files), "_detect_")]

reshape_trial_data <- function(df) {
  
  # 1) まず「存在したら」session と ...列を消す（ここが肝）
  df <- df %>%
    select(-any_of("session")) %>%                 # sessionがあれば消す
    select(-matches("^\\.\\.\\.[0-9]+$"))          # ...43 なども消す
  
  # left
  df_left <- df %>%
    transmute(
      participant = participant,
      date = date,
      trials.thisN = trials.thisN,
      trials.thisTrialN = trials.thisTrialN,
      trials.thisRepN = trials.thisRepN,
      
      side = "left",
      contrast = leftcontrast,
      
      response = case_when(
        key_resp.keys == "num_1" ~ 0,
        key_resp.keys == "num_4" ~ 1,
        TRUE ~ NA_real_
      ),
      rt = key_resp.rt,
      
      cue_type = case_when(
        precue == -1 ~  1,
        precue ==  1 ~ -1,
        precue ==  0 ~  0,
        TRUE ~ NA_real_
      ),
      precue = precue
    )
  
  # right
  df_right <- df %>%
    transmute(
      participant = participant,
      date = date,
      trials.thisN = trials.thisN,
      trials.thisTrialN = trials.thisTrialN,
      trials.thisRepN = trials.thisRepN,
      
      side = "right",
      contrast = rightcontrast,
      
      response = case_when(
        key_resp_3.keys == "num_3" ~ 0,
        key_resp_3.keys == "num_6" ~ 1,
        TRUE ~ NA_real_
      ),
      rt = key_resp_3.rt,
      
      cue_type = case_when(
        precue == -1 ~ -1,
        precue ==  1 ~  1,
        precue ==  0 ~  0,
        TRUE ~ NA_real_
      ),
      precue = precue
    )
  
  bind_rows(df_left, df_right) %>%
    arrange(trials.thisN, side)
}

dat_detect_reshaped <- map_dfr(dat_detect_files, \(f) {
  cat("Processing:", f, "\n")
  read_csv(f, show_col_types = FALSE) %>%
    reshape_trial_data()
})

write_csv(dat_detect_reshaped, "dat_detect.csv")


#################
comp_files <- files[str_detect(basename(files), "_comp_")]
dat_comp <- map_dfr(comp_files, \(f){
  cat("Processing:", f, "\n")
  read_csv(f, show_col_types = FALSE) %>%
    # ここが肝：sessionがあれば消す / ...38 も消す
    select(-any_of("session")) %>%
    select(-matches("^\\.\\.\\.[0-9]+$")) %>%
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
})

write.csv(dat_comp, "dat_comp.csv", row.names = FALSE)
