library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)


files <- list.files("subdata", pattern = "\\.csv$", full.names = TRUE)

dat_detect <- files[str_detect(basename(files), "_detect_")]
reshape_trial_data <- function(df) {
  # 左側の刺激データを作成
  df_left <- df %>%
    mutate(
      contrast = leftcontrast,
      response = case_when(
        key_resp.keys == "num_1" ~ 0,
        key_resp.keys == "num_4" ~ 1,
        TRUE ~ NA_real_
      ),
      rt = key_resp.rt,
      side = "left",
      cue_type = case_when(
        precue == -1 ~ 1,   # left is cued when precue = -1
        precue == 1 ~ -1,   # left is anti-cued when precue = 1
        precue == 0 ~ 0     # left is uncued when precue = 0
      )
    ) %>%
    select(contrast, response, rt, cue_type, side, precue,
           trials.thisRepN, trials.thisTrialN, trials.thisN,
           participant, session, date)
  
  # 右側の刺激データを作成
  df_right <- df %>%
    mutate(
      contrast = rightcontrast,
      response = case_when(
        key_resp_3.keys == "num_3" ~ 0,
        key_resp_3.keys == "num_6" ~ 1,
        TRUE ~ NA_real_
      ),
      rt = key_resp_3.rt,
      side = "right",
      cue_type = case_when(
        precue == -1 ~ -1,  # right is anti-cued when precue = -1
        precue == 1 ~ 1,    # right is cued when precue = 1
        precue == 0 ~ 0     # right is uncued when precue = 0
      )
    ) %>%
    select(contrast, response, rt, cue_type, side, precue,
           trials.thisRepN, trials.thisTrialN, trials.thisN,
           participant, session, date)
  
  # 結合してソート
  bind_rows(df_left, df_right) %>%
    arrange(trials.thisN, desc(side))
}

# すべてのファイルを読み込んで展開
dat_detect_reshaped <- dat_detect %>%
  map_dfr(~{
    cat("Processing:", .x, "\n")
    read_csv(.x, show_col_types = FALSE) %>%
      reshape_trial_data()
  })

write_csv(dat_detect_reshaped, "dat_detect.csv")

#################
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
