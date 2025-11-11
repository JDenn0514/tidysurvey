## code to prepare `DATASET` dataset goes here
df <- make_expanded_df() |> label_vars()

df_svy <- srvyr::as_survey_design(
  df,
  ids = id,
  strata = strata,
  weights = "wts",
  .data = _
)

usethis::use_data(df, overwrite = TRUE)
