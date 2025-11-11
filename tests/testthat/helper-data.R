# Base data with multiple variables for ids, strata, weights, fpc,
# and several variables to pivot.
make_basic_df <- function() {
  n <- 12 # Increased from 8 to 12 for more variety

  tibble::tibble(
    # Core identifiers
    id = 1:n,
    id2 = rep(1:6, times = 2),

    # Groups / strata
    grp = rep(c("A", "B", "C"), each = n / 3),
    strata = rep(1:3, each = n / 3),
    strata2 = rep(c("X", "Y", "Z"), times = n / 3),

    psu = rep(1:6, each = 2), # 6 PSUs, each with 2 sampled SSUs
    ssu = 1:n, # SSU id per row (1..12)

    # Define fpc values aligned to stages
    fpc_psu = rep(15L, n), # 15 PSUs in population, sampled 6
    fpc_ssu = rep(8L, n), # 8 SSUs per PSU in population, sampled 2

    # Weights
    wts = c(1, 2, 1, 1, 1, 3, 1, 2, 2, 1, 3, 1),
    w2 = rep(c(1.1, 0.9, 1.2), each = n / 3),

    # Demographics for realistic survey context
    age = c(25, 34, 42, 56, 23, 67, 31, 45, 52, 28, 39, 61),
    income = c(
      45000,
      67000,
      52000,
      89000,
      34000,
      72000,
      58000,
      94000,
      61000,
      41000,
      76000,
      83000
    ),

    # Likert scale questions (1-5 scale) - these are great for pivoting
    satisfaction_service = c(4, 5, 3, 2, 5, 4, 3, 4, 5, 2, 4, 3),
    satisfaction_price = c(3, 4, 2, 1, 4, 5, 3, 3, 4, 1, 3, 2),
    satisfaction_quality = c(5, 5, 4, 3, 5, 4, 4, 5, 5, 3, 4, 4),
    satisfaction_support = c(4, 3, 3, 2, 4, 5, 2, 4, 3, 2, 5, 3),

    # Agreement scale questions (1-7 scale)
    agree_recommend = c(6, 7, 5, 3, 7, 6, 4, 6, 7, 2, 6, 5),
    agree_repurchase = c(5, 6, 4, 2, 6, 7, 4, 5, 6, 1, 5, 4),
    agree_trust = c(6, 6, 5, 4, 6, 7, 5, 6, 6, 3, 6, 5),

    # Frequency questions (numeric - times per month)
    freq_use_product = c(12, 8, 15, 3, 20, 6, 10, 14, 18, 2, 11, 7),
    freq_visit_store = c(2, 4, 1, 0, 3, 1, 2, 3, 4, 0, 2, 1),
    freq_contact_support = c(0, 1, 0, 2, 0, 0, 1, 0, 1, 3, 1, 0),

    # Binary yes/no questions
    x1 = c(
      "yes",
      "no",
      "yes",
      NA,
      "no",
      "no",
      "yes",
      "no",
      "yes",
      "no",
      "yes",
      "no"
    ),
    x2 = c(
      "yes",
      "yes",
      "no",
      "no",
      "yes",
      "no",
      "no",
      NA,
      "yes",
      "no",
      "no",
      "yes"
    ),

    # Categorical questions
    x3 = c(1L, 2L, 1L, 2L, 1L, 2L, NA, 1L, 2L, 1L, 2L, 1L),

    # Logical questions
    x4 = c(
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      NA,
      TRUE,
      FALSE,
      TRUE,
      FALSE
    ),

    # Rating scales (0-10)
    rating_overall = c(8, 9, 7, 5, 9, 8, 6, 8, 9, 4, 8, 7),
    rating_value = c(7, 8, 6, 3, 8, 9, 6, 7, 8, 2, 7, 6),
    rating_experience = c(8, 8, 7, 6, 8, 9, 7, 8, 8, 5, 8, 7)
  )
}

make_expanded_df <- function() {
  set.seed(123) # ensures reproducibility

  n <- 250 # total rows

  # Base 12-row patterns to repeat
  base_pattern <- tibble::tibble(
    grp = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C"),
    strata = rep(1:3, each = 4),
    strata2 = rep(c("X", "Y", "Z", "X"), 3),
    x1 = c(
      "yes",
      "no",
      "yes",
      NA,
      "no",
      "no",
      "yes",
      "no",
      "yes",
      "no",
      "yes",
      "no"
    ),
    x2 = c(
      "yes",
      "yes",
      "no",
      "no",
      "yes",
      "no",
      "no",
      NA,
      "yes",
      "no",
      "no",
      "yes"
    ),
    x3 = c(1L, 2L, 1L, 2L, 1L, 2L, NA, 1L, 2L, 1L, 2L, 1L),
    x4 = c(
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      NA,
      TRUE,
      FALSE,
      TRUE,
      FALSE
    ),
    satisfaction_service = c(4, 5, 3, 2, 5, 4, 3, 4, 5, 2, 4, 3),
    satisfaction_price = c(3, 4, 2, 1, 4, 5, 3, 3, 4, 1, 3, 2),
    satisfaction_quality = c(5, 5, 4, 3, 5, 4, 4, 5, 5, 3, 4, 4),
    satisfaction_support = c(4, 3, 3, 2, 4, 5, 2, 4, 3, 2, 5, 3),
    agree_recommend = c(6, 7, 5, 3, 7, 6, 4, 6, 7, 2, 6, 5),
    agree_repurchase = c(5, 6, 4, 2, 6, 7, 4, 5, 6, 1, 5, 4),
    agree_trust = c(6, 6, 5, 4, 6, 7, 5, 6, 6, 3, 6, 5),
    freq_use_product = c(12, 8, 15, 3, 20, 6, 10, 14, 18, 2, 11, 7),
    freq_visit_store = c(2, 4, 1, 0, 3, 1, 2, 3, 4, 0, 2, 1),
    freq_contact_support = c(0, 1, 0, 2, 0, 0, 1, 0, 1, 3, 1, 0),
    rating_overall = c(8, 9, 7, 5, 9, 8, 6, 8, 9, 4, 8, 7),
    rating_value = c(7, 8, 6, 3, 8, 9, 6, 7, 8, 2, 7, 6),
    rating_experience = c(8, 8, 7, 6, 8, 9, 7, 8, 8, 5, 8, 7)
  )

  # Repeat patterns to reach n rows
  reps <- ceiling(n / nrow(base_pattern))
  df <- base_pattern[rep(1:nrow(base_pattern), reps), ]
  df <- df[1:n, ]

  # Add unique identifiers
  df$id <- 1:n
  df$id2 <- rep(1:(n / 2), each = 2)[1:n]

  # PSU and SSU
  df$psu <- rep(1:50, each = 5)[1:n] # 50 PSUs, 5 SSUs each
  df$ssu <- 1:n

  # FPC values
  df$fpc_psu <- rep(60L, n) # assume 60 PSUs in population
  df$fpc_ssu <- rep(20L, n) # assume 20 SSUs per PSU in population

  # Randomized numeric values (age, income) based on original means
  df$age <- round(rnorm(n, mean = 40, sd = 12))
  df$income <- round(rnorm(n, mean = 60000, sd = 18000))

  # Randomized weights
  df$wts <- round(runif(n, 0.5, 3), 2)
  df$w2 <- round(runif(n, 0.8, 1.2), 2)

  df
}


# Enhanced labeling function
label_vars <- function(df) {
  # Original labels for x1..x4
  attr(df$x1, "label") <- "Q1. Blue"
  attr(df$x2, "label") <- "Q2. Red"
  attr(df$x3, "label") <- "Q3. Count"
  attr(df$x4, "label") <- "Q4. Flag"

  # Satisfaction questions
  attr(df$satisfaction_service, "label") <- "Satisfaction with Service"
  attr(df$satisfaction_price, "label") <- "Satisfaction with Price"
  attr(df$satisfaction_quality, "label") <- "Satisfaction with Quality"
  attr(df$satisfaction_support, "label") <- "Satisfaction with Support"

  # Agreement questions
  attr(df$agree_recommend, "label") <- "Would Recommend"
  attr(df$agree_repurchase, "label") <- "Would Repurchase"
  attr(df$agree_trust, "label") <- "Trust the Brand"

  # Frequency questions
  attr(df$freq_use_product, "label") <- "Product Usage Frequency"
  attr(df$freq_visit_store, "label") <- "Store Visit Frequency"
  attr(df$freq_contact_support, "label") <- "Support Contact Frequency"

  # Rating questions
  attr(df$rating_overall, "label") <- "Overall Rating"
  attr(df$rating_value, "label") <- "Value for Money Rating"
  attr(df$rating_experience, "label") <- "Experience Rating"

  # Value labels for different question types

  # Binary yes/no
  attr(df$x1, "labels") <- c("Yes" = "yes", "No" = "no")
  attr(df$x2, "labels") <- c("Yes" = "yes", "No" = "no")

  # Categorical
  attr(df$x3, "labels") <- c("One" = 1, "Two" = 2)

  # Logical
  attr(df$x4, "labels") <- c("True" = TRUE, "False" = FALSE)

  satisfaction_labels <- c(
    "Very Dissatisfied" = 1,
    "Dissatisfied" = 2,
    "Neutral" = 3,
    "Satisfied" = 4,
    "Very Satisfied" = 5
  )

  attr(df$satisfaction_service, "labels") <- satisfaction_labels
  attr(df$satisfaction_price, "labels") <- satisfaction_labels
  attr(df$satisfaction_quality, "labels") <- satisfaction_labels
  attr(df$satisfaction_support, "labels") <- satisfaction_labels

  # Agreement scale (1-7)
  agreement_labels <- c(
    "Strongly Disagree" = 1,
    "Disagree" = 2,
    "Somewhat Disagree" = 3,
    "Neutral" = 4,
    "Somewhat Agree" = 5,
    "Agree" = 6,
    "Strongly Agree" = 7
  )
  attr(df$agree_recommend, "labels") <- agreement_labels
  attr(df$agree_repurchase, "labels") <- agreement_labels
  attr(df$agree_trust, "labels") <- agreement_labels

  # Rating scale (0-10)
  rating_labels <- setNames(as.character(0:10), as.character(0:10))
  attr(df$rating_overall, "labels") <- rating_labels
  attr(df$rating_value, "labels") <- rating_labels
  attr(df$rating_experience, "labels") <- rating_labels

  # Question prefaces for different question groups
  satisfaction_preface <- "Please rate your satisfaction with each of the following aspects:"
  attr(df$satisfaction_service, "question_preface") <- satisfaction_preface
  attr(df$satisfaction_price, "question_preface") <- satisfaction_preface
  attr(df$satisfaction_quality, "question_preface") <- satisfaction_preface
  attr(df$satisfaction_support, "question_preface") <- satisfaction_preface

  agreement_preface <- "Please indicate how much you agree or disagree with each statement:"
  attr(df$agree_recommend, "question_preface") <- agreement_preface
  attr(df$agree_repurchase, "question_preface") <- agreement_preface
  attr(df$agree_trust, "question_preface") <- agreement_preface

  frequency_preface <- "How often do you do each of the following (per month):"
  attr(df$freq_use_product, "question_preface") <- frequency_preface
  attr(df$freq_visit_store, "question_preface") <- frequency_preface
  attr(df$freq_contact_support, "question_preface") <- frequency_preface

  rating_preface <- "Please rate each of the following on a scale from 0 to 10:"
  attr(df$rating_overall, "question_preface") <- rating_preface
  attr(df$rating_value, "question_preface") <- rating_preface
  attr(df$rating_experience, "question_preface") <- rating_preface

  # Original binary question preface
  qpref <- "Please indicate which of the following colors you like. Select all that apply:"
  attr(df$x1, "question_preface") <- qpref
  attr(df$x2, "question_preface") <- qpref

  df
}

# Helper function to create replicate weights for testing
make_rep_weights <- function(n = 8, nrep = 4) {
  # Create simple replicate weights matrix
  rep_weights <- matrix(runif(n * nrep, 0.5, 1.5), nrow = n, ncol = nrep)
  colnames(rep_weights) <- paste0("rep_", 1:nrep)
  return(rep_weights)
}

# Small survey design
make_svy <- function(df) {
  survey::svydesign(ids = ~1, weights = ~wts, data = df)
}
