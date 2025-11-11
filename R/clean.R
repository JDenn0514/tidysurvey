#' Clean and augment model output
#'
#' A simplified alternative to `broom::tidy()` that adds confidence intervals,
#' optional reference levels, and sample sizes for each term.
#'
#' @param model A fitted model object (e.g., `lm`, `glm`, or `svyglm`).
#' @param conf_level Confidence level for intervals (default = 0.95).
#' @param include_reference Logical; whether to include reference levels for
#'   categorical predictors.
#' @param conf_method Determines whether the confidence intervals are calculated
#'   using the profile likelihood or the Wald method. Obviously has two options,
#'   "profile" and "wald". Wald is between 3 to 25 times as fast but not as
#'   reliable for small sample sizes (n < 50). For larger sample sizes,
#'   (n > 100), they will be very similar. **The default is Wald.**
#'
#' @return A tibble of coefficients with confidence intervals and counts.
#' @keywords internal
clean <- function(
  model,
  conf_level = 0.95,
  include_reference = FALSE,
  conf_method = c("wald", "profile")
) {
  # Extract coefficient table as tibble
  coefs <- summary(model)$coefficients %>%
    tibble::as_tibble(rownames = "term")

  # calculate CIs
  cis <- calc_ci(model, conf_level = conf_level, method = conf_method)
  # combine CIs with table of coefficients
  coefs <- dplyr::left_join(coefs, cis, by = "term")

  # Add sample sizes for each term
  term_n <- get_term_n(model, include_reference = include_reference)
  coefs$n <- term_n[coefs$term]

  # If including reference levels, add them to the results
  if (include_reference) {
    reference_info <- get_reference_levels(
      model,
      if (inherits(model, "svyglm")) {
        model$survey.design$variables
      } else {
        model$data %||% eval(model$call$data, envir = parent.frame(2))
      }
    )

    if (length(reference_info$terms) > 0) {
      # Create reference rows with matching column names in the same order
      ref_rows <- tibble::tibble(
        term = reference_info$terms
      )

      # Add all other columns in the same order as coefs
      for (col in names(coefs)) {
        if (col == "term") {
          next # Already added
        } else if (col == "Estimate") {
          ref_rows[[col]] <- 0
        } else if (col == "n") {
          ref_rows[[col]] <- reference_info$n_values[reference_info$terms]
        } else {
          ref_rows[[col]] <- NA_real_
        }
      }

      # Combine
      coefs <- dplyr::bind_rows(coefs, ref_rows)
    }
  }

  coefs <- model_reorder(model, coefs)

  attr(coefs, "se") <- coefs$`Std. Error`

  # Determine which p-value column exists and what test was used
  p_col <- NULL
  test_type <- NULL

  if ("Pr(>|t|)" %in% names(coefs)) {
    p_col <- "Pr(>|t|)"
    test_type <- "t"
  } else if ("Pr(>|z|)" %in% names(coefs)) {
    p_col <- "Pr(>|z|)"
    test_type <- "z"
  }

  # Select and rename columns
  coefs <- coefs[c(
    "term",
    "Estimate",
    "n",
    "conf_low",
    "conf_high",
    p_col
  )]

  names(coefs)[names(coefs) == p_col] <- "p_value"
  names(coefs)[names(coefs) == "Estimate"] <- "estimate"

  # Add attribute for test type
  attr(coefs, "test_type") <- test_type

  coefs
}


#' Reorder model terms to ensure reference levels appear first
#'
#' Orders the rows of a coefficient table so that reference levels for
#' categorical predictors appear before other levels within each term.
#'
#' @param model A fitted model object.
#' @param coefs A tibble or data frame of model coefficients.
#'
#' @return A reordered version of `coefs`.
#' @keywords internal
model_reorder <- function(model, coefs) {
  mf <- stats::model.frame(model)
  terms_obj <- stats::terms(model)
  term_labels <- attr(terms_obj, "term.labels")
  intercept_present <- attr(terms_obj, "intercept") == 1

  final_order <- character()

  # Add intercept first
  if (intercept_present && any("(Intercept)" == coefs$term)) {
    final_order <- c(final_order, "(Intercept)")
  }

  # Process each term in the model
  for (term in term_labels) {
    vars <- all.vars(stats::as.formula(paste("~", term)))
    vars_in_mf <- intersect(vars, names(mf))

    # Check which variables are categorical (factor or character)
    # Fix: Use vapply instead of sapply to ensure we get a logical vector
    if (length(vars_in_mf) > 0) {
      categorical_vars <- vars_in_mf[vapply(
        vars_in_mf,
        function(v) {
          is.factor(mf[[v]]) || is.character(mf[[v]])
        },
        FUN.VALUE = logical(1)
      )]
    } else {
      categorical_vars <- character(0)
    }

    numeric_vars <- setdiff(vars_in_mf, categorical_vars)

    # Case 1: Single categorical variable
    if (length(categorical_vars) == 1 && length(numeric_vars) == 0) {
      v <- categorical_vars
      var <- mf[[v]]

      # Get levels (convert to factor if character)
      if (is.character(var)) {
        levs <- sort(unique(var))
      } else {
        levs <- levels(var)
      }

      # Add reference level first, then other levels
      for (lv in levs) {
        term_name <- paste0(v, lv)
        if (any(term_name == coefs$term)) {
          final_order <- c(final_order, term_name)
        }
      }
    } else if (length(categorical_vars) > 1 && length(numeric_vars) == 0) {
      # Case 2: Interaction of multiple categorical variables
      # Get all level combinations
      levs_list <- lapply(categorical_vars, function(v) {
        var <- mf[[v]]
        if (is.character(var)) {
          sort(unique(var))
        } else {
          levels(var)
        }
      })

      # Create all combinations
      combos <- expand.grid(
        levs_list,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )

      # Generate term names for each combination
      for (i in 1:nrow(combos)) {
        term_name <- paste0(
          mapply(paste0, categorical_vars, combos[i, ]),
          collapse = ":"
        )
        if (any(term_name == coefs$term)) {
          final_order <- c(final_order, term_name)
        }
      }
    } else if (length(categorical_vars) == 1 && length(numeric_vars) == 1) {
      # Case 3: Numeric × Categorical interaction
      v <- categorical_vars
      cont <- numeric_vars
      var <- mf[[v]]

      # Get levels
      if (is.character(var)) {
        levs <- sort(unique(var))
      } else {
        levs <- levels(var)
      }

      # Add interaction terms in level order
      for (lv in levs) {
        # Try both orders of interaction term
        term_name1 <- paste0(cont, ":", v, lv)
        term_name2 <- paste0(v, lv, ":", cont)

        if (any(term_name1 == coefs$term)) {
          final_order <- c(final_order, term_name1)
        } else if (any(term_name2 == coefs$term)) {
          final_order <- c(final_order, term_name2)
        }
      }
    } else {
      # Case 4: Pure numeric or other terms (polynomials, splines, etc.)
      # For transformed terms, just match terms that contain the term label
      matching_terms <- coefs$term[grepl(term, coefs$term, fixed = TRUE)]
      final_order <- c(final_order, matching_terms)
    }
  }

  # Remove duplicates while preserving order
  final_order <- unique(final_order)

  # Reorder coefs based on final_order
  ordered_coefs <- coefs[match(final_order, coefs$term), , drop = FALSE]
  ordered_coefs <- ordered_coefs[!is.na(ordered_coefs$term), , drop = FALSE]

  # Add any remaining terms not yet included
  remaining <- coefs[!any(coefs$term == ordered_coefs$term), , drop = FALSE]
  if (nrow(remaining) > 0) {
    ordered_coefs <- rbind(ordered_coefs, remaining)
  }

  rownames(ordered_coefs) <- NULL
  ordered_coefs
}


#' Get the number of observations for each model term
#'
#' Returns the number of observations contributing to each coefficient,
#' including optional reference levels.
#'
#' @param model A fitted model object (e.g., `lm`, `glm`, or `svyglm`).
#' @param include_reference Logical; whether to include reference levels.
#'
#' @return A named integer vector with term names as names.
#' @keywords internal
get_term_n <- function(model, include_reference = FALSE) {
  # --- Get model data ---
  if (inherits(model, "svyglm")) {
    model_data <- model$survey.design$variables
  } else {
    model_data <- model$data
    if (is.null(model_data)) {
      model_data <- tryCatch(
        eval(model$call$data, envir = parent.frame(2)),
        error = function(e) stop("Could not retrieve model data.")
      )
    }
  }

  # --- Get model matrix and coefficients ---
  mm <- stats::model.matrix(model)
  coef_names <- names(stats::coef(model))
  total_n <- nrow(model_data)

  # --- Initialize result vector ---
  n_values <- setNames(rep(NA_integer_, length(coef_names)), coef_names)

  # --- Intercept ---
  if (any("(Intercept)" == coef_names)) {
    n_values["(Intercept)"] <- total_n
  }

  # --- Non-intercept terms ---
  for (coef_name in setdiff(coef_names, "(Intercept)")) {
    if (any(coef_name == colnames(mm))) {
      mm_col <- mm[, coef_name]

      # Handle standard dummy or centered contrasts
      if (all(mm_col %in% c(0, 1))) {
        n_values[coef_name] <- sum(mm_col)
      } else if (all(mm_col %in% c(-1, 0, 1))) {
        # sum absolute values in case of sum contrasts or Helmert coding
        n_values[coef_name] <- sum(abs(mm_col) > 0)
      } else {
        # Continuous predictors or complex transforms
        n_values[coef_name] <- total_n
      }
    } else {
      # Aliased or transformed terms (not in matrix)
      n_values[coef_name] <- total_n
    }
  }

  # --- Include reference levels if requested ---
  if (include_reference) {
    reference_info <- get_reference_levels(model, model_data)
    n_values <- c(n_values, reference_info$n_values)
  }

  return(n_values)
}


#' Extract reference level terms and counts
#'
#' Identifies reference levels for factor predictors in a model and
#' returns their names and observation counts.
#'
#' @param model A fitted model object.
#' @param model_data The data used to fit the model.
#'
#' @return A list with elements `terms` (character vector of reference
#'   terms) and `n_values` (named integer vector of counts).
#' @keywords internal
get_reference_levels <- function(model, model_data) {
  mf <- stats::model.frame(model)

  reference_terms <- character()
  reference_n <- integer()

  # Get model coefficients to validate against
  coef_names <- names(stats::coef(model))

  # Loop through all factor OR CHARACTER variables in the model frame
  for (v in names(mf)) {
    var <- mf[[v]]

    # Skip if not factor or character
    if (!is.factor(var) && !is.character(var)) {
      next
    }

    # Convert character to factor temporarily to work with it
    if (is.character(var)) {
      var <- factor(var)
    }

    # Determine reference level based on contrasts
    ref_level <- get_factor_reference(var, v, model)
    ref_term_name <- paste0(v, ref_level)
    ref_count <- sum(mf[[v]] == ref_level, na.rm = TRUE) # Use original variable for counting

    reference_terms <- c(reference_terms, ref_term_name)
    reference_n <- c(reference_n, ref_count)
  }

  # Handle factor × factor interactions explicitly
  term_labels <- attr(stats::terms(model), "term.labels")
  interaction_terms <- term_labels[grepl(":", term_labels)]

  for (term_label in interaction_terms) {
    vars <- all.vars(stats::as.formula(paste("~", term_label)))

    # Only check variables that actually exist in model frame
    vars <- intersect(vars, names(mf))

    if (length(vars) == 0) {
      next
    }

    # Check for factor OR character variables
    factor_vars <- vars[vapply(
      vars,
      function(v) {
        is.factor(mf[[v]]) || is.character(mf[[v]])
      },
      FUN.VALUE = logical(1)
    )]

    if (length(factor_vars) > 1) {
      # Get reference levels for each factor considering contrasts
      levs_list <- lapply(factor_vars, function(v) {
        var <- mf[[v]]
        if (is.character(var)) {
          var <- factor(var)
        }
        get_factor_reference(var, v, model)
      })

      combo <- paste0(mapply(paste0, factor_vars, levs_list), collapse = ":")
      logical_mask <- Reduce(
        `&`,
        Map(function(v, lv) mf[[v]] == lv, factor_vars, levs_list)
      )
      ref_count <- sum(logical_mask, na.rm = TRUE)

      reference_terms <- c(reference_terms, combo)
      reference_n <- c(reference_n, ref_count)
    }
  }

  # Validate reference terms against model coefficients
  validated <- validate_reference_terms(reference_terms, coef_names, mf)

  names(reference_n) <- reference_terms

  list(
    terms = reference_terms,
    n_values = reference_n,
    validation = validated
  )
}


# Helper function to determine reference level based on contrasts
get_factor_reference <- function(factor_var, var_name, model) {
  # Get the contrast matrix for this factor
  contrast_matrix <- tryCatch(
    stats::contrasts(factor_var),
    error = function(e) NULL
  )

  # If no contrast matrix, fall back to first level
  if (is.null(contrast_matrix)) {
    return(levels(factor_var)[1])
  }

  all_levels <- levels(factor_var)

  # Handle ordered factors explicitly
  if (is.ordered(factor_var)) {
    # For ordered factors with polynomial contrasts (default),
    # the reference is still typically the first level
    # But we check which level is NOT in the coefficient names
    return(all_levels[1])
  }

  # For treatment contrasts (most common), reference is the level
  # NOT appearing in the row names of the contrast matrix
  coded_levels <- rownames(contrast_matrix)

  # The reference level is the one not in the coded levels
  # (for treatment contrasts, this is the baseline)
  reference <- setdiff(all_levels, coded_levels)

  if (length(reference) == 1) {
    return(reference)
  }

  # If we can't determine unambiguously, check the contrast type
  contrast_attr <- attr(contrast_matrix, "contrasts")

  # For sum contrasts, all levels appear in coding
  # In this case, we need different logic
  if (!is.null(contrast_attr) && contrast_attr == "contr.sum") {
    # With sum contrasts, typically the last level is the reference
    return(all_levels[length(all_levels)])
  }

  # Default: return first level
  return(all_levels[1])
}

# Helper function to validate reference terms against model coefficients
validate_reference_terms <- function(reference_terms, coef_names, mf) {
  validation <- list()

  for (ref_term in reference_terms) {
    # A reference term should NOT appear in coefficient names
    # (because it's the baseline that other levels are compared against)
    appears_in_coefs <- any(ref_term == coef_names)

    # Extract the variable name(s) from the reference term
    if (grepl(":", ref_term)) {
      # Interaction term
      parts <- strsplit(ref_term, ":")[[1]]
      var_names <- unique(sapply(parts, function(p) {
        # Extract variable name by removing the level value
        # Match the variable name from the model frame
        for (v in names(mf)) {
          if (startsWith(p, v)) {
            return(v)
          }
        }
        return(NA_character_)
      }))
      var_names <- var_names[!is.na(var_names)]

      # For interactions, check if ANY coefficient includes these variables
      any_coef_with_vars <- length(var_names) > 0 &&
        any(sapply(var_names, function(vn) {
          any(grepl(paste0("^", vn), coef_names))
        }))

      validation[[ref_term]] <- list(
        is_valid = !appears_in_coefs && any_coef_with_vars,
        appears_in_coefs = appears_in_coefs,
        note = if (!appears_in_coefs && any_coef_with_vars) {
          "Valid reference (not in coefficients)"
        } else if (appears_in_coefs) {
          "WARNING: Reference term appears in coefficients - may not be true reference"
        } else {
          "WARNING: Variable not found in model coefficients"
        }
      )
    } else {
      # Simple term - extract variable name
      # Find which model frame variable this term starts with
      var_name <- NA_character_
      for (v in names(mf)) {
        if (startsWith(ref_term, v)) {
          var_name <- v
          break
        }
      }

      # Check if this variable appears in any coefficient
      any_coef_with_var <- !is.na(var_name) &&
        any(grepl(paste0("^", var_name), coef_names))

      validation[[ref_term]] <- list(
        is_valid = !appears_in_coefs && any_coef_with_var,
        appears_in_coefs = appears_in_coefs,
        note = if (!appears_in_coefs && any_coef_with_var) {
          "Valid reference (not in coefficients)"
        } else if (appears_in_coefs) {
          "WARNING: Reference term appears in coefficients - may not be true reference"
        } else {
          "WARNING: Variable not found in model coefficients"
        }
      )
    }
  }

  validation
}

# df_svy <- make_expanded_df() |>
#   label_vars() |>
#   dplyr::mutate(
#     satisfaction_service = make_factor(satisfaction_service)
#   ) |>
#   srvyr::as_survey_design(
#     ids = id,
#     strata = strata,
#     weights = "wts",
#     .data = _
#   )

# model_svy <- survey::svyglm(
#   satisfaction_price ~ grp + satisfaction_service * freq_use_product,
#   df_svy
# ) |>
#   broom::tidy() |>
#   dplyr::pull(term)

# clean(model_svy, include_reference = TRUE)
