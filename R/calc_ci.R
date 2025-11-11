#' Calculate confidence intervals for lm, glm, and svyglm
#'
#' This function calculates confidence intervals for `lm`, `glm`,
#' `svyglm`, and `svystat` objects. The `svyglm`and `svystat` methods
#' are based on code from the`survey` package. In addition, there are
#' two methods: wald and profile likelihood.
#'
#' @param model The model/data object
#' @param conf_level The confidence interval, default is 0.95
#' @param method The method for calculating CIs
#'
#' @keywords internal
calc_ci <- function(
  model,
  conf_level = 0.95,
  method = c("wald", "profile"),
  df = Inf
) {
  UseMethod("calc_ci")
}

#' @keywords internal
calc_ci.lm <- function(
  model,
  conf_level = 0.95,
  method = c("wald", "profile")
) {
  method <- match.arg(method)
  if (method == "profile" || method == "likelihood") {
    ci <- stats::confint(model, level = conf_level)
    out <- tibble::as_tibble(ci, rownames = "term")
    names(out) <- c("term", "conf_low", "conf_high")
    return(out)
  }
  s <- summary(model)$coefficients
  tt <- tibble::as_tibble(s, rownames = "term")
  crit <- stats::qt(1 - (1 - conf_level) / 2, df = model$df.residual)
  tt$conf_low <- tt$Estimate - crit * tt$`Std. Error`
  tt$conf_high <- tt$Estimate + crit * tt$`Std. Error`
  tt[c("term", "conf_low", "conf_high")]
}

#' @keywords internal
calc_ci.glm <- function(
  model,
  conf_level = 0.95,
  method = c("wald", "profile")
) {
  method <- match.arg(method)
  if (method == "profile") {
    ci <- stats::confint(model, level = conf_level)
    out <- tibble::as_tibble(ci, rownames = "term")
    names(out) <- c("term", "conf_low", "conf_high")
    return(out)
  } else if (method == "wald") {
    s <- summary(model)$coefficients
    tt <- tibble::as_tibble(s, rownames = "term")
    if (!is.null(model$family) && model$family$family == "gaussian") {
      crit <- stats::qt(1 - (1 - conf_level) / 2, df = model$df.residual)
    } else {
      crit <- stats::qnorm(1 - (1 - conf_level) / 2)
    }
    tt$conf_low <- tt$Estimate - crit * tt$`Std. Error`
    tt$conf_high <- tt$Estimate + crit * tt$`Std. Error`
    tt[c("term", "conf_low", "conf_high")]
  }
}

# svyglm method with Wald and likelihood/profile support
#' @keywords internal
calc_ci.svyglm <- function(
  model,
  conf_level = 0.95,
  method = c("wald", "profile")
) {
  method <- match.arg(method)

  # Extract coefficient table
  s <- summary(model)$coefficients
  tt <- tibble::as_tibble(s, rownames = "term")

  # get df
  ddf <- model$df.residual

  if (method == "wald") {
    # Wald critical value per survey df rules
    crit <- if (is.infinite(ddf)) {
      stats::qnorm(1 - (1 - conf_level) / 2)
    } else if (ddf > 0) {
      stats::qt(1 - (1 - conf_level) / 2, df = ddf)
    } else {
      NA_real_
    }
    tt$conf_low <- tt$Estimate - crit * tt$`Std. Error`
    tt$conf_high <- tt$Estimate + crit * tt$`Std. Error`
    return(tt[c("term", "conf_low", "conf_high")])
  } else if (method == "profile") {
    # Likelihood/profile path (survey-aware)

    # 1) Determine which parameters (all by default)
    pnames <- names(stats::coef(model))
    parm_idx <- seq_along(pnames)

    # 2) Lambda: robust-to-naive variance ratio (diagonal)
    cov_u <- model$cov.unscaled
    cov_n <- model$naive.cov
    if (is.null(cov_u) || is.null(cov_n)) {
      cli::cli_abort(
        "Profile CIs require cov.unscaled and naive.cov; not found on svyglm object."
      )
    }
    # Safely extract diagonal elements
    du <- diag(cov_u[parm_idx, parm_idx, drop = FALSE])
    dn <- diag(cov_n[parm_idx, parm_idx, drop = FALSE])
    if (any(!is.finite(du)) || any(!is.finite(dn))) {
      cli::cli_abort(
        "Non-finite diagonal entries in covariances; cannot compute lambda for profile CIs."
      )
    }
    lam <- du / dn

    # 3) Map requested conf_level to per-parameter unscaled alpha, df-aware and lambda-adjusted
    # alpha[i] corresponds to one-sided tail for profiling
    alpha <- if (is.infinite(ddf)) {
      stats::pnorm(stats::qnorm((1 - conf_level) / 2) * sqrt(lam)) / 2
    } else if (ddf <= 0) {
      cli::cli_abort("Zero or negative denominator df for profile CI.")
    } else {
      stats::pnorm(stats::qt((1 - conf_level) / 2, df = ddf) * sqrt(lam)) / 2
    }

    # 4) Profile each parameter and collect intervals
    rval <- vector("list", length(parm_idx))
    for (i in seq_along(parm_idx)) {
      which_i <- parm_idx[i]
      prof_obj <- MASSprofile_glm(
        fitted = model,
        which = which_i,
        alpha = alpha[i]
      )
      ci_i <- confint_profile(
        object = prof_obj,
        parm = which_i,
        level = conf_level,
        unscaled_level = 2 * alpha[i]
      )
      rval[[i]] <- ci_i
    }

    # 5) Assemble output
    names(rval) <- pnames[parm_idx]
    ci_out <- if (length(rval) == 1) rval[[1]] else do.call(rbind, rval)
    ci_tbl <- tibble::as_tibble(ci_out, rownames = "term")
    names(ci_tbl) <- c("term", "conf_low", "conf_high")
    return(ci_tbl)
  }

  # Should never reach here, but safe guard:
  cli::cli_abort("Unknown method; use method = 'wald' or 'profile'.")
}

#'
#' @keywords interral
calc_ci.svystat <- function(
  model,
  conf_level = 0.95,
  method = c("wald", "profile"), # ignored here
  df = Inf
) {
  # Extract coefficients and SEs
  est <- coef(model)
  v <- vcov(model)
  if (!is.matrix(v) || NCOL(v) == 1) {
    se <- sqrt(v)
  } else {
    se <- sqrt(diag(v))
  }

  # Critical value (t or z)
  alpha <- (1 - conf_level) / 2
  crit <- qt(c(alpha, 1 - alpha), df = df)

  # Calculate CIs
  lower <- as.vector(est + se * crit[1])
  upper <- as.vector(est + se * crit[2])

  # Return as tibble
  out <- tibble::tibble(
    mean = unname(est),
    conf_low = unname(lower),
    conf_high = unname(upper)
  )

  out
}

#' @keywords internal
calc_ci.svrepstat <- calc_ci.svystat


# utility functions ------------------------------------------------------

SE <- function(object, ...) {
  v <- vcov(object)
  if (!is.matrix(v) || NCOL(v) == 1) sqrt(v) else sqrt(diag(v))
}


confint_profile <- function(
  object,
  parm = NULL,
  level = 0.95,
  unscaled_level,
  ...
) {
  of <- attr(object, "original.fit")
  pnames <- names(stats::coef(of))

  # Resolve parm to indices; default = all
  if (is.null(parm)) {
    parm <- seq_along(pnames)
  } else if (is.character(parm)) {
    parm <- match(parm, pnames, nomatch = 0L)
  }

  # Two-sided percentages for column names
  a <- (1 - level) / 2
  pct <- paste(round(100 * c(a, 1 - a), 1), "%")

  # Cutoffs from unscaled_level (tail probabilities)
  cutoff <- c(
    stats::qnorm(unscaled_level),
    stats::qnorm(unscaled_level, lower.tail = FALSE)
  )

  # Initialize output
  ci <- array(
    NA_real_,
    dim = c(length(parm), 2L),
    dimnames = list(pnames[parm], pct)
  )

  for (pm in parm) {
    pname <- pnames[pm]
    pro <- object[[pname]]
    if (is.null(pro)) {
      next
    }

    # pro is a data.frame with first column z-values and a "par.vals" matrix
    if (length(pnames) > 1L) {
      sp <- stats::spline(
        x = pro[, "par.vals"][, pm],
        y = pro[, 1]
      )
    } else {
      sp <- stats::spline(x = pro[, "par.vals"], y = pro[, 1])
    }

    # Approximate inverse of profile function at desired cutoffs
    ci[pname, ] <- stats::approx(sp$y, sp$x, xout = cutoff)$y
  }

  drop(ci)
}


MASSprofile_glm <- function(
  fitted,
  which,
  alpha = 0.01,
  maxsteps = 10,
  del = NULL,
  trace = FALSE,
  ...
) {
  Pnames <- names(B0 <- stats::coef(fitted))
  nonA <- !is.na(B0)
  pv0 <- t(as.matrix(B0))
  p <- length(Pnames)

  # Default which = all params if missing
  if (missing(which)) {
    which <- seq_len(p)
  }
  if (is.character(which)) {
    which <- match(which, Pnames)
  }

  summ <- summary(fitted)
  std.err <- summ$coefficients[, "Std. Error", drop = FALSE]
  mf <- stats::model.frame(fitted)
  Y <- stats::model.response(mf)
  n <- NROW(Y)
  O <- stats::model.offset(mf)
  if (!length(O)) {
    O <- rep(0, n)
  }
  W <- stats::model.weights(mf)
  if (length(W) == 0L) {
    W <- rep(1, n)
  }
  OrigDev <- stats::deviance(fitted)
  DispPar <- summ$dispersion
  X <- stats::model.matrix(fitted)
  fam <- stats::family(fitted)

  # Family-specific profiling scale
  profName <- NULL
  zmax <- NULL
  switch(
    fam$family,
    binomial = ,
    poisson = ,
    `Negative Binomial` = {
      zmax <- sqrt(stats::qchisq(alpha, 1, lower.tail = FALSE))
      profName <- "z"
    },
    gaussian = ,
    quasi = ,
    inverse.gaussian = ,
    quasibinomial = ,
    quasipoisson = {
      zmax <- sqrt(stats::qf(alpha, 1, n - p, lower.tail = FALSE))
      profName <- "tau"
    }
  )

  if (is.null(zmax)) {
    cli::cli_abort("Unsupported family for profiling.")
  }
  if (is.null(del)) {
    del <- zmax / 5
  }

  prof <- vector("list", length = length(which))
  names(prof) <- Pnames[which]

  for (i in which) {
    if (!nonA[i]) {
      next
    }

    zi <- 0
    pvi <- pv0
    a <- nonA
    a[i] <- FALSE
    Xi <- X[, a, drop = FALSE]
    pi <- Pnames[i]

    for (sgn in c(-1, 1)) {
      if (trace) {
        message("\nParameter: ", pi, " ", c("down", "up")[(sgn + 1) / 2 + 1])
      }
      step <- 0
      z <- 0
      LP <- X[, nonA, drop = FALSE] %*% B0[nonA] + O

      while ((step <- step + 1) < maxsteps && abs(z) < zmax) {
        bi <- B0[i] + sgn * step * del * std.err[Pnames[i], 1]
        o <- O + X[, i] * bi

        fm <- stats::glm.fit(
          x = Xi,
          y = Y,
          weights = W,
          etastart = LP,
          offset = o,
          family = fam,
          control = fitted$control
        )

        LP <- Xi %*% fm$coefficients + o
        ri <- pv0
        ri[, names(stats::coef(fm))] <- stats::coef(fm)
        ri[, pi] <- bi
        pvi <- rbind(pvi, ri)

        zz <- (fm$deviance - OrigDev) / DispPar
        if (zz > -0.001) {
          zz <- max(zz, 0)
        } else {
          stop(
            "profiling found a better solution; original fit had not converged"
          )
        }
        z <- sgn * sqrt(zz)
        zi <- c(zi, z)
      }
    }

    si <- order(zi)
    prof[[pi]] <- structure(data.frame(zi[si]), names = profName)
    prof[[pi]]$par.vals <- pvi[si, , drop = FALSE]
  }

  val <- structure(prof, original.fit = fitted, summary = summ)
  class(val) <- c("profile.glm", "profile")
  val
}
