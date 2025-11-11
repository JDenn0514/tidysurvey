#' Get the question_preface attribute
#'
#' This function makes it easy to get the question_preface from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the question preface attribute with this function.
#'
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#'
#' @returns If `x` is a variable or vector, a string containing the "question_preface"
#'   attribute, if one is present, is returned. If `x` is a `data.frame` then a
#'   named vector with the "question_preface" attribute from each variable is returned.
#'
#' @examples
#' # create a random vector
#' x <- sample(c(0, 1), replace = TRUE, size = 10)
#' # add a question preface attribute
#' attr(x, "question_preface") <- "This is a question preface"
#' # check to see that there is a new attribute called `question_preface`
#' attributes(x)
#' # now get the question_preface
#' attr_question_preface(x)
#'
#' # now let's create a realistic workflow with a data.frame --------
#' # create a fake dataset
#' df <- data.frame(
#'   x_1 = sample(c(0, 1), replace = TRUE, size = 10),
#'   x_2 = sample(c(0, 1), replace = TRUE, size = 10),
#'   x_3 = sample(c(0, 1), replace = TRUE, size = 10),
#'   x_4 = sample(c(0, 1), replace = TRUE, size = 10)
#' )
#'
#' # set the variable labels
#' attr(df$x_1, "label") <- "Which of the following colors do you like? Blue"
#' attr(df$x_2, "label") <- "Which of the following colors do you like? Red"
#' attr(df$x_3, "label") <- "Which of the following colors do you like? Yellow"
#' attr(df$x_4, "label") <- "Which of the following colors do you like? Purple"
#'
#' # set the value labels
#' attr(df$x_1, "labels") <- c("Blue" = 1)
#' attr(df$x_2, "labels") <- c("Red" = 1)
#' attr(df$x_3, "labels") <- c("Yellow" = 1)
#' attr(df$x_4, "labels") <- c("Purple" = 1)
#'
#' # check the attributes
#'
#'
#' # add the question prefaces and update the variable labels for each column in df
#' for(x in names(df)) {
#'   df[[x]] <- set_question_preface(x, df)
#' }
#'
#' # now if I'm curious what the question prefaces are for the df, I can easily
#' # see all of them using `attr_question_preface`
#' attr_question_preface(df)
#'
#'
#'
#' @export
attr_question_preface <- function(x, data) {
  UseMethod("attr_question_preface")
}

#' @export
attr_question_preface.default <- function(x, data) {
  if (missing(data)) {
    attr(x, "question_preface", exact = TRUE)
  } else {
    attr(data[[x]], "question_preface", exact = TRUE)
  }
}


# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the data
#' @export
attr_question_preface.data.frame <- function(x, data = NULL) {
  # get a list of columns
  cols <- names(x)

  # write up a function that makes the string in the format we want
  string_fun <- function(var) {
    string <- attr(x[[var]], "question_preface", exact = TRUE)
  }

  # iterate string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) |>
    # set the names of the objects in the list
    stats::setNames(cols)
}
