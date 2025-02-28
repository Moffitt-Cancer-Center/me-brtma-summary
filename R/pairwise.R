#' Tabulate pairwise comparisons for all levels
#'
#' @param g1
#' @param g2
#' @param p.adjust.method
#'
#' @returns
#' @export
#'
#' @examples
pairwise.fisher.test <- function(g1, g2, p.adjust.method=p.adjust.methods) {
  g1 <- factor(g1)
  g2 <- factor(g2)
  DNAME <- ""
  p.adjust.method <- match.arg(p.adjust.method)
  compare.levels <- function(i,j) {
    fisher.test(table(g1,g2)[,c(i,j)])$p.value
  }
  PVAL <- pairwise.table(compare.levels, levels(g2), p.adjust.method)
  ans <- list(method="fisher.test", data.name=DNAME,
              p.value=PVAL, p.adjust.method=p.adjust.method)
  class(ans) <- "pairwise.htest"
  ans
}

#' Title
#'
#' @param data
#' @param variable
#' @param by
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
add_stat_pairwise <- function(data, variable, by, ...) {
  # calculate pairwise p-values
  pw <- pairwise.fisher.test(data[[variable]], data[[by]], p.adj = "none")

  # convert p-values to list
  index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L

      p.value.list[[index]] <-
        c(pw$p.value[i, j]) |>
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }

  # convert list to data frame
  p.value.list |>
    unlist() |>
    purrr::discard(is.na) |>
    t() |>
    as.data.frame() |>
    # formatting/roundign p-values
    dplyr::mutate(dplyr::across(everything(), gtsummary::style_pvalue))
}
