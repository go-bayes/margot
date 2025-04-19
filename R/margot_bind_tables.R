#' Bind and Format Domain‑Specific Tables
#'
#' This function binds a named list of domain‑specific tables, optionally
#' sorts by the raw E_Val_bound column (ascending or descending), highlights
#' rows where E_Val_bound exceeds a threshold, and groups rows by domain.
#' Works for LaTeX, HTML, or Markdown output.
#'
#' @importFrom dplyr bind_rows arrange desc select
#' @importFrom kableExtra kbl row_spec pack_rows
#' @importFrom rlang .data
#'
#' @param tables_list A named list of data frames; each element is a domain table.
#' @param sort_E_val_bound Character: one of "none", "asc", or "desc".
#'                         If "asc", sorts by raw E_Val_bound ascending;
#'                         if "desc", sorts descending. Default "none".
#' @param e_val_bound_threshold Numeric; rows with E_Val_bound above this value will be highlighted. Default 1.
#' @param highlight_color Character or NULL; background colour for highlighted rows. Default "yellow".
#' @param bold Logical; whether to bold highlighted rows. Default TRUE.
#' @param output_format Character; one of "latex", "html", or "markdown". Default "latex".
#' @param rename_cols Logical; whether to apply column renaming. Default TRUE.
#' @param col_renames Named list; mapping new_name = old_name for renaming.
#'                     Default list("E-Value" = "E_Value",
#'                                   "E-Value bound" = "E_Val_bound").
#' @param threshold_col Character; name of the column (pre‑rename) to test. Default "E_Val_bound".
#' @param kbl_args List; extra arguments for kbl(), e.g. list(booktabs = TRUE, caption = "Combined Results by Domain").
#'
#' @return A knitr_kable object with rows sorted, highlighted, and grouped by domain.
#' @export
margot_bind_tables <- function(tables_list,
                               sort_E_val_bound = c("none", "asc", "desc"),
                               e_val_bound_threshold = 1,
                               highlight_color = "yellow",
                               bold = TRUE,
                               output_format = "latex",
                               rename_cols = TRUE,
                               col_renames = list(
                                 "E-Value"       = "E_Value",
                                 "E-Value bound" = "E_Val_bound"
                               ),
                               threshold_col = "E_Val_bound",
                               kbl_args = list(booktabs = TRUE, caption = "Combined Results by Domain")) {
  # validate inputs
  if (!is.list(tables_list) || is.null(names(tables_list)) ||
      any(names(tables_list) == "") || length(tables_list) == 0) {
    stop("'tables_list' must be a non-empty named list of data frames")
  }
  if (!all(sapply(tables_list, is.data.frame))) {
    stop("all elements of 'tables_list' must be data frames")
  }

  sort_E_val_bound <- match.arg(sort_E_val_bound)

  # bind rows with domain identifier
  combined <- dplyr::bind_rows(tables_list, .id = "Domain")

  # sort by raw E_Val_bound if requested
  if (sort_E_val_bound == "asc") {
    combined <- combined %>% dplyr::arrange(.data$E_Val_bound)
  } else if (sort_E_val_bound == "desc") {
    combined <- combined %>% dplyr::arrange(dplyr::desc(.data$E_Val_bound))
  }

  # apply column renaming
  if (rename_cols && length(col_renames) > 0) {
    for (new_nm in names(col_renames)) {
      old_nm <- col_renames[[new_nm]]
      if (old_nm %in% names(combined)) {
        names(combined)[names(combined) == old_nm] <- new_nm
      }
    }
  }

  # determine renamed threshold column name
  threshold_col_name <- threshold_col
  for (new_nm in names(col_renames)) {
    if (col_renames[[new_nm]] == threshold_col) {
      threshold_col_name <- new_nm
      break
    }
  }

  # Markdown output: bolding and preserve headers
  if (output_format == "markdown") {
    cap   <- kbl_args$caption %||% NULL
    align <- kbl_args$align   %||% NULL
    df_md <- combined

    # bold threshold-exceeding rows
    if (bold && threshold_col_name %in% names(df_md)) {
      hi_rows <- which(as.numeric(df_md[[threshold_col_name]]) > e_val_bound_threshold)
      if (length(hi_rows)) {
        df_md[] <- lapply(df_md, as.character)
        for (r in hi_rows) {
          for (c in names(df_md)) {
            v <- df_md[r, c]
            if (nzchar(v) && !grepl("^\\*\\*.*\\*\\*$", v)) {
              df_md[r, c] <- paste0("**", v, "**")
            }
          }
        }
      }
    }

    return(knitr::kable(
      df_md,
      format    = "markdown",
      caption   = cap,
      align     = align,
      col.names = names(df_md),
      escape    = FALSE
    ))
  }

  # LaTeX/HTML output: drop Domain, highlight, then group
  domain_vec <- combined$Domain
  disp_df    <- dplyr::select(combined, -Domain)
  hi_rows    <- which(as.numeric(disp_df[[threshold_col_name]]) > e_val_bound_threshold)

  tbl <- do.call(kableExtra::kbl,
                 c(list(disp_df, format = output_format), kbl_args))

  if (length(hi_rows)) {
    if (!is.null(highlight_color)) {
      if (output_format == "html") {
        tbl <- tbl %>% kableExtra::row_spec(
          hi_rows,
          background = highlight_color,
          extra_css   = if (bold) "font-weight: bold;" else NULL
        )
      } else {
        tbl <- tbl %>% kableExtra::row_spec(
          hi_rows,
          bold       = bold,
          background = highlight_color
        )
      }
    } else if (bold) {
      css <- if (output_format == "html") "font-weight: bold;" else NULL
      tbl <- tbl %>% kableExtra::row_spec(hi_rows, extra_css = css, bold = bold)
    }
  }

  # pack rows by Domain
  domains <- unique(domain_vec)
  idx <- data.frame(
    start = vapply(domains, function(x) min(which(domain_vec == x)), integer(1)),
    end   = vapply(domains, function(x) max(which(domain_vec == x)), integer(1))
  )
  for (i in seq_along(domains)) {
    try({
      tbl <- tbl %>% kableExtra::pack_rows(domains[i], idx$start[i], idx$end[i])
    })
  }

  tbl
}
