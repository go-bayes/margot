#' Bind and format domain-specific tables
#'
#' This function binds a named list of domain-specific tables, optionally
#' sorts by the raw E_Val_bound column (ascending or descending),
#' optionally renames columns, highlights rows above a threshold, and
#' groups rows by domain. works for latex, html, or markdown output.
#'
#' @param tables_list a named list of data frames; each element is a domain table.
#'   row-names are captured as an "Outcome" column.
#' @param sort_E_val_bound character: one of "none", "asc", or "desc". default "none".
#' @param e_val_bound_threshold numeric; threshold for highlighting rows. default 1.
#' @param highlight_color character or NULL; background colour for highlighted rows. default "yellow".
#' @param bold logical; whether to bold highlighted rows. default TRUE.
#' @param output_format character; one of "latex", "html", or "markdown". default "latex".
#' @param rename_cols logical; whether to apply column renaming. default TRUE.
#' @param col_renames named list; mapping new_name = old_name for renaming.
#'   default list("E-Value"="E_Value","E-Value bound"="E_Val_bound").
#' @param rename_ate logical; if TRUE, renames the effect column to "ATE". default FALSE.
#' @param threshold_col character; name of the (pre-rename) column to test. default "E_Val_bound".
#' @param kbl_args list; extra arguments for kableExtra::kbl(),
#'   e.g. list(booktabs=TRUE, caption="Combined Results by Domain", align=NULL).
#'
#' @importFrom dplyr bind_rows arrange desc select everything
#' @importFrom kableExtra kbl row_spec pack_rows
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#' @export
margot_bind_tables <- function(
    tables_list,
    sort_E_val_bound      = c("none","asc","desc"),
    e_val_bound_threshold = 1,
    highlight_color       = "yellow",
    bold                  = TRUE,
    output_format         = "markdown",
    rename_cols           = TRUE,
    col_renames           = list("E-Value"="E_Value", "E-Value bound"="E_Val_bound"),
    rename_ate            = FALSE,
    threshold_col         = "E_Val_bound",
    kbl_args              = list(booktabs=TRUE, caption="Combined Results by Domain", align=NULL)
) {
  # coerce sort argument
  sort_E_val_bound <- match.arg(sort_E_val_bound)

  # bring rownames into a column called "Outcome"
  tables_list <- lapply(tables_list, function(df) {
    if (!"Outcome" %in% names(df)) {
      df <- tibble::rownames_to_column(df, var = "Outcome")
    }
    df
  })

  # bind rows with domain identifier
  combined <- dplyr::bind_rows(tables_list, .id = "Domain") %>%
    dplyr::select(Domain, Outcome, everything())

  # sort by raw E_Val_bound if requested
  if (sort_E_val_bound == "asc") {
    combined <- combined %>% arrange(.data$E_Val_bound)
  } else if (sort_E_val_bound == "desc") {
    combined <- combined %>% arrange(desc(.data$E_Val_bound))
  }

  # 1) rename effect column to "ATE" if requested
  if (rename_ate) {
    old_eff <- intersect(c("E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]"), names(combined))
    if (length(old_eff) == 1) {
      names(combined)[names(combined) == old_eff] <- "ATE"
    }
  }

  # 2) apply column renaming map
  if (rename_cols && length(col_renames) > 0) {
    for (new_nm in names(col_renames)) {
      old_nm <- col_renames[[new_nm]]
      idx <- which(names(combined) == old_nm)
      if (length(idx) == 1) {
        names(combined)[idx] <- new_nm
      }
    }
  }

  # determine final threshold column name
  threshold_col_name <- if (threshold_col %in% names(combined)) {
    threshold_col
  } else {
    names(col_renames)[col_renames == threshold_col] %||% threshold_col
  }

  # markdown output: bold rows above threshold
  if (output_format == "markdown") {
    df_md <- combined
    if (bold && threshold_col_name %in% names(df_md)) {
      hi <- which(as.numeric(df_md[[threshold_col_name]]) > e_val_bound_threshold)
      if (length(hi) > 0) {
        df_md[] <- lapply(df_md, as.character)
        for (r in hi) {
          df_md[r, ] <- lapply(df_md[r, ], function(v) {
            if (nzchar(v) && !grepl("^\\*\\*.*\\*\\*$", v)) {
              paste0("**", v, "**")
            } else {
              v
            }
          })
        }
      }
    }
    align_vec <- if (!is.null(kbl_args$align)) {
      kbl_args$align
    } else {
      rep("l", ncol(df_md))
    }
    return(knitr::kable(
      df_md,
      format    = "markdown",
      caption   = kbl_args$caption,
      row.names = FALSE,
      align     = align_vec,
      col.names = names(df_md),
      escape    = FALSE
    ))
  }

  # latex/html output
  domain_vec <- combined$Domain
  disp_df <- dplyr::select(combined, -Domain)
  hi_rows    <- which(as.numeric(disp_df[[threshold_col_name]]) > e_val_bound_threshold)

  tbl <- do.call(kableExtra::kbl, c(list(disp_df, format = output_format), kbl_args))

  if (length(hi_rows) > 0) {
    if (!is.null(highlight_color)) {
      if (output_format == "html") {
        tbl <- tbl %>%
          kableExtra::row_spec(
            hi_rows,
            background = highlight_color,
            extra_css  = if (bold) "font-weight: bold;" else NULL
          )
      } else {
        tbl <- tbl %>%
          kableExtra::row_spec(
            hi_rows,
            bold       = bold,
            background = highlight_color
          )
      }
    } else if (bold) {
      css <- if (output_format == "html") "font-weight: bold;" else NULL
      tbl <- tbl %>%
        kableExtra::row_spec(hi_rows, extra_css = css, bold = bold)
    }
  }

  # pack rows by Domain
  domains <- unique(domain_vec)
  idx     <- data.frame(
    start = vapply(domains, function(x) min(which(domain_vec == x)), integer(1)),
    end   = vapply(domains, function(x) max(which(domain_vec == x)), integer(1))
  )
  for (i in seq_along(domains)) {
    tbl <- try(tbl %>% kableExtra::pack_rows(domains[i], idx$start[i], idx$end[i]), silent = TRUE)
  }

  tbl
}
