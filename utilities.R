remove_holidays_and_calc_expected_hours <- function(dfs, holidays_df) {
  # Default to the first day of the previous month
  month_ref <- floor_date(Sys.Date() %m-% months(1), "month")
  # grab holiday dates
  holidays <- holidays_df$date

  start_month <- floor_date(month_ref, "month")
  end_month <- ceiling_date(month_ref, "month") - days(1)

  all_dates <- seq.Date(start_month, end_month, by = "day")

  workdays <- all_dates[
    !weekdays(all_dates) %in% c("Saturday", "Sunday") &
      !all_dates %in% holidays
  ]

  expected_hours <- length(workdays) * 6.5

  # Preserve names of input list
  dfs_named <- dfs
  if (is.null(names(dfs_named))) {
    names(dfs_named) <- paste0("df", seq_along(dfs_named))
  }

  # Clean and track each df
  cleaned_dfs <- imap(
    dfs_named,
    ~ {
      if ("Start Date" %in% names(.x) && nrow(.x) > 0) {
        .x <- filter(.x, !as.Date(.x[["Start Date"]]) %in% holidays)
      }
      .x
    }
  )

  # Summary table
  summary_tbl <- tibble(
    name = names(cleaned_dfs),
    n_rows = map_int(cleaned_dfs, nrow),
    expected_hours = expected_hours
  )

  list(data = cleaned_dfs, summary = summary_tbl)
}


clean_emails <- function(email_string, blacklist) {
  email_string %>%
    str_split(",") %>% # Split by comma only
    map(~ trimws(.x)) %>% # Trim whitespace
    map(~ setdiff(.x, blacklist)) %>% # Remove admin emails
    map_chr(~ paste(.x, collapse = ", ")) # Re-collapse to string
}


is_gt_stop <- function(gt_object) {
  stopifnot(
    "'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = any(
      "gt_tbl" %in% class(gt_object)
    )
  )
}

gt_theme_pff <- function(gt_object, ..., divider, spanners, rank_col) {
  is_gt_stop(gt_object)

  built_table <- gt_object %>%
    opt_row_striping() %>%
    opt_all_caps() %>%
    tab_options(
      table_body.hlines.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "white",
      table.border.bottom.color = "grey",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(1),
      # column_labels.background.color = "#8CD3D6",
      stub.font.weight = 'bold',
      column_labels.padding = px(6),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(0),
      column_labels.border.bottom.color = "transparent",
      row.striping.background_color = "#f8f8f8",
      data_row.padding = px(8),
      heading.align = "left",
      heading.title.font.size = px(30),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = px(18),
      table.font.size = px(14),
      ...
    ) %>%
    # customize font
    opt_table_font(
      font = google_font(name = "Roboto"),
    )

  if (!missing(spanners)) {
    span_vars <- unlist(gt_object[["_spanners"]][["vars"]])

    # add blank span and modify
    built_table <- built_table %>%
      tab_spanner(
        columns = c(gt::everything(), -any_of(span_vars)),
        label = " ",
        id = "blank"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "transparent"),
          cell_text(color = "transparent", size = px(12), weight = "bold"),
          cell_borders(sides = "left", color = "transparent", weight = px(3)),
          cell_borders(sides = "top", color = "transparent", weight = px(3))
        ),
        locations = list(
          cells_column_spanners(
            spanners = "blank"
          )
        )
      ) %>%
      # add real spanners and style
      tab_style(
        style = list(
          cell_fill(color = "#f5f5f5"),
          cell_text(color = "#2d2e2e", size = px(12), weight = "bold"),
          cell_borders(sides = "left", color = "white", weight = px(3)),
          cell_borders(sides = "top", color = "white", weight = px(3))
        ),
        locations = list(
          cells_column_spanners(
            spanners = spanners
          )
        )
      )
  }

  if (!missing(divider)) {
    built_table <- built_table %>%
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "lightgrey",
          weight = px(2)
        ),
        locations = cells_body(columns = {{ divider }})
      ) %>%
      tab_style(
        style = cell_borders("left", color = "#ffffff", weight = px(2)),
        locations = cells_column_labels(columns = {{ divider }})
      )
  }

  if (!missing(rank_col)) {
    built_table <- built_table %>%
      tab_style(
        style = list(
          cell_fill(color = "#e4e8ec"),
          cell_borders(color = "#e4e8ec")
        ),
        locations = cells_body(columns = {{ rank_col }})
      ) %>%
      cols_align("center", {{ rank_col }})
  }

  built_table %>%
    tab_style(
      style = list(
        cell_fill(color = "#468f86"), #58978f
        cell_text(color = "white", size = px(13), weight = "bold"),
        cell_borders(
          sides = c("bottom"),
          color = "#f1f4f8",
          weight = px(2.5)
        )
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_stubhead()
      )
    )
}

# gt_theme_pff <- function(gt_object, ..., divider, spanners, rank_col) {
#   is_gt_stop(gt_object)

gt_theme_538 <- function(gt_object, ...) {
  stopifnot(
    `'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
      class(gt_object)
  )
  # Extract first column name
  first_col <- names(gt_object[["_data"]])[1]
  first_col_sym <- rlang::sym(first_col)

  gt_object %>%
    opt_table_font(
      font = list(google_font("Fira Sans"), default_fonts()),
      weight = "normal"
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(font = google_font("Nunito"), weight = "normal")
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(font = google_font("Chivo"), weight = "normal")
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = "top", color = "black", weight = px(0)),
        cell_text(
          font = google_font("Chivo"),
          transform = "uppercase",
          v_align = "bottom",
          size = px(14),
          weight = "normal"
        )
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
      locations = cells_row_groups()
    ) %>%
    tab_options(
      column_labels.background.color = "white", #"#eef5f2",
      data_row.padding = px(6),
      heading.border.bottom.style = "none",
      table.border.top.width = px(3),
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      heading.align = "left",
      heading.title.font.size = px(27),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = px(17),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "white",
      stub.background.color = "#f2f2f2",
      stub.border.style = "dashed",
      stub.border.color = "white",
      table_body.hlines.style = "dashed",
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 15,
      #  heading.align = "left",
      ...
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "#949494", weight = px(2)),
      locations = cells_body(
        columns = c(everything()),
        rows = !!first_col_sym == "Total"
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "top",
        color = "#949494",
        weight = px(2)
      ),
      locations = cells_stub(
        rows = !!first_col_sym == "Total"
      )
    ) %>%
    opt_css(
      "tbody tr:last-child {\n    border-bottom: 2px solid #ffffff00;\n      }\n\n    ",
      add = TRUE
    )
}
