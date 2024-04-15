#' Edit values of a table.
#'
#' @param ref a table reference in the format provided by list_tables()
#' @param conn a connection object
#' @param backup whether to make a backup of the data before editing
#'
#' @export
#'
#'
edit_vals <- function(ref, backup = T, conn = db){
  if (ref$type != "table") {
    stop("This function only works with tables")
  }

  data <- start_query(ref) %>%
    run_query(geom_col = NULL, crs = 4326, conn = conn)

  if (backup) {
    if (!dir.exists("backups")) {
      dir.create("backups")
    }
    #make a backup of the data
    backup_name <- paste0(ref$table_schema, "-", ref$table_name, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    saveRDS(data, file.path("backups", paste0(backup_name, filext = ".rds")))
  }

  edited_data <- DataEditR::data_edit(data,
                                      col_edit = F,
                                      col_names = F,
                                      title = paste0(ref$table_schema, ".", ref$table_name),
                                      logo = "")


  #ask if user wants to save the data
  user_input <- readline("Save changes? (y/n) ")
  if (tolower(user_input) == "y") {
    #check which rows have been edited
    edited_rows <- dplyr::anti_join(edited_data, data)
    if (nrow(edited_rows) == 0) {
      message("No changes to save")
    } else {
      rows_to_delete <- data %>%
        dplyr::filter(kwtid %in% edited_rows$kwtid)

      if (nrow(rows_to_delete) > 0) {
        #delete the edited rows
        query <- glue::glue_sql("DELETE FROM {`ref$table_schema`}.{`ref$table_name`} WHERE kwtid IN ({rows_to_delete$kwtid*})",
                                .con = conn)
        DBI::dbExecute(conn, query)
      }

      #get max kwtid
      query <- glue::glue_sql("SELECT MAX(kwtid) FROM {`ref$table_schema`}.{`ref$table_name`}",
                              .con = conn)
      max_kwtid <- DBI::dbGetQuery(conn, query) %>%
        dplyr::pull(max) %>%
        as.integer()
      if (is.na(max_kwtid)) max_kwtid <- 0
      edited_rows$kwtid <- (max_kwtid + 1):(max_kwtid +
                                              nrow(edited_rows))

      #append the edited rows
      DBI::dbWriteTable(conn, RPostgres::Id(schema = ref$table_schema,
                                            table = ref$table_name),
                        edited_rows, append = T)
      message("Changes saved")
    }
  } else {
    message("Changes not saved")
  }
}

