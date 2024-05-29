#' Edit values of a table.
#' @description This function allows you to edit the values of a table in a database. It will open a shiny app where you can edit the values of the table. You can add, remove and edit rows. The function will then update the database with the changes you made. PLEASE NOTE: in the app, you must click the synchronise button and then 'Done' to apply changes.
#'
#' @param ref a table reference in the format provided by list_tables()
#' @param conn a connection object
#' @param backup whether to make a backup of the data before editing
#'
#' @examples
#' if (interactive()) {
#'   db <- DBI::dbConnect(odbc::odbc(), dsn = "shared", timeout = 10)
#'   make an example dataset in the testing schema of the database
#'   include a kwtid column, a name column, a description column and a geom column
#'   DBI::dbExecute(db, "DROP TABLE IF EXISTS testing.edit_vals_example;")
#'   DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS testing;")
#'   DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS testing.edit_vals_example (
#'                       kwtid SERIAL PRIMARY KEY,
#'                       name VARCHAR(50),
#'                       description TEXT,
#'                       geom geometry(POINT, 4326)
#'                     );")
#'   #generate some example data
#'   example_data <- data.frame(name = c("A", "B", "C"),
#'                             description = c("First", "Second", "Third"),
#'                             geom = list(c(0, 0), c(1, 1), c(2, 2)) %>%
#'                               lapply(sf::st_point) %>%
#'                               sf::st_as_sfc() %>%
#'                               sf::st_as_text())
#'   DBI::dbAppendTable(db,
#'                   RPostgres::Id(schema = "testing", table = "edit_vals_example"),
#'                   example_data)
#'   #list datbase tables
#'   db_list <- list_db(conn = db)
#'   #edit the example table
#'   edit_vals(db_list$testing$edit_vals_example, backup = F, conn = db)
#' }
#'
#' @export

edit_vals <- function(ref, backup = T, conn = db){
  if (ref$type != "table") {
    stop("This function only works with tables")
  }
  if (!("kwtid" %in% ref$atts$column_name)) {
    stop("kwtid column not found. Please add a kwtid column and make it the primary key.")
  }
  if ("geometry_text" %in% ref$atts$column_name) {
    stop("geometry_text column found. Please remove it.")
  }

  data <- start_query(ref = ref, conn = conn) %>%
    run_query(ref = ref, conn = conn)

  if (backup) {
    if (!dir.exists("backups")) {
      dir.create("backups")
    }
    #make a backup of the data
    backup_name <- paste0(ref$table_schema, "-", ref$table_name, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    saveRDS(data, file.path("backups", paste0(backup_name, filext = ".rds")))
  }

  if ("geometry" %in% ref$atts$udt_name) {
    geom_colname <- ref$atts %>%
      dplyr::filter(udt_name == "geometry") %>%
      dplyr::pull(column_name)
    if (length(geom_colname) > 1) {
      stop("Multiple geometry columns found. Please remove all but one.")
    }
    data_to_edit <- data %>%
      dplyr::mutate(geometry_text = .data[[geom_colname]] %>%
                      sf::st_as_text()) %>%
      sf::st_drop_geometry()
  } else {
    data_to_edit <- data
  }

  edited_data <- DataEditR::data_edit(data_to_edit,
                                      col_edit = F,
                                      col_names = F,
                                      title = paste0(ref$table_schema, ".", ref$table_name),
                                      logo = "")

  if (identical(data_to_edit, edited_data)) {
    message("No changes to save")
    return()
  }

  #ask if user wants to save the data
  user_input <- NA
  while (!(user_input %in% c("y", "n"))) {
    user_input <- tolower(readline("Save changes? (y/n) "))
    if (!(user_input %in% c("y", "n"))) {
      message("Invalid input. Please enter 'y' or 'n'")
    }
  }
  if (user_input == "n") {
    message("Changes not saved")
    return()
  } else {
    #add any missing kwtids
    edited_data <- edited_data %>%
      dplyr::mutate(kwtid = replace(kwtid,
                                    is.na(kwtid),
                                    seq(max(kwtid, na.rm = T) + 1,
                                        max(kwtid, na.rm = T) + sum(is.na(kwtid)))))

    #update any values that have changed
    edited_rows <- edited_data %>%
      dplyr::anti_join(data) %>%
      dplyr::filter(kwtid %in% data$kwtid) %>%
      dplyr::select(-geometry_text)

    if (nrow(edited_rows) > 0) {
      message("Updating rows with kwtids: ", paste(edited_rows$kwtid, collapse = ", "))
      #update the edited rows
      for (i in 1:nrow(edited_rows)) {
        vals <- as.list(edited_rows[i, ])
        for (j in 1:length(vals)) {
          query <- glue::glue_sql("UPDATE {`ref$table_schema`}.{`ref$table_name`}
                                SET {`names(vals[j])`} = {vals[[j]]}
                                WHERE kwtid = {edited_rows$kwtid[i]}",
                                  .con = conn)
          DBI::dbExecute(conn, query)
        }
      }
    }

    #insert any new values
    inserted_rows <- edited_data %>%
      dplyr::anti_join(data) %>%
      dplyr::filter(!(kwtid %in% data$kwtid)) %>%
      dplyr::select(-geometry_text)

    if (nrow(inserted_rows) > 0) {
      message("Inserting rows with kwtids: ", paste(inserted_rows$kwtid, collapse = ", "))
      #update the edited rows
      for (i in 1:nrow(inserted_rows)) {
        vals <- as.list(inserted_rows[i, ])
        #add in the geometry value
        if ("geometry" %in% ref$atts$udt_name) {
          geom_name <- ref$atts %>%
            dplyr::filter(udt_name == "geometry") %>%
            dplyr::pull(column_name)
          geom_val <- list(data_to_edit$geometry_text[1])
          message("Inserted rows will have the same geometry as the first row")
          names(geom_val) <- geom_name
          vals <- c(vals, geom_val)
        }
        query <- glue::glue_sql("INSERT INTO {`ref$table_schema`}.{`ref$table_name`}
                                  ({`names(vals)`*})
                               VALUES ({vals*});",
                                .con = conn)
        DBI::dbExecute(conn, query)
      }
    }


    #delete any removed rows
    deleted_kwtids <- data_to_edit %>%
      dplyr::anti_join(edited_data) %>%
      dplyr::filter(!(kwtid %in% edited_data$kwtid)) %>%
      dplyr::pull(kwtid)

    if (length(deleted_kwtids) > 0) {
      message("Deleting rows with kwtids: ", paste(deleted_kwtids, collapse = ", "))
      query <- glue::glue_sql("DELETE FROM {`ref$table_schema`}.{`ref$table_name`}
                              WHERE kwtid IN ({deleted_kwtids*})",
                              .con = conn)
      DBI::dbExecute(conn, query)
    }
  }
}

