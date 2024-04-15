#' Write a ReDaMoR data model to the database
#'
#' @param x data model generated using ReDaMoR::read_json_data_model or ReDaMoR::model_relational_data
#' @param schema_name name of the scheme to create the tables in
#' @param owner_name owner of the tables
#' @param overwrite logical. If set to TRUE then it will overwrite any existing tables in the schema with the same name as a table in the model.
#' @param conn database connection
#'
#' @return
#' @export
#'
db_write_data_model <- function(x, schema_name, owner_name, overwrite = F, conn = db){
  #find if tables already exist
  for (table in x) {
    exists <- DBI::dbExistsTable(conn, RPostgres::Id(schema_name, table$tableName))

    if (exists & !overwrite) {
      stop(paste0('Table "', table$tableName, '" already exists in the schema "', schema_name, '"'))
    } else if (exists & overwrite) {
      DBI::dbExecute(conn, glue::glue_sql("DROP TABLE {`schema_name`}.{`table$tableName`} CASCADE;",
                                          .con = conn))
    }
  }

  #Create tables
  for (table in x) {
    #initialise table
    create_statement <- glue::glue_sql('CREATE TABLE {`schema_name`}.{`table$tableName`}
                            (
                              kwtid integer NOT NULL,
                              PRIMARY KEY (kwtid)
                            )',
                            .con = conn)
    DBI::dbExecute(conn, create_statement)

    #add table description
    comment_statement <- glue::glue_sql('COMMENT ON TABLE {`schema_name`}.{`table$tableName`}
                            is {table$display$comment}',
                            .con = conn)
    DBI::dbExecute(conn, comment_statement)

    #add table owner
    owner_statement <- glue::glue_sql('
                            ALTER TABLE IF EXISTS {`schema_name`}.{`table$tableName`}
                            OWNER to "KWTAdminGroup";',
                            .con = conn)
    DBI::dbExecute(conn, owner_statement)

    #add table fields
    fields <- table$fields %>%
      dplyr::filter(name != "kwtid") %>%
      dplyr::mutate(type = dplyr::case_when(
        type == "POSIXct" ~ "timestamp without time zone",
        type == "integer" ~ "integer",
        type == "character" ~ "character varying",
        type == "logical" ~ "boolean",
        TRUE ~ NA
      ))

    if (any(is.na(fields$type))) stop("Data type not recognised")

    for (i in 1:nrow(fields)) {
      #create field
      field_statement <- glue::glue_sql(
        paste('ALTER TABLE IF EXISTS {`schema_name`}.{`table$tableName`}
            ADD COLUMN {`fields$name[i]`}', fields$type[i],
            if (!fields$nullable[i]) {
              "NOT NULL"
            }),
        .con = conn)
      DBI::dbExecute(conn, field_statement)

      #comment on field
      comment_statement <- glue::glue_sql(
        'COMMENT ON COLUMN {`schema_name`}.{`table$tableName`}.{`fields$name[i]`} is {fields$comment[i]}',
        .con = conn
      )
      DBI::dbExecute(conn, comment_statement)

    }
  }

  #add foreign keys
  for (table in x) {
    for (fk in table$foreignKeys) {
      foreign_key_statement <- glue::glue_sql(
        'ALTER TABLE {`schema_name`}.{`table$tableName`}
         ADD CONSTRAINT {`fk$key$from`} FOREIGN KEY ({`fk$key$from`})
          REFERENCES {`schema_name`}.{`fk$refTable`} ({`fk$key$to`}) MATCH SIMPLE
          ON UPDATE RESTRICT
          ON DELETE RESTRICT;',
        .con = conn
      )
      DBI::dbExecute(conn, foreign_key_statement)
    }
  }
}
