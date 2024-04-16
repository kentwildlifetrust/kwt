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
db_write_data_model <- function(x, schema_name, owner_name, crs_srid = 4326, overwrite = F, conn = db){
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

    #add table owner
    owner_statement <- glue::glue_sql('
                            ALTER TABLE IF EXISTS {`schema_name`}.{`table$tableName`}
                            OWNER to {`owner_name`};',
                            .con = conn)
    DBI::dbExecute(conn, owner_statement)

    #add sequence on kwtid
    sequence_name <- paste0(table$tableName, "_kwtid")
    sequence_statement <- glue::glue_sql('CREATE SEQUENCE IF NOT EXISTS {`schema_name`}.{`sequence_name`}
                                            INCREMENT 1
                                            START 1
                                            MINVALUE 1
                                            MAXVALUE 1000000000000
                                            CACHE 1000;',
                                         .con = conn)
    DBI::dbExecute(conn, sequence_statement)

    #change schema name
    alter_sequence_statement_1 <- glue::glue_sql('ALTER SEQUENCE {`schema_name`}.{`sequence_name`}
                                                  OWNER TO {`owner_name`};',
                                                 .con = conn)
    DBI::dbExecute(conn, alter_sequence_statement_1)

    alter_sequence_statement_2 <- glue::glue_sql('ALTER SEQUENCE {`schema_name`}.{`sequence_name`}
                                                  OWNED BY {`schema_name`}.{`table$tableName`}.kwtid;',
                                                 .con = conn)
    DBI::dbExecute(conn, alter_sequence_statement_2)

    #add table description
    comment_statement <- glue::glue_sql('COMMENT ON TABLE {`schema_name`}.{`table$tableName`}
                            is {table$display$comment}',
                            .con = conn)
    DBI::dbExecute(conn, comment_statement)

    #add table fields
    fields <- table$fields %>%
      dplyr::filter(name != "kwtid") %>%
      dplyr::mutate(type = dplyr::case_when(
        type == "POSIXct" ~ "timestamp without time zone",
        type == "numeric" ~ "float",
        type == "date" ~ "date",
        type == "timestamp" ~ "timestamp without time zone",
        type == "integer" ~ "integer",
        type == "character" ~ "character varying",
        type == "logical" ~ "boolean",
        type == "point geometry" ~ glue::glue("geometry(POINT, {crs_srid})"),
        type == "linestring geometry" ~ glue::glue("geometry(LINESTRING, {crs_srid})"),
        type == "polygon geometry" ~ glue::glue("geometry(POLYGON, {crs_srid})"),
        type == "multipoint geometry" ~ glue::glue("geometry(MULTIPOINT, {crs_srid})"),
        type == "multilinestring geometry" ~ glue::glue("geometry(MULTILINESTRING, {crs_srid})"),
        type == "multipolygon geometry" ~ glue::glue("geometry(MULTIPOLYGON, {crs_srid})"),
        type == "geometry collection" ~ glue::glue("geometry(GEOMETRYCOLLECTION, {crs_srid})"),
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
  message(paste("Successfully created" , length(x), "tables"))
}
