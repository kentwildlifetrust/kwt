#' Write a postgisMoR data model to the database
#'
#' @param x RelDataModel list created by `kwt::read_json_data_model` or `kwt::model_relational_data`
#' @param schema_name name of the schema to create the tables in. If NULL, the table names must be in the format schema_name.table_name.
#' @param crs_srid the SRID of the coordinate reference system to use for geometry columns, if they are present in any of the tables.
#' @param overwrite logical. If set to TRUE then it will overwrite any existing tables with names matching the tables in the model. Data will be lost.
#' @param conn database connection object
#'
#' @export
#'
db_write_data_model <- function(x, schema_name = NULL, crs_srid = 4326, overwrite = F, conn = db){
  library(magrittr)

  #check that the data types are all valid
  valid_types <- c("POSIXct", "numeric", "date", "timestamp", "integer", "character",
                   "logical", "point geometry", "linestring geometry",
                   "polygon geometry", "multipoint geometry", "multilinestring geometry",
                   "multipolygon geometry", "geometry collection")
  data_types <- unique(unlist(lapply(x, function(table) table$fields$type)))
  invalid_types <- data_types[!data_types %in% valid_types]

  if (length(invalid_types) > 0) {
    stop(paste("Invalid data types found:",
               paste(invalid_types, collapse = ", "),
               "\n Please use one of the following data types:",
               paste(valid_types, collapse = ", ")))
  }


  #make a list of tables and schemas
  table_schemas <- names(x) %>%
    stringr::str_split("\\.")

  #all tables must not have a schema name if schema_name is provided
  if (!is.null(schema_name) & any(sapply(table_schemas, length) == 2)) {
    stop("Table names must not contain a schema name if schema_name is provided")
  }
  #all tables must have a schema name if schema_name is not provided
  if (is.null(schema_name) & any(sapply(table_schemas, length) != 2)) {
    stop("Table names must contain a schema name if schema_name is not provided")
  }
  #if schema_name is provided, add it into table_schemas
  if (!is.null(schema_name)) {
    table_schemas <- lapply(table_schemas, function(x) list(schema = schema_name, table = x))
  } else {
    table_schemas <- lapply(table_schemas, function(x) list(schema = x[1], table = x[2]))
  }

  stopifnot(length(table_schemas) == length(x))

  #check that the named schemas exist. If not, create them
  for (schema in unique(sapply(table_schemas, function(x) x$schema))) {
    schema_exists <- "SELECT EXISTS (SELECT 1 FROM information_schema.schemata WHERE schema_name = {schema});" %>%
      glue::glue_sql(.con = conn) %>%
      DBI::dbGetQuery(conn, .)
    if (schema_exists$exists != "1") {
      DBI::dbExecute(conn, glue::glue_sql("CREATE SCHEMA {`schema`};", .con = conn))
      message("Created schema '", schema, "'")
    }
  }


  #find if tables already exist
  for (i in 1:length(x)) {
    s <- table_schemas[[i]]$schema
    t <- table_schemas[[i]]$table
    exists <- DBI::dbExistsTable(conn, DBI::Id(schema=s, table=t))

    if (exists & !overwrite) {
      stop(paste0('Table "', t, '" already exists in the schema "', s, '"'))
    } else if (exists & overwrite) {
      DBI::dbExecute(conn, glue::glue_sql("DROP TABLE {`s`}.{`t`} CASCADE;",
                                          .con = conn))
      DBI::dbExecute(conn, glue::glue_sql("DROP SEQUENCE {`s`}.{`paste0(t, '_kwtid')`};",
                                          .con = conn))
    }
  }

  #Create tables
  for (i in 1:length(x)) {
    s <- table_schemas[[i]]$schema
    t <- table_schemas[[i]]$table

    #add sequence on kwtid
    sequence_name <- paste0(t, "_kwtid")
    sequence_statement <- glue::glue_sql('CREATE SEQUENCE IF NOT EXISTS {`s`}.{`sequence_name`}
                                            INCREMENT 1
                                            START 1
                                            MINVALUE 1
                                            MAXVALUE 1000000000000
                                            CACHE 1;',
                                         .con = conn)
    DBI::dbExecute(conn, sequence_statement)

    #initialise table
    create_statement <- glue::glue_sql("CREATE TABLE {`s`}.{`t`}
                            (
                              kwtid integer DEFAULT nextval({paste0(s, '.', sequence_name)}) NOT NULL,
                              PRIMARY KEY (kwtid)
                            )",
                                       .con = conn)
    DBI::dbExecute(conn, create_statement)


    #add table description
    comment_statement <- glue::glue_sql('COMMENT ON TABLE {`s`}.{`t`}
                            is {x[[i]]$display$comment}',
                                        .con = conn)
    DBI::dbExecute(conn, comment_statement)

    #add table fields
    fields <- x[[i]]$fields
    if (nrow(fields) == 0) next

    fields <- fields %>%
      dplyr::filter(name != "kwtid")

    if (nrow(fields) == 0) next

    fields <- fields %>%
      dplyr::mutate(type = dplyr::case_when(
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

    for (j in 1:nrow(fields)) {
      #create field
      field_statement <- glue::glue_sql(
        paste('ALTER TABLE IF EXISTS {`s`}.{`t`}
            ADD COLUMN {`fields$name[j]`}', fields$type[j],
              if (!fields$nullable[j]) {
                "NOT NULL"
              }),
        .con = conn)
      DBI::dbExecute(conn, field_statement)
      #comment on field
      comment_statement <- glue::glue_sql(
        'COMMENT ON COLUMN {`s`}.{`t`}.{`fields$name[j]`} is {fields$comment[j]}',
        .con = conn
      )
      DBI::dbExecute(conn, comment_statement)
    }
  }

  #add foreign keys
  for (i in 1:length(x)) {
    from <- list(
      schema = table_schemas[[i]]$schema,
      table = table_schemas[[i]]$table
    )
    for (fk in x[[i]]$foreignKeys) {
      to <- table_schemas[[which(names(x) %in% fk$refTable)]]
      foreign_key_statement <- glue::glue_sql(
        'ALTER TABLE {`from$schema`}.{`from$table`}
         ADD CONSTRAINT {`fk$key$from`} FOREIGN KEY ({`fk$key$from`})
          REFERENCES {`to$schema`}.{`to$table`} ({`fk$key$to`}) MATCH SIMPLE
          ON UPDATE RESTRICT
          ON DELETE RESTRICT;',
        .con = conn
      )
      DBI::dbExecute(conn, foreign_key_statement)
    }
  }
  message(paste("Successfully created" , length(x), "tables"))
}

get_geom_type <- function(conn, schema, table) {
  geom_type <- "SELECT srid,
            CONCAT('geometry(', type, ', ', srid, ')') AS type
        FROM
            geometry_columns
        WHERE
            f_table_schema = {schema}
            AND f_table_name = {table}
            AND f_geometry_column = 'geom';" %>%
    glue::glue_sql(.con = conn) %>%
    DBI::dbGetQuery(conn, .)

  if (nrow(geom_type) == 1) {
    geom_type <- geom_type %>%
      dplyr::mutate(type = dplyr::case_when(
        type == glue::glue("geometry(POINT, {geom_type$srid[1]})") ~ "point geometry",
        type == glue::glue("geometry(LINESTRING, {geom_type$srid[1]})") ~ "linestring geometry",
        type == glue::glue("geometry(POLYGON, {geom_type$srid[1]})") ~ "polygon geometry",
        type == glue::glue("geometry(MULTIPOINT, {geom_type$srid[1]})") ~ "multipoint geometry",
        type == glue::glue("geometry(MULTILINESTRING, {geom_type$srid[1]})") ~ "multilinestring geometry",
        type == glue::glue("geometry(MULTIPOLYGON, {geom_type$srid[1]})") ~ "multipolygon geometry",
        type == glue::glue("geometry(GEOMETRYCOLLECTION, {geom_type$srid[1]})") ~ "geometry collection",
        type == "geometry(GEOMETRY, 0)" ~ "geometry collection",
        TRUE ~ NA
      ))
    return(geom_type$type)
  } else {
    return(NA)
  }
}


#' Create a JSON data model based on a database
#'
#' @description This function helps with viewing database structures, by providing the right json to be ready by \code{read_json_data_model}.
#'     You can then make destructive updates to the data structure using \code{\link{db_write_data_model}}.
#'
#' @param conn database connection object
#' @param file_path path to save the JSON file
#'
#' @return a JSON file with the data model
#' @export
#'
#' @examples
#' \dontrun{
#'  conn <- DBI::dbConnect(odbc::odbc(), dsn = "Legacy Landscapes", timeout = 10)
#'  db_to_json_data_model(conn, "data_model.json")
#'  m <- read_json_data_model("data_model.json")
#'  m <- model_relational_data(m)
#'  db_write_data_model(m, schema_name = NULL, crs_srid = 4326, overwrite = T, conn = conn)
#'  DBI::dbDisconnect(conn)
#'  }
#'
#'
db_to_json_data_model <- function(conn, file_path){
  if (!inherits(conn, "DBIConnection")) {
    stop("conn must be a database connection")
  }

  ignore_schemas <- c("information_schema", "pg_catalog", "postgis", "admin", "dev", "backup", "public")

  #work out the tables and schemas
  table_schemas <- "SELECT table_schema AS schema, table_name AS table
              FROM information_schema.tables
       WHERE NOT table_schema IN ({ignore_schemas*})
       AND table_type = 'BASE TABLE';" %>%
    glue::glue_sql(.con = conn) %>%
    DBI::dbGetQuery(conn, .)

  json <- mapply(function(table, schema) {
    fields <- "SELECT column_name AS name, data_type AS type, is_nullable AS nullable
                FROM information_schema.columns
                WHERE table_name = {table} AND table_schema = {schema};" %>%
      glue::glue_sql(.con = conn) %>%
      DBI::dbGetQuery(conn, .) %>%
      dplyr::mutate(nullable = nullable %in% "YES")

    #check each field for a uniqueness restraint
    unique_constraints <- "SELECT
          conname AS constraint_name,
          conrelid::regclass AS table_name,
          a.attname AS column_name,
          con.conname AS constraint_type,
          n.nspname AS schema_name
      FROM
          pg_constraint con
      JOIN
          pg_attribute a ON a.attnum = ANY(con.conkey) AND a.attrelid = con.conrelid
      JOIN
          pg_namespace n ON n.oid = con.connamespace
      WHERE
          con.contype = 'u'  -- 'u' indicates a unique constraint
          AND n.nspname = {schema}
          AND con.conrelid = {paste0(schema, '.', table)}::regclass" %>%
        glue::glue_sql(.con = conn) %>%
        DBI::dbGetQuery(conn, .)


    fields <- fields %>%
      dplyr::left_join(unique_constraints, by = c("name" = "column_name")) %>%
      dplyr::mutate(unique = !is.na(constraint_name)) %>%
      dplyr::select(-constraint_name, -table_name, -constraint_type, -schema_name)


    #multile geometry columns should throw and error
    stopifnot(sum(fields$type %in% "USER-DEFINED") <= 1)

    #convert the data types
    fields <- fields %>%
      dplyr::mutate(type = dplyr::case_when(
        type == "float" ~ "numeric",
        type == "date" ~ "date",
        type == "timestamp without time zone" ~ "timestamp",
        type == "integer" ~ "integer",
        type == "character varying" ~ "character",
        type == "boolean" ~ "logical",
        type == "text" ~ "character",
        type == "bigint" ~ "integer",
        type == "geometry" ~ "geometry collection",
        type == "double precision" ~ "numeric",
        type == "USER-DEFINED" ~ get_geom_type(conn, schema, table),
        TRUE ~ NA
      ))

    if (any(is.na(fields$type))) stop("Data type in the column ", paste0(fields$name[is.na(fields$type)], collapse = ", "), " in the table ", schema, ".", table, " not recognised")

    fields$unique[fields$name == "kwtid"] <- T

    #get comments for each field
    col_comments <- "SELECT a.attname AS column_name,
                      d.description AS column_description
                  FROM
                      pg_description d
                  JOIN
                      pg_attribute a ON d.objsubid = a.attnum
                  JOIN
                      pg_class c ON d.objoid = c.oid AND a.attrelid = c.oid
                  JOIN
                      pg_namespace n ON c.relnamespace = n.oid
                  WHERE
                      n.nspname = {schema}
                      AND c.relname = {table};" %>%
        glue::glue_sql(.con = conn) %>%
        DBI::dbGetQuery(conn, .)

    fields <- fields %>%
      dplyr::left_join(col_comments, by = c("name" = "column_name")) %>%
      dplyr::mutate(comment = ifelse(is.na(column_description), "", column_description)) %>%
      dplyr::select(-column_description)

    foreign_keys <- "SELECT
                      CONCAT(ccu.table_schema, '.', ccu.table_name)  AS ref_table,
                      kcu.column_name AS key_from,
                      ccu.column_name AS key_to
                    FROM
                      information_schema.table_constraints AS tc
                      JOIN information_schema.key_column_usage AS kcu
                        ON tc.constraint_name = kcu.constraint_name
                      JOIN information_schema.constraint_column_usage AS ccu
                        ON ccu.constraint_name = tc.constraint_name
                    WHERE
                      tc.constraint_type = 'FOREIGN KEY' AND
                      tc.table_name = {table} AND
                      tc.table_schema = {schema};" %>%
      glue::glue_sql(.con = conn) %>%
      DBI::dbGetQuery(conn, .) %>%
      dplyr::distinct()

    table_comment <- "SELECT obj_description({paste0(schema, '.', table)}::regclass) AS table_comment;" %>%
      glue::glue_sql(.con = conn) %>%
      DBI::dbGetQuery(conn, .) %>%
      dplyr::pull()

    if (length(table_comment) == 0 || is.na(table_comment)) {
      table_comment <- "NA"
    }

    list(
      tableName = list(paste0(schema, ".", table)),
      fields = fields,
      primaryKey = list("kwtid"),
      foreignKeys = if (nrow(foreign_keys) > 0) {
        lapply(1:nrow(foreign_keys), function(i) {
          list(
            refTable = list(foreign_keys$ref_table[i]),
            key = list(list(
              from = foreign_keys$key_from[i],
              to = foreign_keys$key_to[i]
            )),
            cardinality = c(0, -1, 1, 1)
          )
        }) %>%
          unname()
      },
      indexes = list(list(
        fields = list("kwtid"),
        unique = list(T)
      )),
      display = list(
        x = list(45),
        y = list(45),
        color = list("NA"),
        comment = list(table_comment)
      )
    )
  }, table = table_schemas$table, schema = table_schemas$schema, SIMPLIFY = F)

  names(json) <- paste0(table_schemas$schema, ".", table_schemas$table)

  #write to a file with line breaks
  jsonlite::toJSON(json, auto_unbox = T) %>%
    jsonlite::prettify() %>%
    writeLines(con = file_path)

  return()

}

