#' View a directed graph of view dependencies in a database.
#'
#' @param conn A DBI connection object.
#' @param ignore_schemas A character vector of schema names to ignore.
#'
#' @return
#' @export
#'
map_views <- function(conn = db, ignore_schemas = c("information_schema", "pg_catalog", "postgis", "admin", "dev", "outputs_backup", "public")){
  #list the views in the database
  views <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "SELECT schemaname AS table_schema,
              viewname AS table_name,
              definition AS view_definition
              FROM pg_catalog.pg_views
              WHERE NOT schemaname IN ({ignore_schemas*});",
      .con = conn
    )
  )
  #list the materialised views in the database
  mat_views <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "SELECT schemaname AS table_schema,
              matviewname AS table_name,
              definition AS view_definition
              FROM pg_catalog.pg_matviews
              WHERE NOT schemaname IN ({ignore_schemas*});",
      .con = conn
    )
  )

  #list the tables in the database
  tables <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "SELECT table_schema, table_name
              FROM information_schema.tables
       WHERE NOT table_schema IN ({ignore_schemas*});",
      .con = conn
    )
  )

  #combine the views and tables into one data frame
  all_objects <- dplyr::full_join(
      dplyr::mutate(tables),
      dplyr::mutate(views, is_view = T),
      by = c("table_schema", "table_name")
    ) %>%
    dplyr::full_join(
      dplyr::mutate(mat_views, is_matview = T),
      by = c("table_schema", "table_name")
    ) %>%
    dplyr::mutate(is_view = dplyr::coalesce(is_view, F),
                  is_matview = dplyr::coalesce(is_matview, F),
                  view_definition = dplyr::coalesce(view_definition.x, view_definition.y)) %>%
    dplyr::select(table_schema, table_name, is_view, is_matview, view_definition) %>%
    dplyr::mutate(nrow = mapply(function(table_schema, table_name) {
      DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT COUNT(*) AS nrow
              FROM {`table_schema`}.{`table_name`};",
          .con = conn
        )
      )$nrow %>%
        as.integer()
    }, table_schema, table_name) %>%
      unname(),
    fields = mapply(function(table_schema, table_name) {
      DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT column_name
              FROM information_schema.columns
              WHERE table_schema = {table_schema}
              AND table_name = {table_name};",
          .con = conn
        )
      )$column_name %>%
        as.character() %>%
        paste0(collapse = "</li><li>")
    }, table_schema, table_name) %>%
      unname())

  #for each view, check the view definition for references to other tables or views
  #create a directory for each schema
  # for (schema_name in unique(all_objects$table_schema)) {
  #   dir_name <- paste0("dev/", schema_name)
  #   if (dir.exists(dir_name)) {
  #     unlink(dir_name, recursive = TRUE)
  #   }
  #   dir.create(dir_name, showWarnings = FALSE)
  # }

  #fontawesome codes
  #f0c8 = square
  #f02e = bookmark
  #f111 = circle

  generate_palette <- function(n) {
    if (n <= 12) {
      # Use a qualitative color palette for up to 12 colors
      qual_palette <- colorspace::qualitative_hcl(n, palette = "Set3")
      return(qual_palette)
    } else {
      # Use a continuous color palette for more than 12 colors
      # The sequential_hcl function creates a sequential palette with high contrast
      seq_palette <- colorspace::rainbow_hcl(n)
      return(seq_palette)
    }
  }

  pal <- generate_palette(length(unique(all_objects$table_schema)))

  schema_colors <- setNames(pal, unique(all_objects$table_schema))

  all_objects$view_definition <- replace(all_objects$view_definition, is.na(all_objects$view_definition), paste0(all_objects$table_schema, ".", all_objects$table_name)[is.na(all_objects$view_definition)])

  #make a data frame of nodes for the network
  nodes <- data.frame(
    id = 1:nrow(all_objects),
    label = all_objects$table_name,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = mapply(function(is_view, is_matview){
      ifelse(is_view, "f0c8", ifelse(is_matview, "f02e", "f111"))
    }, all_objects$is_view, all_objects$is_matview),
    icon.color = sapply(all_objects$table_schema, function(table_schema) schema_colors[table_schema]),
    group = all_objects$table_schema,
    title = paste0("<b>", all_objects$table_schema, ".", all_objects$table_name,"</b>","<br>",all_objects$nrow, " rows<br>Fields:<ul><li>", all_objects$fields, "</li></ul>")
  )

  #make a data frame of edges for the network
  edges <- data.frame(from = integer(), to = integer())
  for (i in which(all_objects$is_view | all_objects$is_matview)) {
    def <- all_objects$view_definition[i] %>%
      stringr::str_split(stringr::fixed("\r\n")) %>%
      unlist()

    #save def to file
    # def %>%
    #   c("-- !preview conn=con", .) %>%
    #   writeLines(paste0("dev/", all_objects$table_schema[i], "/", all_objects$table_name[i], ".sql"))

    #find all references to other tables or views
    refs <- sapply(paste0(all_objects$table_schema, ".", all_objects$table_name), function(name) {
      any(grepl(name, def, fixed = TRUE))
    }) %>%
      unname() %>%
      which()

    edges <- rbind(edges, data.frame(from = rep(i, length(refs)), to = refs))
  }

  #check for views with no dependencies
  no_deps <- setdiff(1:nrow(all_objects), unique(c(edges$from, edges$to)))

  edges$arrows <- "to"

  #create directed graph
  # library(igraph)
  # g <- graph(edges, directed = TRUE)
  # plot(g)

  db_name <- DBI::dbGetInfo(conn)$dbname

  network <- visNetwork::visNetwork(nodes, edges,
                                    main = paste(db_name, "database"),
                                    submain = "tables (circle), views (square) & matviews (bookmark)") %>%
    visNetwork::visOptions(selectedBy = "group") %>%
    visNetwork::visLayout(randomSeed = 1) %>%
    visNetwork::addFontAwesome()

  #save the network as an html file
  # htmlwidgets::saveWidget(network, "dev/view_dependencies.html", selfcontained = TRUE)

  return(network)
}
