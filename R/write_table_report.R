#' Write an HTML report about a table
#'
#' @param ref A reference to a table
#' @param conn A database connection
#' @param filepath Path to an html file to write the report as. Default is `table_schema__table_name.html`
#'
#' @return NULL
#' @export
#'
write_table_report <- function(ref, conn = db, filepath = paste0(ref$table_schema, "__", ref$table_name, ".html")){
  rmarkdown::render(
    input = system.file("extdata/table_report_template.Rmd", package = "kwt"),
    output_file = filepath,
    params = list(
      table_schema = ref$table_schema,
      table_name = ref$table_name,
      conn = conn
    )
  )
}
