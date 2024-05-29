# edit_vals <- function(ref, backup = T, conn = db){
#   if (ref$type != "table") {
#     stop("This function only works with tables")
#   }
#
#   #read in the first 100 rows
#   data <- start_query(ref, conn = conn) %>%
#     slice(1:100) %>%
#     run_query()
# }
