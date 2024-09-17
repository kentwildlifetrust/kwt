#' Add a role to the database
#'
#' @description
#' Creates a new role and records it in an admin.logins table. If the admin.logins table does not exist, it will be created and filled with random passwords.
#' Best practice is to make a new set of credentials for every webapp and every human user.
#'
#' @param conn Connection to a database, which must be using the KWTAdmin credentials.
#' @param username The username for the new credentials.
#'
#' @param group The name of a user group to add the credentials to.
#'
#' @return A list including the username and password of the new credentials
#' @export
#'
add_role <- function(conn, username, group){
  #check if table admin.logins exists
  if(!DBI::dbExistsTable(conn, DBI::Id("admin", "logins"))){
    #create admin.logins
    DBI::dbExecute(conn, "CREATE TABLE admin.logins (kwtid INT PRIMARY KEY, username TEXT, password TEXT)")
    #fill the table with radom passwords
    df <- data.frame(kwtid = 1:20, username = NA_character_, password = paste0("pgiskwt", sample(10000:99999, 20)))
    DBI::dbWriteTable(conn, DBI::Id("admin", "logins"), df, append = TRUE)
  }

  #check that the owner of admin.logins is KWTAdmin
  owner <- DBI::dbGetQuery(conn, "SELECT pg_get_userbyid(relowner) FROM pg_class WHERE relname = 'logins'")$pg_get_userbyid
  stopifnot(owner == "KWTAdmin")
  #check that no other roles have been granted privileges on the table
  roles <- DBI::dbGetQuery(conn, "SELECT grantee FROM information_schema.role_table_grants WHERE table_name = 'logins'")$grantee %>%
    unique()
  stopifnot(roles == "KWTAdmin")

 statement <- glue::glue_sql("DO $$
  DECLARE
    new_username TEXT := {username};
    new_password TEXT;
    existing_password TEXT;
  BEGIN

    -- Check if the new username already exists
    SELECT password INTO existing_password
    FROM admin.logins
    WHERE username = new_username;

    IF existing_password IS NOT NULL THEN
      -- Username already exists, return notice with password
      RAISE NOTICE 'This username already exists, the password is %', existing_password;
    ELSE
      -- Username doesn't exist, create user
      WITH new_credentials AS (
        UPDATE admin.logins
        SET username = new_username
        WHERE kwtid = (
          SELECT kwtid
          FROM admin.logins
          WHERE (username IS NULL OR username = '')
          ORDER BY kwtid
          LIMIT 1
        )
        RETURNING password
      )
      SELECT password INTO new_password FROM new_credentials;


  	EXECUTE 'CREATE USER ' || quote_ident(new_username) || ' WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT NOREPLICATION CONNECTION LIMIT -1 PASSWORD ' || quote_literal(new_password);

    RAISE NOTICE 'Username: %', new_username;
  	RAISE NOTICE 'Password: %', new_password;

    END IF;

  END $$;",
  .con = conn)

 DBI::dbExecute(conn, statement)

 creds <- DBI::dbGetQuery(conn,
                          glue::glue_sql("SELECT * FROM admin.logins
                                         WHERE logins.username = {username}",
                                         .con = conn))
 return(creds %>%
          dplyr::select(username, password) %>%
          as.list())

}
