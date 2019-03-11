devtools::load_all("~/hlprs")
devtools::load_all("~/nbastatstools")
# kickoff to create nba tables
nba_db <- hlprs::connect_to_postgres_db(.dbname = "basketball",
                                        .user = "postgres",
                                        .password = "R@sta!123")

nbastatstools::create_db_schema_nba(
  nba_db,
  create_schema_sql = "~/nbastatstools/inst/sql/create_schema_nba.sql")

nbastatstools::create_db_table_nba_boxscores(
  seasons = 2000:2018,
  db_connection = nba_db,
  create_table_sql = "~/nbastatstools/inst/sql/create_table_nba.boxscores.sql")
  