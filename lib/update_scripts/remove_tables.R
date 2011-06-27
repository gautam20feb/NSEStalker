RemoveTables <- function(
connection)
{
  tables <- dbListTables(connection)
  sapply(tables, function(x) { dbRemoveTable(connection,x)})
}