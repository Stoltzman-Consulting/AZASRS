#` Newest functions
#` @return the sample datas


db_location = "../DB_Application/asrs_temporary.db"

db_connect = function(){
  con = DBI::dbConnect(RSQLite::SQLite(), db_location)
  return(con)
}

db_disconnect = function(con){
  DBI::dbDisconnect(con)
}

pull_nav = function(db_con = db_connect(), from_date='2018-01-01'){
  querystring = paste0("SELECT * FROM nav WHERE date >= '", from_date,"'")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
  return(dat)
}

pull_fundinfo = function(db_con = db_connect()){
  querystring = paste0("SELECT * FROM fundinfo")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
  return(dat)
}
