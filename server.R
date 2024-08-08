library(shiny)
library(RSQLite)
library(sodium)
library(glue)
library(shinyalert)

account_creation <- function(username, password){
  conn <- dbConnect(RSQLite::SQLite(), dbname = "MTG.db")

  dbExecute(conn, glue("
    CREATE TABLE IF NOT EXISTS mtg_users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username VARCHAR NOT NULL,
      username_hash BLOB NOT NULL,
      password_hash BLOB NOT NULL
      )
    "))
  
  query <- "SELECT COUNT(*) as count FROM mtg_users WHERE username = ?"
  result <- dbGetQuery(conn, query, params = list(username))
  
  if (result$count > 0) {
    shinyalert("Account already exists", "The account for this username already exists.", type = "error")
  } else {
    username_hash <- sodium::password_store(username)
    password_hash <- sodium::password_store(password)
    
    dbExecute(conn, glue("INSERT INTO mtg_users (username, username_hash, password_hash) VALUES (?, ?, ?)"),
              params = list(username, username_hash, password_hash))
  
    shinyalert("Account created", "The account has been successfully created.", type = "success")  
  }
  dbDisconnect(conn)
}

login <- function(username, password){
  conn <- dbConnect(RSQLite::SQLite(), dbname = "MTG.db")
  
  # Retrieve the stored hashes
  query <- glue("SELECT username_hash, password_hash FROM mtg_users WHERE username = ?")
  result <- dbGetQuery(conn, query, params = list(username))
  dbDisconnect(conn)
  
  if (nrow(result) == 0) {
    shinyalert("Login failed!", "The account for this username doesn't exist", type = "error")
    return(FALSE)
  }
  
  stored_username_hash <- result$username_hash[[1]]
  stored_password_hash <- result$password_hash[[1]]
  
  # Verify username and password
  username_verified <- sodium::password_verify(stored_username_hash, username)
  password_verified <- sodium::password_verify(stored_password_hash, password)
  
  if (!username_verified || !password_verified) {
    shinyalert("Error", "Invalid username or password", type = "error")
    return(FALSE)
  }
  
  return(TRUE)

}

empty_fields <- function(username, password) {
  
  if (is.null(username) || username == "" || 
      is.null(password) || password == "") {
    shinyalert("Error", "Username and password cannot be empty", type = "error")
    return(FALSE)
  }
  return(TRUE)
}



server <- function(input, output, session) {
  
  observeEvent(input$createAccount, {
    username <- input$usernameInput
    password <- input$passInput
    
    if (empty_fields(username,password)) {
      account_creation(username,password)    
      
      # Clears the text inputs after successful login
      updateTextInput(session, "usernameInput", value = "")
      updateTextInput(session, "passInput", value = "")
    }  
    

    })
  
  observeEvent(input$loginAccount, {
    username <- input$usernameInput
    password <- input$passInput
    
    if (empty_fields(username,password)) {
      login_successful <- login(username,password)
      if (login_successful){
        shinyalert("Success", "Login successful!", type = "success")
        
      }
    }
  })
}


