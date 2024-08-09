library(shiny)
library(shinyjs)
library(RSQLite)
library(DBI)
library(dplyr)
library(dbplyr)
library(sodium)
library(glue)
library(shinyalert)
library(reticulate)

source("utils.R")

# Python part
use_python("/opt/homebrew/bin/python3", required = TRUE)
dict <- reticulate::dict
oracledb <- import("oracledb")
pd <- import("pandas")

account_creation <- function(username, password){
    conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
    cursor <- conn$cursor()
    
    # Check if username already exists
    cursor$execute("SELECT COUNT(*) as count FROM mtg_users WHERE USERNAME = :1",list(username))
    result <- as.integer(cursor$fetchone()[[1]])
    
    if (result > 0) {
      shinyalert("Account already exists", "The account for this username already exists.", type = "error")
    } else {
      # Hash username and password
      username_hash <- sodium::password_store(username)
      password_hash <- sodium::password_store(password)
      
      username_hash_hex <- sodium::bin2hex(as.raw(username_hash))
      password_hash_hex <- sodium::bin2hex(as.raw(password_hash))
      
      # Insert new user
      cursor$execute(
        "INSERT INTO mtg_users (username, username_hash, password_hash) VALUES (:1, :2, :3)", 
        list(username, username_hash_hex, password_hash_hex)
      )
      conn$commit()
      shinyalert("Account created", "The account has been successfully created.", type = "success")  
    }
  cursor$close()
  conn$close()
}

login <- function(username, password){
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  # Retrieve the stored hashes
  cursor$execute(glue("SELECT USERNAME_HASH, PASSWORD_HASH FROM mtg_users WHERE USERNAME = :1"), list(username))
  result <- cursor$fetchone()
  cursor$close()
  conn$close()
  
  if (length(result) == 0) {
    shinyalert("Login failed!", "The account for this username doesn't exist", type = "error")
    return(FALSE)
  }
  
  stored_username_hash <- result$username_hash[[1]]
  stored_password_hash <- result$password_hash[[2]]
  
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
        runjs('$("#login_screen").hide(); $("#main_page").show();')
      }
    }
  })
}


