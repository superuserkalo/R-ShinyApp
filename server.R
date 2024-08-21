library(shiny)
library(RSQLite)
library(DBI)
library(dplyr)
library(dbplyr)
library(sodium)
library(glue)
library(shinyalert)
library(reticulate)
library(DT)

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
  cursor$execute("SELECT COUNT(*) as count FROM mtg_users WHERE username = :1",list(username))
  result <- as.integer(cursor$fetchone()[[1]])
  
  if (result > 0) {
    shinyalert("Account already exists", "The account for this username already exists.", type = "error")
  } else {
    # Hash password
    password_hash <- sodium::password_store(password)

    # Insert new user
    cursor$execute(
      "INSERT INTO mtg_users (username, password_hash) VALUES (:1, :2)", 
      list(username, password_hash)
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
  cursor$execute(glue("SELECT password_hash FROM mtg_users WHERE username = :1"), list(username))
  result <- cursor$fetchone()
  cursor$close()
  conn$close()
  
  if (length(result) == 0) {
    shinyalert("Login failed!", "Invalid username", type = "error")
    return(FALSE)
  }
  
  stored_password_hash <- result[[1]]
  
  # Verify password
  password_verified <- sodium::password_verify(stored_password_hash, password)
  
  if (!password_verified) {
    shinyalert("Login failed!", "Invalid password", type = "error")
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

fetch_media_list <- function(){
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  cursor$execute("SELECT * FROM MTG.MTG_MEDIA_LIST_TEST")  # Adjust query as per your table structure
  df_ml <- cursor$fetchall()
  cursor$close()
  conn$close()
  
  # Convert to data frame
  ml_df <- as.data.frame(df_ml)
  
  return(ml_df)
}

server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  
  output$is_logged_in <- reactive({
    logged_in()
  })
  outputOptions(output, "is_logged_in", suspendWhenHidden = FALSE)
  
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
        logged_in(TRUE)
      }
      
    }
  })
  output$media_list <- renderDT({
    if (logged_in()) {
      fetch_media_list()  # Fetch and render the media list as a data frame
    }
  })
}


