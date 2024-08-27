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
#use_python("C:/Users/thort/AppData/Local/Programs/Python/Python312/python.exe", required = TRUE)
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

fetch_media_list <- function() {
  tryCatch({
    conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
    cursor <- conn$cursor()
    
    cursor$execute("SELECT * FROM MTG.MTG_MEDIA_LIST_TEST")
    result <- cursor$fetchall()

    if (length(result) == 0) {
      shinyalert("Error", "No data found in the media list.", type = "error")
      return(NULL)
    }
    
    df <- pd$DataFrame(result, columns = sapply(cursor$description, function(x) x[[0]]))
    ml_df <- as.data.frame(df)
  
    cursor$close()
    conn$close()
    
    return(ml_df)
  }, error = function(e) {
    shinyalert("Error", paste("Error fetching media list:", e$message), type = "error")
    return(NULL)
  })
}

add_entry <- function(category, subcategory, media_name, company_name) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  # Fetch the highest REC_ID
  cursor$execute("SELECT MAX(REC_ID) FROM MTG.MTG_MEDIA_LIST_TEST")
  result <- cursor$fetchone()
  highest_rec_id <- result[[1]]
  
  # Generate the next REC_ID
  if (is.null(highest_rec_id)) {
    highest_rec_id <- 0
  }
  new_rec_id <- highest_rec_id + 1
  
  cursor$execute(
    "INSERT INTO MTG.MTG_MEDIA_LIST_TEST (REC_ID, CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME) VALUES (:rec_id, :category, :subcategory, :media_name, :company_name)",
    dict(rec_id = new_rec_id, category = category, subcategory = subcategory, media_name = media_name, company_name = company_name)
  )
  
  conn$commit()
  cursor$close()
  conn$close()
  
} 

server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  show_clear_filters <- reactiveVal(FALSE)
  filtered_medialist <- reactiveVal(NULL)
  media_list <- reactiveVal(NULL)
  has_subcategories <- reactiveVal(FALSE)
  addbutton_pressed <- reactiveVal(FALSE)
  has_addsubcategories <- reactiveVal(FALSE)
  
  output$is_logged_in <- reactive({
    logged_in()
  })
  outputOptions(output, "is_logged_in", suspendWhenHidden = FALSE)
  
  output$showClearFilters <- reactive({
    show_clear_filters()
  })
  outputOptions(output, "showClearFilters", suspendWhenHidden = FALSE)
  
  output$hasSubcategories <- reactive({
    has_subcategories()
  })
  outputOptions(output, "hasSubcategories", suspendWhenHidden = FALSE)
  
  output$addButtonPressed <- reactive({
    addbutton_pressed()
  })
  outputOptions(output, "addButtonPressed", suspendWhenHidden = FALSE)
  
  output$hasAddSubcategories <- reactive({
    has_addsubcategories()
  })
  outputOptions(output, "hasAddSubcategories", suspendWhenHidden = FALSE)
  
  observeEvent(input$createAccount, {
    username <- input$usernameInput
    password <- input$passInput
    
    if (empty_fields(username,password)) {
      account_creation(username,password)    
      
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
        
        media_list(fetch_media_list())
        categories <- unique(media_list()$CATEGORY)
        updateSelectInput(session, "category", choices = categories)

        output$media_list <- renderDT({
          datatable(media_list())
        })
      }
      
    }
  })
  
  observeEvent(input$category, {
    
    media_list_val <- media_list()
    subcategories <- unique(media_list_val$SUBCATEGORY[media_list_val$CATEGORY == input$category])
    subcategories <- subcategories[!is.na(subcategories)]  # Filter out NA values
    has_subcategories(length(subcategories) > 0)
    updateSelectInput(session, "subcategory", choices = c("", subcategories))
  })
  
  observeEvent(input$filterButton, {
    media_list_val <- media_list()
    
    if (!is.null(input$category) && input$category != "") {
      media_list_val <- media_list_val %>% filter(CATEGORY == input$category)
    }
    
    if (has_subcategories() && !is.null(input$subcategory) && input$subcategory != "") {
      media_list_val <- media_list_val %>% filter(SUBCATEGORY == input$subcategory)
    }
    
    filtered_medialist(media_list_val)
    show_clear_filters(TRUE)
    
    output$media_list <- renderDT({
      datatable(filtered_medialist())
    })
    
  })
  
  observeEvent(input$clearFilters, {
    filtered_medialist(media_list())
    
    # Reset category and subcategory selections
    updateSelectInput(session, "category", selected = "")
    updateSelectInput(session, "subcategory", selected = "")
    
    show_clear_filters(FALSE)
    has_subcategories(FALSE)
    
    output$media_list <- renderDT({
      datatable(media_list())
    })
  })
  observeEvent(input$addButton, {
    addbutton_pressed(TRUE)
    
    categories <- unique(media_list()$CATEGORY)
    updateSelectInput(session, "addCategory", choices = categories)
  })
  
  observeEvent(input$addCategory, {
    media_list_val <- media_list()
    subcategories <- unique(media_list_val$SUBCATEGORY[media_list_val$CATEGORY == input$addCategory])
    subcategories <- subcategories[!is.na(subcategories)]  # Filter out NA values
    has_addsubcategories(length(subcategories) > 0)
    updateSelectInput(session, "addSubcategory", choices = c("", subcategories))
  })
  
  observeEvent(input$cancelAddButton, {
    
    updateSelectInput(session, "addCategory", selected = "")
    updateSelectInput(session, "addSubcategory", selected = "")
    updateTextInput(session, "saddMediaName", value = "")
    updateTextInput(session, "addCompanyName", value = "")
    addbutton_pressed(FALSE)
  })
  
  observeEvent(input$addEntryButton, {
    CATEGORY <- input$addCategory
    SUBCATEGORY <- input$addSubcategory
    MEDIA_NAME <- input$addMediaName
    COMPANY_NAME <- input$addCompanyName
    
    updateSelectInput(session, "addCategory", selected = "")
    updateSelectInput(session, "addSubcategory", selected = "")
    updateTextInput(session, "addMediaName", value = "")
    updateTextInput(session, "addCompanyName", value = "")
    addbutton_pressed(FALSE)
    
    add_entry(CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME)
    
    media_list(fetch_media_list())
  })
}



