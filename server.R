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

empty_fields_add <- function(category, subcategory, media_name, company_name, hide_media_name, has_subcategories) {
  
  # Check if both media name and company name are empty
  if (!hide_media_name && (is.null(media_name) || media_name == "") && (is.null(company_name) || company_name == "")) {
    shinyalert("Error", "Media Name and Company Name cannot both be empty", type = "error")
    return(FALSE)
  }
  
  # Check if media name is empty (if not hidden)
  if (!hide_media_name && (is.null(media_name) || media_name == "")) {
    shinyalert("Error", "Media Name must be filled out", type = "error")
    return(FALSE)
  }
  
  # Check if company name is empty
  if (is.null(company_name) || company_name == "") {
    shinyalert("Error", "Company Name must be filled out", type = "error")
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

duplicate_filter <- function(category, subcategory, media_name, company_name, has_addsubcategories, hide_media_name) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  # Define the base query for duplicate check
  base_query <- "SELECT COUNT(*) FROM MTG.MTG_MEDIA_LIST_TEST WHERE CATEGORY = :1"
  params <- list(category)
  
  # Add conditions based on the category requirements
  if (has_addsubcategories && !hide_media_name) {
    base_query <- paste0(base_query, " AND SUBCATEGORY = :2 AND MEDIA_NAME = :3 AND COMPANY_NAME = :4")
    params <- list(category, subcategory, media_name, company_name)
  } else if (!has_addsubcategories && !hide_media_name) {
    base_query <- paste0(base_query, " AND MEDIA_NAME = :2 AND COMPANY_NAME = :3")
    params <- list(category, media_name, company_name)
  } else if (has_addsubcategories && hide_media_name) {
    base_query <- paste0(base_query, " AND SUBCATEGORY = :2 AND COMPANY_NAME = :3")
    params <- list(category, subcategory, company_name)
  } else if (!has_addsubcategories && hide_media_name) {
    base_query <- paste0(base_query, " AND COMPANY_NAME = :2")
    params <- list(category, company_name)
  }
  
  # Execute the duplicate check query
  cursor$execute(base_query, params)
  result <- cursor$fetchone()
  
  if (result[[1]] > 0) {
    shinyalert("Error", "Duplicate entry detected", type = "error")
    return(FALSE)
  } else {
    return(TRUE)
  }

  cursor$close()
  conn$close()
}

add_entry <- function(category, subcategory, media_name, company_name, has_addsubcategories, hide_media_name) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
   
  # Define the base query for insertion
  base_insert_query <- "INSERT INTO MTG.MTG_MEDIA_LIST_TEST (CATEGORY"
  insert_params <- dict(category = category)
    
  # Add fields based on the category requirements
  if (has_addsubcategories) {
    base_insert_query <- paste0(base_insert_query, ", SUBCATEGORY")
    insert_params$subcategory <- subcategory
  }
    
  if (!hide_media_name) {
    base_insert_query <- paste0(base_insert_query, ", MEDIA_NAME")
    insert_params$media_name <- media_name
  }
    
  base_insert_query <- paste0(base_insert_query, ", COMPANY_NAME) VALUES (:category")
  insert_params$company_name <- company_name
    
  if (has_addsubcategories) {
    base_insert_query <- paste0(base_insert_query, ", :subcategory")
  }
    
  if (!hide_media_name) {
    base_insert_query <- paste0(base_insert_query, ", :media_name")
  }
    
  base_insert_query <- paste0(base_insert_query, ", :company_name)")
    
  # Execute the insertion query
  cursor$execute(base_insert_query, insert_params)
    
  conn$commit()
  shinyalert("Success", "Entry added successfully!", type = "success")
  
  cursor$close()
  conn$close()
}

delete_entries <- function(selected_ids) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  tryCatch({
    for (rec_id in selected_ids) {
      cursor$execute("DELETE FROM MTG.MTG_MEDIA_LIST_TEST WHERE REC_ID = :rec_id", dict(rec_id = rec_id))
    }
    
    conn$commit()
    shinyalert("Deleted", "The selected entries have been deleted.", type = "success")
  }, error = function(e) {
    conn$rollback()
    shinyalert("Error", paste("Deletion failed:", e$message), type = "error")
  }, finally = {
    cursor$close()
    conn$close()
  })
}

save_edit <- function(rec_id, new_media_name, new_company_name) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  tryCatch({
    cursor$execute(
      "UPDATE MTG.MTG_MEDIA_LIST_TEST SET MEDIA_NAME = :1, COMPANY_NAME = :2 WHERE REC_ID = :3",
      list(new_media_name, new_company_name, rec_id)
    )
    conn$commit()
    shinyalert("Success", "Entry updated successfully!", type = "success")
  }, error = function(e) {
    conn$rollback()
    shinyalert("Error", paste("Update failed:", e$message), type = "error")
  }, finally = {
    cursor$close()
    conn$close()
  })
}

server <- function(input, output, session) { 
  
  logged_in <- reactiveVal(FALSE)
  show_clear_filters <- reactiveVal(FALSE)
  filtered_medialist <- reactiveVal(NULL)
  media_list <- reactiveVal(NULL)
  has_subcategories <- reactiveVal(FALSE)
  addbutton_pressed <- reactiveVal(FALSE)
  has_addsubcategories <- reactiveVal(FALSE)
  adding_entry <- reactiveVal(FALSE)
  hide_media_name <- reactiveVal(FALSE)
  editing_entry <- reactiveVal(FALSE)
  selected_row <- reactiveVal(NULL)
  
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
  
  output$addingEntry <- reactive({
    adding_entry()
  })
  outputOptions(output, "addingEntry", suspendWhenHidden = FALSE)
  
  output$hideMediaName <- reactive({
    hide_media_name()
  })
  outputOptions(output, "hideMediaName", suspendWhenHidden = FALSE)
  
  output$editingEntry <- reactive({
    editing_entry()
  })
  outputOptions(output, "editingEntry", suspendWhenHidden = FALSE)
  
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
          datatable(media_list(), selection = list(mode = 'multiple', target = 'row'))
        })
      }
      
    }
})
  
  observeEvent(input$category, {
    
    media_list_val <- media_list()
    subcategories <- unique(media_list_val$SUBCATEGORY[media_list_val$CATEGORY == input$category])
    subcategories <- subcategories[!is.na(subcategories)]  # Filter out NA values
    has_subcategories(length(subcategories) > 0)
    
    if (length(subcategories) > 0) {
      updateSelectInput(session, "subcategory", choices = subcategories, selected = subcategories[1])
    } else {
      updateSelectInput(session, "subcategory", choices = NULL)
    }
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
    
    if (length(subcategories) > 0) {
      updateSelectInput(session, "addSubcategory", choices = subcategories, selected = subcategories[1])
    } else {
      updateSelectInput(session, "addSubcategory", choices = NULL)
    }
    
    hide_media_name(input$addCategory %in% c("OOH", "P4"))
  })
  
  observeEvent(input$cancelAddButton, {
    
    updateTextInput(session, "saddMediaName", value = "")
    updateTextInput(session, "addCompanyName", value = "")
    adding_entry(FALSE)
    addbutton_pressed(FALSE)
  })
  
observeEvent(input$addEntryButton, {
  adding_entry(TRUE)
  
  CATEGORY <- input$addCategory
  SUBCATEGORY <- input$addSubcategory
  MEDIA_NAME <- input$addMediaName
  COMPANY_NAME <- input$addCompanyName
  
  if (!empty_fields_add(CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME, hide_media_name(), has_addsubcategories())) {
    adding_entry(FALSE)
    return()
  }
  
  if (duplicate_filter(CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME, has_addsubcategories(), hide_media_name())) {
    add_entry(CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME, has_addsubcategories(), hide_media_name())
    media_list(fetch_media_list())
    updateSelectInput(session, "addCategory", selected = "")
    updateSelectInput(session, "addSubcategory", selected = NULL)
    updateTextInput(session, "addMediaName", value = "")
    updateTextInput(session, "addCompanyName", value = "")
    adding_entry(FALSE)
    addbutton_pressed(FALSE)
  } else {
    updateSelectInput(session, "addCategory", selected = "")
    updateSelectInput(session, "addSubcategory", selected = NULL)
    updateTextInput(session, "addMediaName", value = "")
    updateTextInput(session, "addCompanyName", value = "")
    adding_entry(FALSE)
    addbutton_pressed(FALSE)
  }
})
  
  observeEvent(input$deleteButton, {
    selected_rows <- input$media_list_rows_selected
    
    if (length(selected_rows) == 0) {
      shinyalert("No selection", "Please select one or more rows to delete.", type = "warning")
    } else {
      # Determine whether to use the filtered or unfiltered list
      if (!is.null(filtered_medialist()) && nrow(filtered_medialist()) > 0) {
        selected_ids <- filtered_medialist()[selected_rows, "REC_ID"]
      } else {
        selected_ids <- media_list()[selected_rows, "REC_ID"]
      }
      
      shinyalert(
        title = "Are you sure?",
        text = "Do you really want to delete the selected entries?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes",
        cancelButtonText = "No",
        callbackR = function(confirm) {
          if (confirm) {
            delete_entries(selected_ids)
            
            # Refresh the media list after deletion
            media_list(fetch_media_list())
            filtered_medialist(NULL)  # Clear the filtered list
            output$media_list <- renderDT({
              datatable(media_list(), selection = "multiple")
            })
          } else {
            shinyalert("Error", "Deletion failed!", type = "error")
          }
        }
      )
    }
  })

observeEvent(input$editButton, {
  selected_rows <- input$media_list_rows_selected
  
  if (length(selected_rows) == 0) {
    shinyalert("No selection", "Please select one row to edit.", type = "warning")
  } else if (length(selected_rows) > 1) {
    shinyalert("Multiple selection", "Please select only one row to edit.", type = "warning")
  } else {
    selected_row(selected_rows)
    editing_entry(TRUE)
    
    # Determine whether to use the filtered or unfiltered list
    if (!is.null(filtered_medialist()) && nrow(filtered_medialist()) > 0) {
      row_data <- filtered_medialist()[selected_rows, ]
    } else {
      row_data <- media_list()[selected_rows, ]
    }
    
    showModal(modalDialog(
      title = "Edit Entry",
      textInput("editMediaName", "Media Name", value = row_data$MEDIA_NAME),
      textInput("editCompanyName", "Company Name", value = row_data$COMPANY_NAME),
      footer = tagList(
        actionButton("saveEditButton", "Save"),
        actionButton("cancelEditButton", "Cancel")
      )
    ))
  }
})

  observeEvent(input$saveEditButton, {
    removeModal()
  
    rec_id <- media_list()[selected_row(), "REC_ID"]
    new_media_name <- input$editMediaName
    new_company_name <- input$editCompanyName
  
    shinyalert(
      title = "Confirm",
      text = "Are you sure you want to save the changes?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      cancelButtonText = "No",
      callbackR = function(confirm) {
        if (confirm) {
          save_edit(rec_id, new_media_name, new_company_name)
          media_list(fetch_media_list())
          filtered_medialist(NULL)  # Clear the filtered list
          output$media_list <- renderDT({
            datatable(media_list(), selection = "multiple")
          })
        }
      }
    )
  })
  
  observeEvent(input$cancelEditButton, {
    removeModal()
    selected_row(NULL)
    editing_entry(FALSE)
  })
}

