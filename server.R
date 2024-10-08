library(shiny)
library(DBI)
library(dplyr)
library(dbplyr)
library(sodium)
library(glue)
library(shinyalert)
library(reticulate)
library(DT)
library(stringr)
library(openxlsx)

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

trim_whitespace <- function(input_string) {
  str_trim(input_string, side = "both")
}   

no_whitespaces <- function(input_string) {
  if (str_detect(input_string, "\\s")) {
    return(FALSE)
  }else {
    return(TRUE)
  }
}

empty_fields <- function(username, password) {
  
  if (is.null(username) || username == "" || 
      is.null(password) || password == "") {
    shinyalert("Error", "Username and password cannot be empty", type = "error")
    return(FALSE)
  }
  return(TRUE)
}

empty_fields_add <- function(category, subcategory, media_name, company_name, hide_media_name) {
  
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

duplicate_filter <- function(category, subcategory, media_name, company_name, hide_media_name, has_addsubcategories, editing_entry = FALSE, exclude_rec_id = NULL) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  # Start with a basic query that compares lower case values and selects REC_ID
  query <- "SELECT REC_ID FROM MTG.MTG_MEDIA_LIST_TEST WHERE LOWER(COMPANY_NAME) = :company_name AND LOWER(CATEGORY) = :category"
  params <- list(company_name = tolower(company_name), category = tolower(category))
  
  if (has_addsubcategories) {
    query <- paste(query, "AND LOWER(SUBCATEGORY) = :subcategory")
    params$subcategory <- tolower(subcategory)
  }
  
  # Add conditions based on input
  if (!hide_media_name) {
    query <- paste(query, "AND LOWER(MEDIA_NAME) = :media_name")
    params$media_name <- tolower(media_name)
  }
  
  if (editing_entry && !is.null(exclude_rec_id)) {
    query <- paste(query, "AND REC_ID != :exclude_rec_id")
    params$exclude_rec_id <- exclude_rec_id
  }

  # Execute the query
  cursor$execute(query, params)
  result <- as.integer(cursor$fetchone()[[1]])
  cursor$close()
  conn$close()
  
  # Check if similar entries exist
  if (length(result) > 0) {
    similar_entries_rec_ids <- sapply(result, function(x) x[[1]])  # Extract the first element of each list
    shinyalert(title = "Duplicate entry found", text = paste("A similar entry already exists! REC_ID(s):", paste(similar_entries_rec_ids, collapse = ", ")), type = "error")
    return(TRUE)  # Duplicate exists
  } else {
    return(FALSE)  # No duplicate
  }
}

add_entry <- function(category, subcategory, media_name, company_name, has_addsubcategories, hide_media_name, username) {
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
  
  # Get the REC_ID of the newly inserted row
  cursor$execute("SELECT MAX(REC_ID) FROM MTG.MTG_MEDIA_LIST_TEST")
  rec_id <- cursor$fetchone()[[1]]
    
  conn$commit()
  shinyalert("Success", "Entry added successfully!", type = "success")
  
  log_query <- "INSERT INTO MTG.MTG_MEDIA_LIST_NEW_MEDIA_TEST (REC_ID, CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME, USER_NAME, DAT_INSERT) VALUES (:1, :2, :3, :4, :5, :6, SYSDATE)"
  log_params <- list(rec_id, category, subcategory, media_name, company_name, username)
  cursor$execute(log_query, log_params)
  conn$commit()
  
  cursor$close()
  conn$close()
}

delete_entries <- function(selected_ids, username) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  tryCatch({
    for (rec_id in selected_ids) {
      # Log the deletion
      log_query <- "INSERT INTO MTG.MTG_MEDIA_LIST_DELETED_TEST (REC_ID, DAT_DELETED, USER_NAME) VALUES (:1, SYSDATE, :2)"
      cursor$execute(log_query, list(rec_id, username))
      
      # Delete the entry
      delete_query <- "DELETE FROM MTG.MTG_MEDIA_LIST_TEST WHERE REC_ID = :rec_id"
      cursor$execute(delete_query, dict(rec_id = rec_id))
      
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

save_edit <- function(rec_id, new_media_name, new_company_name, username) {
  conn <- oracledb$connect(user = DB_username, password = DB_password, dsn = DB_dsn)
  cursor <- conn$cursor()
  
  tryCatch({
    # Fetch the old values
    cursor$execute("SELECT CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME FROM MTG.MTG_MEDIA_LIST_TEST WHERE REC_ID = :1", list(rec_id))
    old_values <- cursor$fetchone()
    
    category_old <- old_values[[1]]
    subcategory_old <- old_values[[2]]
    media_name_old <- old_values[[3]]
    company_name_old <- old_values[[4]]
    
    # Update the entry
    cursor$execute(
      "UPDATE MTG.MTG_MEDIA_LIST_TEST SET MEDIA_NAME = :1, COMPANY_NAME = :2 WHERE REC_ID = :3",
      list(new_media_name, new_company_name, rec_id)
    )
    conn$commit()
    shinyalert("Success", "Entry updated successfully!", type = "success")
    
    # Log the change
    log_query <- "INSERT INTO MTG.MTG_MEDIA_LIST_CHANGES_TEST (REC_ID, CATEGORY_OLD, SUBCATEGORY_OLD, MEDIA_NAME_OLD, COMPANY_NAME_OLD, MEDIA_NAME_NEW, COMPANY_NAME_NEW, USER_NAME, DAT_CHANGE) VALUES (:1, :2, :3, :4, :5, :6, :7, :8, SYSDATE)"
    log_params <- list(rec_id, category_old, subcategory_old, media_name_old, company_name_old, new_media_name, new_company_name, username)
    cursor$execute(log_query, log_params)
    conn$commit()
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
  logged_in_user <- reactiveVal(NULL)
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
  selected_row_data <- reactiveVal(NULL)
  file_type <- reactiveVal(NULL)
  
  output$rowsSelected <- reactive({
    length(input$media_list_rows_selected) > 0
  })
  outputOptions(output, "rowsSelected", suspendWhenHidden = FALSE)
  
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("media_list", switch(file_type(), "CSV" = "csv", "Excel" = "xlsx"), sep = ".")
    },
    content = function(file) {
      if (file_type() == "CSV") {
        write.csv(media_list(), file, row.names = FALSE)
      } else if (file_type() == "Excel") {
        openxlsx::write.xlsx(media_list(), file, rowNames = FALSE)
      }
    }
  )
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      "media_list.csv"
    },
    content = function(file) {
      file_type("CSV")
      write.csv(media_list(), file, row.names = FALSE)
    }
  )
  
  output$downloadExcel <- downloadHandler(
    filename = function() {
      "media_list.xlsx"
    },
    content = function(file) {
      file_type("Excel")
      openxlsx::write.xlsx(media_list(), file, rowNames = FALSE)
    }
  )
  
  observeEvent(input$createAccount, {
    username <- input$usernameInput
    password <- input$passInput
    
    if (no_whitespaces(username) && no_whitespaces(username)) {
      if (empty_fields(username,password)) {
        account_creation(username,password)    
      
        updateTextInput(session, "usernameInput", value = "")
        updateTextInput(session, "passInput", value = "")
      }  
    }else {
      shinyalert(title = "Error",text = "No whitespaces allowed. Please remove spaces and try again.",type = "error")
    }
  })
  
  observeEvent(input$loginAccount, {
    username <- input$usernameInput
    password <- input$passInput
    
    if (no_whitespaces(username) && no_whitespaces(username)) {
      if (empty_fields(username, password)) {
        login_successful <- login(username,password)
        if (login_successful){
          shinyalert("Success", "Login successful!", type = "success")
          logged_in(TRUE)
          logged_in_user(username)
        
          media_list(fetch_media_list())
          categories <- unique(media_list()$CATEGORY)
          updateSelectInput(session, "category", choices = categories)

          output$media_list <- renderDT({
            datatable(media_list(), selection = list(mode = 'multiple', target = 'row'))
          })
        }
      }
    }else {
      shinyalert(title = "Error",text = "No whitespaces allowed. Please remove spaces and try again.",type = "error")
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
    updateSelectInput(session, "category", selected = NULL)
    updateSelectInput(session, "subcategory", selected = NULL)
    
    show_clear_filters(FALSE)
    
    
    output$media_list <- renderDT({
      datatable(media_list())
    })
  })
  
  observeEvent(input$addButton, {
    addbutton_pressed(TRUE)
    
    categories <- unique(media_list()$CATEGORY)
    updateSelectInput(session, "addCategoryModal", choices = categories)
    
    showModal(modalDialog(
      title = "Add Entry",
      selectInput("addCategoryModal", "Category", choices = unique(media_list()$CATEGORY)),
      conditionalPanel(
        condition = "output.hasAddSubcategories",
        selectInput("addSubcategoryModal", "Subcategory", choices = NULL)
      ),
      conditionalPanel(
        condition = "!output.hideMediaName",
        textInput("addMediaNameModal", "Media Name", value = "")
      ),
      textInput("addCompanyNameModal", "Company Name", value = ""),
      footer = tagList(
        actionButton("saveAddButton", "Save"),
        actionButton("cancelAddButtonModal", "Cancel")
      )
    ))
    
    # Ensure the subcategory is set correctly when the modal is shown
    if (length(categories) > 0) {
      media_list_val <- media_list()
      subcategories <- unique(media_list_val$SUBCATEGORY[media_list_val$CATEGORY == categories[1]])
      subcategories <- subcategories[!is.na(subcategories)]  # Filter out NA values
      has_addsubcategories(length(subcategories) > 0)
      
      if (length(subcategories) > 0) {
        updateSelectInput(session, "addSubcategoryModal", choices = subcategories, selected = subcategories[1])
      } else {
        updateSelectInput(session, "addSubcategoryModal", choices = NULL)
      }
    }
  })
  
  observeEvent(input$addCategoryModal, {
    media_list_val <- media_list()
    subcategories <- unique(media_list_val$SUBCATEGORY[media_list_val$CATEGORY == input$addCategoryModal])
    subcategories <- subcategories[!is.na(subcategories)]  # Filter out NA values
    has_addsubcategories(length(subcategories) > 0)
    
    if (length(subcategories) > 0) {
      updateSelectInput(session, "addSubcategoryModal", choices = subcategories, selected = subcategories[1])
    } else {
      updateSelectInput(session, "addSubcategoryModal", choices = NULL)
    }
    
    hide_media_name(input$addCategoryModal %in% c("OOH", "P4"))
  })
  
  observeEvent(input$cancelAddButtonModal, {
    removeModal()
    updateTextInput(session, "addMediaName", value = "")
    updateTextInput(session, "addCompanyName", value = "")
    adding_entry(FALSE)
    addbutton_pressed(FALSE)
  })
 
  observeEvent(input$saveAddButton, {
    CATEGORY <- trim_whitespace(input$addCategoryModal)
    SUBCATEGORY <- trim_whitespace(input$addSubcategoryModal)
    MEDIA_NAME <- trim_whitespace(input$addMediaNameModal)
    COMPANY_NAME <- trim_whitespace(input$addCompanyNameModal)
    
    if (!empty_fields_add(CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME, hide_media_name())) {
      return()
    }
    
    # Check for duplicates and similar entries
    if (!duplicate_filter(CATEGORY,SUBCATEGORY, MEDIA_NAME, COMPANY_NAME,hide_media_name(),has_addsubcategories())) {
      add_entry(CATEGORY, SUBCATEGORY, MEDIA_NAME, COMPANY_NAME, has_addsubcategories(), hide_media_name(), logged_in_user())
      filtered_medialist(NULL)  # Clear the filtered list
      show_clear_filters(FALSE)
      media_list(fetch_media_list())
      output$media_list <- renderDT({
        datatable(media_list(), selection = "multiple")
      })
      updateTextInput(session, "addMediaNameModal", value = "")
      updateTextInput(session, "addCompanyNameModal", value = "")
      adding_entry(FALSE)
      addbutton_pressed(FALSE)
      removeModal()
    } else {
      updateTextInput(session, "addMediaNameModal", value = "")
      updateTextInput(session, "addCompanyNameModal", value = "")
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
            delete_entries(selected_ids, logged_in_user())
            
            # Refresh the media list after deletion
            media_list(fetch_media_list())
            show_clear_filters(FALSE)
            output$media_list <- renderDT({
              datatable(media_list(), selection = "multiple")
            })
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
    
    selected_row_data(row_data)
    hide_media_name(row_data$CATEGORY %in% c("OOH", "P4"))
  
    showModal(modalDialog(
      title = "Edit Entry",
            conditionalPanel(
        condition = "!output.hideMediaName",
        textInput("editMediaName", "Media Name", value = row_data$MEDIA_NAME)
      ),
      textInput("editCompanyName", "Company Name", value = row_data$COMPANY_NAME),
      footer = tagList(
        actionButton("saveEditButton", "Save"),
        actionButton("cancelEditButton", "Cancel")
        )
      ))
    }
  })

  observeEvent(input$saveEditButton, {
    row_data <- selected_row_data()
    has_addsubcategories <- has_addsubcategories()
    
      rec_id <- row_data$REC_ID
      new_media_name <- trim_whitespace(input$editMediaName)
      new_company_name <- (input$editCompanyName)
      
      # Check if any field is empty
      if (!empty_fields_add(row_data$CATEGORY, row_data$SUBCATEGORY, new_media_name, new_company_name, hide_media_name())) {
        return()
      }
      
      if (!duplicate_filter(row_data$CATEGORY, row_data$SUBCATEGORY, new_media_name, new_company_name, hide_media_name(), has_addsubcategories(), editing_entry = TRUE, exclude_rec_id = rec_id)) {
        shinyalert(
          title = "Confirm",
          text = "Are you sure you want to save the changes?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes",
          cancelButtonText = "No",
          callbackR = function(confirm) {
            if (confirm) {
              save_edit(rec_id, new_media_name, new_company_name, logged_in_user())
              media_list(fetch_media_list())
              filtered_medialist(NULL)  # Clear the filtered list
              show_clear_filters(FALSE)
              output$media_list <- renderDT({
                datatable(media_list(), selection = "multiple")
              })
              removeModal()  # Close the modal after successful save
            }
          }
        )
      }
    })
  
  
  observeEvent(input$cancelEditButton, {
    removeModal()
    selected_row(NULL)
    editing_entry(FALSE)
  })

  output$selectedRowsInfo <- renderText({
    selected_rows <- input$media_list_rows_selected
    if (length(selected_rows) > 0) {
      selected_ids <- if (!is.null(filtered_medialist()) && nrow(filtered_medialist()) > 0) {
        filtered_medialist()[selected_rows, "REC_ID"]
      } else {
        media_list()[selected_rows, "REC_ID"]
      }
      paste("Selected Rows:", length(selected_rows), "REC_IDs:", paste(selected_ids, collapse = ", "))
    
    } else {
      ""
    }
  })

  observeEvent(input$clearSelection, {
    selectRows(dataTableProxy("media_list"), NULL)
  })

  observeEvent(input$logoutButton, {
    logged_in(FALSE)         # Set logged_in to FALSE
    logged_in_user(NULL)     # Clear the logged_in_user value
  
    updateTextInput(session, "usernameInput", value = "")
    updateTextInput(session, "passInput", value = "")
  
    shinyalert("Logged out", "You have been logged out successfully.", type = "info")

  })
  
  observeEvent(input$downloadButton, {
    showModal(modalDialog(
      title = "Download Data",
      "Choose the file format:",
      footer = tagList(
        actionButton("downloadCSV", "CSV", style = "margin-left: 5px;"),
        actionButton("downloadExcel", "Excel"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$downloadCSV, {
    file_type("CSV")
    removeModal()
    output$downloadData <- downloadHandler(
      filename = function() {
        "media_list.csv"
      },
      content = function(file) {
        write.csv(media_list(), file, row.names = FALSE)
      }
    )
    outputOptions(output, "downloadData", suspendWhenHidden = FALSE)
    session$sendCustomMessage(type = "shiny-download", message = list(id = "downloadData"))
  })
  
  observeEvent(input$downloadExcel, {
    file_type("Excel")
    removeModal()
    output$downloadData <- downloadHandler(
      filename = function() {
        "media_list.xlsx"
      },
      content = function(file) {
        openxlsx::write.xlsx(media_list(), file, rowNames = FALSE)
      }
    )
    outputOptions(output, "downloadData", suspendWhenHidden = FALSE)
    session$sendCustomMessage(type = "shiny-download", message = list(id = "downloadData"))
  })
}


