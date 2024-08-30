library(shiny)
library(bslib)
library(DT)

# Define custom theme
custom_theme <- bs_theme(
  bg = "#f8f9fa",
  fg = "#343a40",
  primary = "#007bff",
  secondary = "#343a40",
  success = "#28a745",
  danger = "#dc3545",
  info = "#17a2b8",
  light = "#f8f9fa",
  dark = "#343a40"
)

ui <- fluidPage(
  conditionalPanel(condition = "!output.is_logged_in", fluidRow(column(
    width = 12,
    offset =  2,
    # Input section
    card(
      style = "display: flex; flex-direction: column; justify-content: center; align-items: center; align: center; width: 60%;",
      textInput("usernameInput", "", placeholder = "Enter username"),
      passwordInput("passInput", "", placeholder = "Enter password")
    ),
    # Button section
    card(
      style = "display: flex; flex-direction: column; justify-content: center; align-items: center; align: center; width: 60%;",
      actionButton("loginAccount", label = "Login", style = "margin-bottom: 10px;"),
      actionButton("createAccount", label = "Create Account")
    )
  ))),
  
  conditionalPanel(
    condition = "output.is_logged_in",
    navbarPage(
      title = "RTR",
      id = "navbar",
      
      tabPanel("Home", sidebarLayout(
        sidebarPanel(
          layout_column_wrap(
            width = 1,
            gap = "1rem",
            card(
              h4("Filter"),
              selectInput("category", "Category", choices = NULL),
              conditionalPanel(
                condition = "output.hasSubcategories",
                selectInput("subcategory", "Subcategory", choices = NULL)
              ),
              actionButton("filterButton", "Filter"),
              conditionalPanel(condition = "output.showClearFilters", actionButton("clearFilters", "Clear Filters"))
            ),
            card(
              h4("Management"),
              actionButton("addButton", "Add"),
              actionButton("editButton", "Edit"),
              actionButton("deleteButton", "Delete")
            ),
            conditionalPanel(condition = "output.rowsSelected", card(
              h4("Selected Rows"),
              textOutput("selectedRowsInfo"),
              actionButton("clearSelection", "Clear Selection")
            )),
            actionButton("logoutButton", "Logout", 
                         style = "position: absolute; bottom: 25px; left: 20px;")
          ),
          width = 3
        ),
        mainPanel(DTOutput("media_list")  # Output for media list)
        )),
        theme = custom_theme
      )
    )
  )
)  