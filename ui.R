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
  theme = custom_theme,
  tags$head(
    tags$style(
      HTML("
        .centered-content {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          height: 100vh;
        }
        .rtr-image-login {
          width: 20%;
          height: auto;
          margin-bottom: 20px;
        }
        #login-screen-card {
          padding: 15px;
          border-radius: 5px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          margin-bottom: 15px;
          width: 42%;
        }
        .card .shiny-input-container {
          margin-bottom: 10px; 
        }
        .shiny-input-container {
          margin-bottom: 10px; 
        }
      ")
    )
  ),
  
  conditionalPanel(
    condition = "!output.is_logged_in",
    fluidRow(
      column(
        width = 12,
        class = "centered-content",
        img(src = "company_image.png", class = "rtr-image-login"),
        card(
          id = "login-screen-card",
          style = "display: flex; flex-direction: column; justify-content: center; align-items: center; align: center;",
          textInput("usernameInput", "", placeholder = "Enter username"),
          passwordInput("passInput", "", placeholder = "Enter password"),
          actionButton("loginAccount", label = "Login"),
          actionButton("createAccount", label = "Create Account")
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.is_logged_in",
    navbarPage(
      title = img(src = "company_image.png", height = "50px"),
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
              conditionalPanel(condition = "output.showClearFilters", actionButton("clearFilters", "Clear Filters")),
              
              h4("Management"),
              actionButton("addButton", "Add"),
              actionButton("editButton", "Edit"),
              actionButton("deleteButton", "Delete"),

              actionButton("logoutButton", "Logout", class = "logout-button",
                           style = "position: absolute; bottom: -50px; left: 30%")
            ),
            conditionalPanel(condition = "output.rowsSelected", card(
              h4("Selected Rows"),
              textOutput("selectedRowsInfo"),
              actionButton("clearSelection", "Clear Selection")
            ))
          ),
          width = 3
        ),
        mainPanel(DTOutput("media_list")  # Output for media list)
        )
      )
      ),

    )
  )
)