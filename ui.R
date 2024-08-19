library(shiny)
library(bslib)

# Define custom theme
custom_theme <- bs_theme(
  bg = "#272938",
  fg = "#9DAAB6",
  primary = "#007bff",
  secondary = "#9DAAB6",
  success = "#28a745",
  danger = "#dc3545",
  info = "#17a2b8",
  light = "#f8f9fa",
  dark = "#343a40"
)

ui <- fluidPage(

  conditionalPanel(
    condition = "!output.is_logged_in",
    fluidRow(
      column(
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
          actionButton("loginAccount", label = "Login"),
          actionButton("createAccount", label = "Create Account"),
          )
        )
      )
  ),
  
  conditionalPanel(
    condition = "output.is_logged_in",
    fluidRow(
      column(
        width = 12,
        offset = 2,

        card(
          style = "display: flex; flex-direction: column; justify-content: center; align-items: center; align: center; width: 60%;",
          h3("Welcome to the main page!"),
          p("This is the content displayed after a successful login."),
          actionButton("logout", label = "Logout")
          )
        )
      )
    ),
  
  # Loads custom theme
  theme = custom_theme
)