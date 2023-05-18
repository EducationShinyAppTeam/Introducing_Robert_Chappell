# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Meet Robert Chappell", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "App_Template")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("About Me", tabName = "prerequisites", icon = icon("book")),
        menuItem("Contact Me", tabName = "explore", icon = icon("book-open-reader")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Introducing Robert Chappell"), # This should be the full name.
          p("This is a sample Shiny application for BOAST. This app will tell you
            all about an author for the BOAST program, Robert Chappell."),
          tags$figure(
            align = "center",
            tags$img(
              src = "prof-head.JPG",
              width = 200,
              alt = "Picture of Robert Chappell"
            )
          ),
          br(),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "References will be found on the reference page.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/8/2022 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("About Me"),
          p("Here are some interesting things about myself:"),
          tags$ul(
            tags$li("I just finished my first year at Penn State. I am a Data Science
                    Major"),
            tags$li("I play both ice hockey and curling. I play curling for Penn State"),
            tags$li("At home I have a younger brother and my cat Prince"),
            tags$li("Some of my hobbies include cycling, golfing, and running")
          ),
          p('I am very excited to be apart of this program and excited to work
            with everybody'),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              tags$figure(
                align = "center",
                tags$img(
                  src = "cats-pic.jpg",
                  width = 200,
                  alt = "Picture of me with cats"
                ),
                tags$figcaption("A photo with my cats Prince and Sable")
              )
            ),
            column(
              width = 4,
              offset = 7,
              tags$figure(
                align = "center",
                tags$img(
                  src = "hockey-pic.JPG",
                  width = 200,
                  alt = "Picture of me celebrating my goal with teammate"
                ),
                tags$figcaption("A photo of me (right) celebrating my goal with a
                                teammate")
              )
            )
          ),
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Contact Me"),
          textInput(
            inputId = 'text1',
            label = 'Enter Message Here',
            value = "",
            width = '100%',
            placeholder = 'Type the message here'
          ),
          #### verbatimTextOutput()
          p("Need to research a way to store input from the user, so I would be
            able to see them when they sumbit.")
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("I used the style guide along with these references:"),
          p(
            class = "hangingindent",
            "shiny: Create a text input control.
            (v1.7.4). [R package]. Available from
            https://shiny.posit.co/r/reference/shiny/1.7.4/textinput"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app will tell you all about Robert Chappell"
      )
    }
  )
  
  
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)