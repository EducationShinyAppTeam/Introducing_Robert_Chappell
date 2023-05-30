# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Meet Robert Chappell",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Introducing_Robert_Chappell")
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
        menuItem("About Me", tabName = "aboutMe", icon = icon("book")),
        menuItem("Quiz", tabName = "game", icon = icon("gamepad")),
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
          p("My email is rwc5541@psu.edu if you need to contact me."),
          h2("Instructions"),
          p("The following pages are for you to feel free to explore!"),
          tags$ol(
            tags$li("Review facts about me using the About Me Tab."),
            tags$li("Challenge yourself by doing a quick quiz about me.")
          ),
          br(),
          tags$figure(
            align = "center",
            tags$img(
              src = "prof-head.JPG",
              width = 200,
              alt = "Headshot of Robert Chappell"
            ),
          tags$figcaption(
            "Headshot of Robert Chappell"
          ),
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToAboutMe",
              label = "About Me",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Robert Chappell.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 05/19/2023 by RWC.")
          )
        ),
        #### Set up the About Me Page ----
        tabItem(
          tabName = "aboutMe",
          withMathJax(),
          h2("About Me"),
          p("Here are some interesting things about myself:"),
          tags$ul(
            tags$li("I just finished my first year at Penn State. I am a Data 
                    Science Major"),
            tags$li("I play both ice hockey and curling. I play curling for Penn
                    State"),
            tags$li("At home I have a younger brother and my cats Prince and Sable"),
            tags$li("Some of my hobbies include cycling, golfing, and running")
          ),
          p('Some more about me is that I am from Pittsburgh, I am a travel manager
            for the Penn State curling club, and also a member of the sports 
            analytics club. I have played hockey since I was 10, and have two cats
            at home. I enjoy all types of music except for country music, and 
            love to hang out with my friends. Below is a pie chart with my 
            interests.I am very excited to work in this program!'),
          fluidRow(
            column(
              width = 6,
              offset = 0,
              tags$figure(
                align = "center",
                tags$img(
                  src = "cats-pic.jpg",
                  width = 250,
                  alt = "Picture of me with cats"
                ),
                tags$figcaption("A photo with my cats Prince and Sable")
              )
            ),
            column(
              width = 6,
              offset = 0,
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
          plotOutput(
            outputId = "interestsPieChart"
          ),
        ),
        #### Set up an Quiz Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Quiz about me!"),
          p("Answer the three questions about my life, if you need to review, 
            check out the About Me page."),
          br(),
          fluidRow(
            column(
              width = 12,
              offset = 0,
              wellPanel(
                tabsetPanel(
                  id = "quiz",
                  type = "hidden",
                  tabPanel(
                    title = "First Question",
                    value = "Q1",
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "pitts.jpg",
                        width = 300,
                        alt = "View of Pittsburgh"
                      )
                    ),
                    br(),
                    h3("Question 1"),
                    radioButtons(
                      inputId = "answerChoice1",
                      label = "Where is Robert from?",
                      choices = c("Pittsburgh","Toronto","Chicago")
                    )
                  ),
                  tabPanel(
                    title = "Second Question",
                    value = "Q2",
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "curling.jpg",
                        width = 200,
                        alt = "View of Robert with his Curling team"
                      )
                    ),
                    br(),
                    h3("Question 2"),
                    radioButtons(
                      inputId = "answerChoice2",
                      label = "What sport does Robert play at Penn State?",
                      choices = c("Curling","Basketball","Rugby")
                    )
                  ),
                  tabPanel(
                    title = "Third Question",
                    value = "Q3",
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "cats.jpg",
                        width = 200,
                        alt = "Pic of Robert's cat laying together"
                      )
                    ),
                    br(),
                    h3("Question 3"),
                    radioButtons(
                      inputId = "answerChoice3",
                      label = "What is the name of Robert's cats?",
                      choices = c("Prince and Sable",
                                  "Fizz and Rocket", 
                                  "Garfield and Odie")
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "prevPage",
                  label = "Previous Question",
                  size = "large",
                  style = "default"
                ),
              )
            ),
            column(
              width = 4,
              offset = 0,
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "nextPage",
                  label = "Next Question",
                  size = "large",
                  style = "default"
                ),
              )
            ),
            column(
              width = 4,
              offset = 0,
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "submitQuiz",
                  label = "Submit Quiz",
                  size = "large",
                  style = "default"
                )
              )
            )
          ),
        ),
        
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = 'hangingindent',
            "Bailey E (2022). shinyBS: Twitter bootstrap components for shiny. R package version
            0.61.1, <https://CRAN.R-project.org/package=shinyBS>."),
          p(
            class = 'hangingindent',
            "Carey R, Hatfield N (2023). boastUtils: BOAST utilities. R package version 0.1.11.2,
  <https://github.com/EducationShinyAppTeam/boastUtils>."
          ),
          p(class = 'hangingindent',
            "Chang W, Borges Ribeiro B (2021). shinydashboard: Create dashboards with 'Shiny'. R
  package version 0.7.2, <https://CRAN.R-project.org/package=shinydashboard>."),
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J,
            Dipert A, Borges B (2022). shiny: Web application framework for R. R package version
            1.7.4, <https://CRAN.R-project.org/package=shiny>."
          ),
          p(
            class = "hangingindent",
              "Perrier V, Meyer F, Granjon D (2023). shinyWidgets: Custom inputs widgets for shiny.
              R package version 0.7.6, <https://CRAN.R-project.org/package=shinyWidgets>."
          ),
          p(
            class = 'hangingindent',
              "Wickham H. ggplot2: Elegant graphics for data analysis. Springer-Verlag New York,
              2016."
          ),
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
  ## Set up about me button ----
  observeEvent(
    eventExpr = input$goToAboutMe,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = 'pages',
        selected = "aboutMe"
      )
    }
  )
  
  ## Set up Quiz ----
  
  # Current Question Counter----
  currentQuestion <- reactiveVal(1)
  
  ### Next Button ----
  observeEvent(
    eventExpr = input$nextPage,
    handlerExpr = {
      if (currentQuestion() != 3) {
        currentQuestion(currentQuestion() + 1)
        updateTabsetPanel(
          session = session,
          inputId = "quiz",
          selected = paste0("Q", currentQuestion())
        )
      }
    }
  )
  
  ### Previous Button ----
  
  observeEvent(
    eventExpr = input$prevPage,
    handlerExpr = {
      if (currentQuestion() != 1) {
        currentQuestion(currentQuestion() - 1)
        updateTabsetPanel(
          session = session,
          inputId = "quiz",
          selected = paste0("Q", currentQuestion())
        )
      }
    }
  )
  
  #Current Score for the Quiz----
  currentScore <- reactiveVal(0)
  
  ### Submit Button ----
  observeEvent(
    eventExpr = input$submitQuiz,
    handlerExpr = {
      wrongAnswers <- character(0)  # Store the wrong answers
      
      if (!is.null(input$answerChoice1) && input$answerChoice1 == "Pittsburgh") {
        currentScore(currentScore() + 1)
      } else {
        wrongAnswers <- c(wrongAnswers, "Question 1")
      }
      
      if (!is.null(input$answerChoice2) && input$answerChoice2 == "Curling") {
        currentScore(currentScore() + 1)
      } else {
        wrongAnswers <- c(wrongAnswers, "Question 2")
      }
      
      if (!is.null(input$answerChoice3) && input$answerChoice3 == "Prince and Sable") {
        currentScore(currentScore() + 1)
      } else {
        wrongAnswers <- c(wrongAnswers, "Question 3")
      }
      
      sendSweetAlert(
        session = session,
        html = TRUE,
        type = if (currentScore() == 3) {
          "success"
        } else if (currentScore() < 3) {
          "warning"
        },
        title = "Quiz Completed",
        text = paste0(
          "Your Score: ", currentScore(), "/3",
          br(),
          if (length(wrongAnswers) > 0) {
            paste("You got the following question(s) wrong:",
                  br(),
                  paste(wrongAnswers, collapse = " , "))
          } else {
            "Congratulations! You answered all questions correctly."
          }
        )
      )
      
      currentScore(0)  # Reset the score
    }
  )
  
  
  
  ## Set up data viz ----
  
  output$interestsPieChart <- renderPlot(
    expr = {
      intrestData <- data.frame(
        Activity = c("Golf", "Bike", "Hockey", "Running", "Hockey", "Hockey", 
                     "Hockey", "Golf", "Golf", "Bike"))
      
      activity_counts <- table(intrestData$Activity)
      
      activity_percentages <- prop.table(activity_counts) * 100
      
      ggplot(data = intrestData, aes(x = "", fill = Activity)) +
        geom_bar(width = 1, stat = "count") +
        coord_polar("y", start = 0) +
        labs(fill = "Activity") +
        theme(
          text = element_text(size = 18)
        ) +
        scale_fill_manual( 
          values = boastUtils::boastPalette
        ) +
        theme_void()
    },
    alt = "A pie chart with percentages of interests, reflecting Roberts ranking of
    the interests, 40% hockey, 30% golf, 20% biking, 10% running",
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)