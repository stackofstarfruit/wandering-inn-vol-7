source("wanderinginn.R")

  page_one <- tabPanel(
    title = "Chapter Overview",
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                 inputId = "my_order",
                 label = "Choose a display order.",
                 choices = c("Newest First", "Oldest First", 
                             "Longest First", "Shortest First"
                 )
               )
             ),
             mainPanel(
               p(textOutput(outputId = "volume_text")),
               tabsetPanel(type = "tabs",
                           tabPanel(
                             title = "Plot View",
                             plotlyOutput(outputId = "word_plot", height = "1000")
                           ),
                           tabPanel(
                             title = "Table View",
                             tableOutput(outputId = "word_table")
                           )
               ),
             )
           )
  )
  page_two <- tabPanel(
    title = "Characters / Species / Classes by Total Mentions",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "my_category",
                 label = "Choose a category.",
                 choices = c("Character Mentions", "Species Mentions",
                             "Class Mentions"
                 )
               )
             ),
             mainPanel(
               p("Choose a category to visualize on the left."),
               tabsetPanel(type = "tabs",
                           tabPanel(
                             title = "Plot View",
                             plotlyOutput(outputId = "overview_plot", height = "1000")
                           ),
                           tabPanel(
                             title = "Table View",
                             tableOutput(outputId = "overview_table")
                           )
               )
             )
           )
  )
  page_three <- tabPanel(
    title = "Character Appearances by Chapter",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "my_character",
                   label = "Choose a character.",
                   choices = character_names
                 )
               ),
               mainPanel(
                 p(textOutput(outputId = "character_text")),
                 tabsetPanel(type = "tabs",
                   tabPanel(
                     title = "Plot View",
                     plotlyOutput(outputId = "character_plot", height = "1000")
                   ),
                   tabPanel(
                     title = "Table View",
                     tableOutput(outputId = "character_table")
                   )
                 )
               )
             )
    )
  page_four <- tabPanel(
    title = "Length Comparison", 
             p("Here's how Volume 7 stacks up against other long works of literature."),
             plotlyOutput(outputId = "comparison_plot")
  )
  page_five <- tabPanel(
    title = "Chapters by Date",
    p(paste("Volume 7 was published from", format(min_date, "%B %d, %Y"), "to", 
                                           format(max_date, "%B %d, %Y"), 
            "meaning Volume 7 took", time_span, "days to write.")),
    tabsetPanel(type = "tabs",
      tabPanel(
        title = "Calendar View",
        p("Here's how many words were written on each day of the month."),
        plotOutput(outputId = "calendar_plot")
      ),
      tabPanel(
        title = "Pie Chart View",
        p("Here's how the months stack up in terms of writing volume."),
        plotlyOutput(outputId = "by_month_plot")
      )
    )
  )
ui <- navbarPage(
  "The Wandering Inn, Volume 7: Visualized",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)