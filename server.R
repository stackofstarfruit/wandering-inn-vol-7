source("wanderinginn.R")
library("forcats")

Sys.setlocale('LC_ALL','C')
wanderingserver <- function(input, output) {
  output$overview_plot <- renderPlotly({
    if(input$my_category == "Character Mentions") {
      character_totals_plot
    } else if(input$my_category == "Species Mentions") {
      spec_totals_plot
    }
    else if(input$my_category == "Class Mentions") {
      class_totals_plot
    } else {
      character_totals_plot
    }
  })
  
  output$overview_table <- renderTable({
    if(input$my_category == "Character Mentions") {
      final_char_table <-
        character_table %>%
        rename("Mentions" = char_totals) %>%
        rename("Character" = character_names)
    } else if(input$my_category == "Species Mentions") {
      final_spec_table <-
        total_spec_frame %>%
        rename("Mentions" = spec_totals) %>%
        rename("Species" = species_list)
    } else if(input$my_category == "Class Mentions") {
      final_class_table <-
        total_class_frame %>%
        rename("Mentions" = class_totals) %>%
        rename("Class" = class_list)
    } else{
      final_char_table <-
        character_table %>%
        rename("Mentions" = char_totals) %>%
        rename("Character" = character_names)
    }
  })
  
  output$character_text <- renderText({
    character_count <- 
      character_table %>%
      filter(character_names == input$my_character) %>%
      pull(char_totals)
    paste(input$my_character, "is mentioned in Volume 7", character_count, "times.")
  })
  
  output$character_plot <- renderPlotly({
    char_counts <-
      chapters_df %>%
      pull(words) %>%
      char_finder(input$my_character)
    character_plot <- 
      chapters_df %>%
      mutate("char_counts" = char_counts) %>%
      plot_ly(x = ~char_counts, y = ~title,
              type = "bar") %>%
      layout(title = paste(input$my_character, "Mentions in Volume 7 by Chapter"),
             xaxis = list(title = "Word Count"),
             yaxis = list(title = "",
                          categoryorder = "array",
                          categoryarray = ~title))
  })
  
  output$character_table <- renderTable({
    char_counts_table <-
      chapters_df %>%
      pull(words) %>%
      char_finder("Erin")
    final_char_table <-
      data.frame("Chapter" = short_chapters_df$title, "Mentions" = char_counts_table) %>%
      select(Chapter, Mentions) %>%
      map_df(rev)
  })
  
  output$volume_text <- renderText({
    paste("There are", total_word_count, "words in Volume 7. That's two million, 
          nine thousand, five hundred, and seventy-eight words!")
  })
  
  output$word_plot <- renderPlotly({
    if(input$my_order == "Newest First") {
      word_plot
    } else if(input$my_order == "Oldest First") {
      oldest_chapters_df <- 
        short_chapters_df %>%
        arrange(-X)
      plot_ly(oldest_chapters_df, x = ~counts, y = ~title, 
              type = "bar") %>%
        layout(title = "Volume 7 Chapters by Word Count (Oldest First)",
               xaxis = list(title = "Word Count"),
               yaxis = list(title = "",
                            categoryorder = "array",
                            categoryarray = ~title))
    } else if(input$my_order == "Longest First") {
      longest_chapters_df <- 
        short_chapters_df %>%
        arrange(counts)
      plot_ly(longest_chapters_df, x = ~counts, y = ~title, 
              type = "bar") %>%
        layout(title = "Volume 7 Chapters by Word Count (Longest First)",
               xaxis = list(title = "Word Count"),
               yaxis = list(title = "",
                            categoryorder = "array",
                            categoryarray = ~title))
    } else if(input$my_order == "Shortest First") {
      shortest_chapters_df <- 
        short_chapters_df %>%
        arrange(-counts)
      plot_ly(shortest_chapters_df, x = ~counts, y = ~title, 
              type = "bar") %>%
        layout(title = "Volume 7 Chapters by Word Count (Shortest First)",
               xaxis = list(title = "Word Count"),
               yaxis = list(title = "",
                            categoryorder = "array",
                            categoryarray = ~title))
    }
  })
  
  output$word_table <- renderTable({
    word_table <-
      short_chapters_df %>%
      select(title, counts) %>%
      rename("Chapter" = title) %>%
      rename("Words" = counts) %>%
      map_df(rev)
    if(input$my_order == "Newest First") {
      final_word_table <-
        word_table
    } else if(input$my_order == "Oldest First") {
      final_word_table <-
        word_table %>%
        map_df(rev)
    } else if(input$my_order == "Longest First") {
      final_word_table <-
        word_table %>%
        arrange(-Words)
    } else if(input$my_order == "Shortest First") {
      final_word_table <-
        word_table %>%
        arrange(Words)
    } else {
      final_word_table <-
        word_table
    }
  })
  
  output$calendar_plot <- renderPlot({
    calendar_plot
  })
  
  output$by_month_plot <- renderPlotly({
    by_month_plot
  })
  output$comparison_plot <- renderPlotly({
    comparison_plot
  })
}