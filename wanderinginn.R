library(rvest)
library(tidyverse)
library(NLP)
library(plotly)
library(lubridate)
library(ragg)
library(extrafont)

chapters_df <- 
  read.csv("wandering_table.csv") %>%
  mutate(date = as.Date(date))
short_chapters_df <- 
  chapters_df %>%
  select(-words)

#### Building chapters_df from scratch
# get_page <- read_html("https://wanderinginn.com/table-of-contents/")
# filter_odd <- c(FALSE, TRUE)
# chapter_title_list <-
#   get_page %>%
#   html_nodes("p") %>%
#   html_nodes("a") %>%
#   html_text()
# 
# chapter_url_list <-
#   get_page %>%
#   html_nodes("p") %>%
#   html_nodes("a") %>%
#   html_attr("href")
# 
# x_name <- "title"
# y_name <- "url"
# 
# chapters_df <- data.frame(chapter_title_list,chapter_url_list)
# names(chapters_df) <- c(x_name,y_name)
# 
# get_volume <- function(x) {
#   substr(x, 1, 1)
# }
# 
# vol <- vector()
# chapter_number <- nrow(chapters_df)
# for(i in 1:chapter_number) {
#   first_char <- get_volume(chapters_df$title[i])
#   condition_check <- str_detect(first_char, "\\d")
#   if(is.na(condition_check)) {
#     print("error")
#     print(i)
#   }
#   else if(condition_check == FALSE) {
#     j = i
#     while(condition_check == FALSE) {
#       first_char <- get_volume(chapters_df$title[j - 1])
#       condition_check <- str_detect(first_char, "\\d")
#       j = j - 1
#     }
#   }
#   vol[i] <- first_char
# }
# 
# chapters_df$volume = vol
# chapters_df <- 
#   chapters_df %>%
#   filter(volume == 7)

# get_words <- function(my_url) {
#   result <- 
#     read_html(my_url) %>%
#     html_nodes("div") %>%
#     html_nodes(xpath = '//*[@class="entry-content"]') %>%
#     html_text() %>%
#     str_replace_all("\n", " ") %>%
#     str_squish()
# }
# 
# word_count <- function(x) {
#   sapply(strsplit(x, " "), length)
# }
# 
# get_words <- Vectorize(get_words)
# word_count <- Vectorize(word_count)
# 
# words <- 
#   chapters_df %>%
#   pull("url") %>%
#   get_words()
# 
# word_counts <-
#   words %>%
#   word_count() - 4
# 
# chapters_df$counts <- word_counts
# chapters_df$words <- words
# 
# write.csv(chapters_df, "[FILENAME].csv")
# 
# #### END OF chapters_df BUILD
# 
# get_dates <- function(my_url) {
#   read_html(my_url) %>%
#   html_nodes("time.entry-date") %>%
#   html_text()
# }
# get_dates <- Vectorize(get_dates)
# get_vec_dates <- function(x) {
#   data.frame(get_dates(x)) %>%
#   pull(1) %>%
#   as.Date(format = "%B %d, %Y")
# }
#   
# chapters_df <-
#   chapters_df %>%
#   mutate("date" = get_vec_dates(url))

char_finder <- function(x, y) {
  sapply(str_extract_all(x, y), length)
}
char_finder <- Vectorize(char_finder)

#### Building character list from scratch
# character_names <- c("Erin", "Ryoka", "Mrsha", "Lyonette")
# 
get_counts <- function(x) {
  num_vec <-
    chapters_df %>%
    pull(words) %>%
    char_finder(x)
  final_num <- sum(num_vec)
}
get_counts <- Vectorize(get_counts)
# 
# char_totals <-
#   character_names %>%
#   get_counts()
# total_char_frame <- data.frame(character_names, char_totals)
# 
# write.csv(total_char_frame, "new_char_counts.csv")

character_table <- read.csv("char_counts.csv")
character_names <- 
  character_table %>%
  pull(character_names)

word_plot <- 
  plot_ly(short_chapters_df, x = ~counts, y = ~title, 
          type = "bar") %>%
  layout(title = "Volume 7 Chapters by Word Count (Newest First)",
         xaxis = list(title = "Word Count"),
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~title))
total_word_count <- sum(short_chapters_df$counts)

titles <- c("Volume 7 (The Wandering Inn)", "Worm", "Mother of Learning", "A Storm of Swords (ASOIAF)", "Inheritance (Eragon)", "Order of the Phoenix (Harry Potter)", "The Fellowship of the Ring (LOTR)", "In Search of Lost Time", "War and Peace", "Infinite Jest", "The Stand", "It", "Harry Potter (All Books)", "ASIOAF (All Books)", "Lord of the Rings (All Books)", "Inheritance Cycle (All Books)")
title_counts <- c(total_word_count, 1680000, 823563, 414792, 280712, 257045, 187790, 1267069, 587287, 543709, 467812, 441156, 1084200, 1770000, 455200, 906077)

comparison_data <- data.frame(titles, title_counts)
comparison_plot <-
  comparison_data %>%
  arrange(-title_counts) %>%
  plot_ly(x = ~titles, y = ~title_counts,
          type = "bar") %>% 
  layout(title = "Volume 7 v. Other Long Literary Works",
         xaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~title),
         yaxis = list(title = "Word Count"))

character_totals_plot <-
  character_table %>%
  arrange(char_totals) %>%
  plot_ly(y = ~character_names, x = ~char_totals,
          type = "bar") %>% 
  layout(title = "Characters by Total Mentions",
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~title),
         xaxis = list(title = "Mentions"))

date_plot <-
  short_chapters_df %>%
  plot_ly(x = ~date, y = ~counts,
          type= "bar")
min_date <- 
  short_chapters_df %>%
  slice_head(n = 1) %>%
  pull(date)
max_date <-
  short_chapters_df %>%
  slice_tail(n = 1) %>%
  pull(date)
time_span <- max_date - min_date

calendar_data <- 
  short_chapters_df %>%
  complete(date = seq(ymd("2020-01-01"), 
                      ymd("2020-12-31"), 
                      "day")) %>%
  mutate(weekday = wday(date, label = T, week_start = 1), 
         month = month(date, label = T, abbr = F),
         week = isoweek(date),
         day = day(date))
calendar_data <- 
  calendar_data %>%
  mutate(week = case_when(month == "December" & week == 1 ~ 53,
                          month == "January" & week %in% 52:53 ~ 0,
                          TRUE ~ week)
  ) %>%
  mutate("short_weekday" = substring(weekday, 1, 1))
theme_calendar <- function(){
  theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(family = "Arial"),
        
        panel.grid = element_blank(),
        panel.background = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Arial", face = "bold", size = 15),
        
        legend.position = "top",
        legend.text = element_text(family = "Arial", hjust = .5),
        legend.title = element_text(family = "Arial", size = 9, hjust = 1),
        
        plot.caption =  element_text(family = "Arial", hjust = 1, size = 8),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(family = "Arial", hjust = .5, size = 8, 
                                  face = "bold"),
        plot.subtitle = element_text(family = "Arial", hjust = .5, size = 16)
  )
}
calendar_plot <-
  calendar_data %>%
  ggplot(aes(weekday, -week, fill = counts)) +
  geom_tile(colour = "white", size = .4)  + 
  geom_text(aes(label = day), size = 2.5) +
  guides(fill = guide_colorbar(barwidth = 15, 
                               barheight = .4,
                               title.position = "top")) +
  facet_wrap(~ month, nrow = 3, ncol = 4, scales = "free") +
  labs(fill = "words", title = "Word Counts by Day") +
  theme_calendar()
calendar_plot
by_month_plot <-
  short_chapters_df %>%
  mutate("month" = month(date, label = T, abbr = F)) %>%
  group_by(month) %>%
  summarize("month_totals" = sum(counts)) %>%
  plot_ly(labels = ~month, values = ~month_totals, type = "pie",
          textposition = "inside", textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'), sort = FALSE,
          showlegend = FALSE) %>%
  layout(title = "Months by Word Count")

species_list <- c("Human", "Drake", "Gnoll", "Antinium", "Goblin", "Lizardfolk", 
                  "Selphid", "Dullahan", "Gazer", "Centaur", "Vampire", "Dragon",
                  "Dwarf", "Drowned Man", "Stitchfolk", "Minotaur", "Elf", 
                  "Demon", "Garuda", "Fae")
spec_totals <-
    species_list %>%
    get_counts()
total_spec_frame <- 
  data.frame(species_list, spec_totals) %>%
  arrange(-spec_totals)
spec_totals_plot <-
  total_spec_frame %>%
  arrange(spec_totals) %>%
  plot_ly(y = ~species_list, x = ~spec_totals,
          type = "bar") %>% 
  layout(title = "Species by Total Mentions",
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~title),
         xaxis = list(title = "Mentions"))
#### Building class list from scratch
# class_list <-
#   read_html("https://thewanderinginn.fandom.com/wiki/List_of_Classes") %>%
#   html_nodes("table") %>%
#   .[[1]] %>%
#   html_table() %>%
#   pull("Name")
# class_list <- class_list[-175]
# class_list <-
#   data.frame(class_list) %>%
#   separate_rows(class_list, sep=" \\/ ") %>%
#   separate_rows(class_list, sep="\\/") %>%
#   pull(class_list)
# class_totals <-
#   class_list %>%
#   str_replace_all("\\[", "\\\\[") %>%
#   str_replace_all("\\]", "\\\\]") %>%
#   get_counts() %>%
#   data.frame() %>%
#   pull(1)
# total_class_frame <- data.frame(class_list, class_totals)
# write.csv(total_class_frame, "class_counts.csv")

total_class_frame <- 
  read.csv("class_counts.csv") %>%
  arrange(-class_totals) %>%
  select(-X)

class_totals_plot <-
  total_class_frame %>%
  filter(class_totals >= 10) %>%
  arrange(class_totals) %>%
  plot_ly(y = ~class_list, x = ~class_totals,
          type = "bar") %>% 
  layout(title = "Classes by Total Mentions",
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~title),
         xaxis = list(title = "Mentions"))