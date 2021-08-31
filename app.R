pacman::p_load(tidyverse,
               anytime,
               lubridate,
               zoo,
               stringr,
               tidytext,
               shinythemes,
               shinyWidgets,
               zoo)


final_data <- readRDS(file = "final_data.rds")
chat_names <- final_data %>% pull(chat_name) %>% unique()
person_names <- final_data %>% pull(sender_name) %>% unique()
# person_names <- person_names[person_names != "Oliver Lountain"]


check_passcode <- function(passcode) {
    if (passcode == '0'){
        return(0)
    }
    else{
        return("nope")
    }
        
}

get_frequent_words <- function(final_data, chat_names){
    tmp <- final_data %>% 
        filter(chat_name %in% chat_names) %>% 
        pull(content)
    
    tmp <- paste(tmp, collapse = ' ')
    
    tmp <- str_replace_all(tmp, '\n', '')
    tmp <- str_replace_all(tmp, "[[,.!?:;]]", '')
    
    tmp <- str_to_lower(tmp)
    
    tmp <- str_split(tmp, pattern = ' ')[[1]]
    
    tmp <- tibble(tmp) %>%
        filter(!(tmp %in% c(stop_words$word, "yeah"))) %>%
        count(tmp) %>%
        arrange(-n) %>% 
        pull(tmp)
    
    tmp <- tmp[tmp != ' ']
    tmp <- tmp[tmp != '']
    
    return(tmp)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("slate"),

    # Application title
    titlePanel("Messenger Data"),

    fluidRow(
        column(4,
               wellPanel(
        
                    condition = "input.passcode == '0'",
    
                    radioButtons("display_type",
                                 "How would you like to filter the data?",
                                 c("By chat name" = "by_chat",
                                   "By group members" = "by_members")),
    
                    conditionalPanel(
                        condition = "input.display_type == 'by_chat'",
                        selectInput("chat",
                                    label = 'Chat to display',
                                    choices = chat_names,
                                    multiple = TRUE)
                    ),
    
                    conditionalPanel(
                        condition = "input.display_type == 'by_members'",
                        selectInput("people",
                                    label = 'People to include',
                                    choices = person_names,
                                    multiple = TRUE)
                    )
        
        
        
    
            
               )



        ),

        # Show a plot of the generated distribution
        column(8,
               wellPanel(h2("Messages sent over time"),
                    
                    conditionalPanel(
                        condition = "input.display_type == 'by_chat'",
                        plotOutput("chat_plot")
                    ),
        
                   conditionalPanel(
                       condition = "input.display_type == 'by_members'",
                       plotOutput("person_plot")
                   )
               )

        )
    ),
    
    fluidRow(
        column(4),
        
        column(4,
               wellPanel(h2("100 most frequently used words"),
                   
                   conditionalPanel(
                       condition = "input.display_type == 'by_chat'",
                       textOutput("freq_words_chat")
                   ),
                   
                   
                   conditionalPanel(
                       condition = "input.display_type == 'by_members'",
                       textOutput("freq_words_person")
                   )
               )
            ),
        
        column(4,
               wellPanel(h2("Who sent the most messages"),
                         
                         conditionalPanel(
                             condition = "input.display_type == 'by_chat'",
                             plotOutput("chat_plot_column")
                         ),
                         
                         
                         conditionalPanel(
                             condition = "input.display_type == 'by_members'",
                             plotOutput("person_plot_column")
                         )
               )
        ),
        
        
    )
)

# Define server logic
server <- function(input, output) {
    
    
    # output$passcode <- renderText(input$passcode)
    

    output$chat_plot <- renderPlot({
        
        # pick the chats to see
        chat <- input$chat
        
        if (length(chat) > 0) {
        
            # filter the data
            data_to_display <- final_data %>%
                mutate(year_mon = factor(as.yearmon(timestamp))) %>% 
                filter(chat_name %in% chat)
            
            # display the correct months (maybe change this code to make it less gross)
            min_month <- which((data_to_display$year_mon %>% levels() %>% as.character()) == (data_to_display$timestamp %>% min() %>% as.yearmon()))
            
            max_month <- which((data_to_display$year_mon %>% levels() %>% as.character()) == (data_to_display$timestamp %>% max() %>% as.yearmon()))
            
            my_months <- (data_to_display$year_mon %>% levels() %>% as.character())[min_month:max_month]
            
            my_months <- factor(my_months, levels = my_months)
            
            data_to_display$year_mon <- factor(data_to_display$year_mon, levels = my_months)
            
            
            # make the plot
            data_to_display %>% 
                ggplot(aes(year_mon)) + 
                geom_bar(aes(fill = chat_name)) +
                ylab("Number of Messages") +
                xlab("Month and Year") +
                scale_fill_discrete(drop=FALSE, name = "Chat Name") +
                scale_x_discrete(drop=FALSE) +
                theme(axis.text.x=element_text(angle=45, hjust = 1))
        }


    })
    
    output$freq_words_chat <- renderText({
        
        # pick the chats to see
        chat <- input$chat
        
        if (length(chat) > 0) {
            
            # filter the data
            data_to_display <- final_data %>%
                mutate(year_mon = factor(as.yearmon(timestamp))) %>% 
                filter(chat_name %in% chat)
            
            
            words <- get_frequent_words(data_to_display, chat)[1:100]
            
            paste(words, collapse = ', ')
    
        }
        
    })
    
    
    output$chat_plot_column <- renderPlot({
        # pick the chats to see
        chat <- input$chat
        
        if (length(chat) > 0) {
            
            # filter the data
            data_to_display <- final_data %>%
                mutate(year_mon = factor(as.yearmon(timestamp))) %>% 
                filter(chat_name %in% chat)
            
            # make the plot
            data_to_display %>% 
                count(sender_name) %>% arrange(-n) %>%
                ggplot(aes(x = n, y = reorder(sender_name, n))) +
                geom_col() +
                xlab("Number of Messages") +
                ylab("Sender Name")
        }
        
        
    })
    
    
    output$person_plot <- renderPlot({
        # pick the chats to see
        people <- input$people
        
        chat <- final_data %>% 
            filter(sender_name %in% people) %>% 
            pull(chat_name) %>% unique()
        
        chat <- chat[chat != "Oliver Lountain"]
        
        if (length(people) > 0) {
            
            # filter the data
            data_to_display <- final_data %>%
                mutate(year_mon = factor(as.yearmon(timestamp))) %>% 
                filter(chat_name %in% chat) %>% 
                filter(sender_name %in% people)
            
            # display the correct months (maybe change this code to make it less gross)
            min_month <- which((data_to_display$year_mon %>% levels() %>% as.character()) == (data_to_display$timestamp %>% min() %>% as.yearmon()))
            
            max_month <- which((data_to_display$year_mon %>% levels() %>% as.character()) == (data_to_display$timestamp %>% max() %>% as.yearmon()))
            
            my_months <- (data_to_display$year_mon %>% levels() %>% as.character())[min_month:max_month]
            
            my_months <- factor(my_months, levels = my_months)
            
            data_to_display$year_mon <- factor(data_to_display$year_mon, levels = my_months)
            
            
            # make the plot
            data_to_display %>% 
                ggplot(aes(year_mon)) + 
                geom_bar(aes(fill = sender_name)) +
                ylab("Number of Messages") +
                xlab("Month and Year") +
                scale_fill_discrete(drop=FALSE, name = "Sender Name") +
                scale_x_discrete(drop=FALSE) +
                theme(axis.text.x=element_text(angle=45, hjust = 1))
        }
        
        
    })
    
    output$freq_words_person <- renderText({
        
        people <- input$people
        
        chat <- final_data %>% 
            filter(sender_name %in% people) %>% 
            pull(chat_name) %>% unique()
        
        chat <- chat[chat != "Oliver Lountain"]
        
        if (length(people) > 0) {
            
            # filter the data
            data_to_display <- final_data %>%
                mutate(year_mon = factor(as.yearmon(timestamp))) %>% 
                filter(chat_name %in% chat) %>% 
                filter(sender_name %in% people)
            
            
            words <- get_frequent_words(data_to_display, chat)[1:100]
            
            paste(words, collapse = ', ')
            
        
        }
      
        
          
    })
    
    output$person_plot_column <- renderPlot({
        # pick the chats to see
        people <- input$people
        
        chat <- final_data %>% 
            filter(sender_name %in% people) %>% 
            pull(chat_name) %>% unique()
        
        chat <- chat[chat != "Oliver Lountain"]
        
        if (length(people) > 0) {
            
            # filter the data
            data_to_display <- final_data %>%
                filter(chat_name %in% chat) %>% 
                filter(sender_name %in% people)
            
            # make the plot
            data_to_display %>% 
                count(sender_name) %>% arrange(-n) %>%
                ggplot(aes(x = n, y = reorder(sender_name, n))) +
                geom_col() +
                xlab("Number of Messages") +
                ylab("Sender Name")
        }
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
