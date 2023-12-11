library(shiny)
library(ggplot2)
library(stringr)

#load dataset into R
marketing_df <- read.csv('marketing_sales_data.csv')
instagram_df <- read.csv("social media influencers-INSTAGRAM - -DEC 2022.csv")
consumer_df <- read.csv("influencer marketing dataset.csv")

#Data cleaning

#changing column followers to numerical value
instagram_df$followers <- ifelse(
  str_detect(instagram_df$followers, "K"),
  as.numeric(str_replace_all(instagram_df$followers, "K", "")) * 1000,
  ifelse(
    str_detect(instagram_df$followers, "M"), 
    as.numeric(str_replace_all(instagram_df$followers, "M", "")) * 1000000,
  )
)

#Categorizing influencers' impact
instagram_df$Impact <- ifelse(
  instagram_df$followers > 10000000,
  "Mega",
  ifelse(
    instagram_df$followers >= 1000000, 
    "Macro",
    ifelse(
      instagram_df$followers >= 10000, 
      "Micro",
      ifelse(
        instagram_df$followers >= 1000,
        "Nano"
      )
    )
  )
)

# Define UI 
intro_page <- fluidPage(
  titlePanel("Influencer Effect on Sales"),
  h5("Through this project, we will analyze data that correlates to Influencer Marketing 
  through various social media platforms and explore its true impact on sales numbers. 
  The results of our research would affect what we can categorize into 3 parties: Business, 
  Consumer and Societal. Each page in our website showcases the individual parties")
)


Business <- fluidPage(
  titlePanel("Business"),
  h5("Businesses hire influencers with huge followings to become an ambassador for 
     their company and promote products on their social media platforms in order to 
     reach a larger audience that to their advantage, looks up to them."),
    h3("Choose a Business Category and look at our findings!"),
  h5("The results that will determine the efficiency of Influencer Marketing can be used to 
     impact decision making for businesses. Marketing strategies can either shift from/to generic 
     strategies from/to Influencer Marketing, taking into consideration the cost, profit, and 
     efficiency between both marketing strategies. The decisions they make will impact business growth."),
  mainPanel(
    tabsetPanel(
      tabPanel("Sales", plotOutput(outputId = "salesplot")),
      tabPanel("Industry", plotOutput(outputId = "industrypie"))
    )
  )
)


Consumer <- fluidPage(
    titlePanel("Consumer Interest Pie Chart"),
    h5("This Pie Chart shows the ratio of certain social media categories that consumers are interested in 
      compared to one another. This allows us to know which category is most popular within consumers to 
       determine influencer marketing reach."),
    mainPanel(
      plotOutput("ConsumerChart")
      )
)

Society <- fluidPage(
  titlePanel("Impacts on different parties"),
  h5("Based on our findings in the consumer section of our project, we can see that the most popular 
    social media category is Fashion and Beauty (see consumer page for pie chart). As for our findings
    in the business section of our project, we have found that businesses tend to ..."),
  mainPanel(
    tabsetPanel(
      tabPanel("Business", 
               h3("Business are now looking into new marketing stragedy and this project is to 
                  encourage business owners to look into the potentials from having influencers as
                  their main marketing channel for the following reasons:"),
               h4("1. Sales"),
               h5("We can see from the business page, despite the difference in impact of the influencers,
                  the plot clearly shows that sales very much reflect a high return in investment. There
                  is no reason for business owners to not consider investing in influencers even if they
                  have impact of category nano since the return in sales is significant enough."),
               h4("2. Industry type"),
               h5("As we can see in the business page, influencers is spread over different industries,
                  meaning that businesses in different industries can benefit from the trend instead of
                  a specific industry.")
               ),
      tabPanel("Consumer",
               h5("Based on our findings in the consumer section of our project, we can see that the most popular 
    social media category is Fashion and Beauty (see consumer page for pie chart). The goal of this project is
    to find whether or not Influencer Marketing is efficient. With our findings, we can conclude that if Business
    were to do Influencer Marketing, doing it on Fashion and Beauty would be the most effective due to the number of
    consumers that keep up with Fashion and Beauty trends through social media.")),
      tabPanel("Society",
               h5("In our proposal, we pose the question of 'To what extent?'. This can be applied to this matter of power and manipulation. 
  To what extent will influencers have the power to affect decision-making done by the public? Will this be a problem in the real world? 
  Will it go further than sales? Will it go as far as influencing the public’s political choices? The concern of ‘Power’ these influencers 
  have been considered and to our findings, influencer marketing is not too big of a scale to handle. We see a healthy number of sales through
  Influencer Marketing and use of social media in both the business and the consumer aspect of this project."))
    )
  )
)

ui <- navbarPage(
  "INFO 201 FINAL PROJECT",
  tabPanel(
    "Introduction",
    intro_page
  ),
  tabPanel(
    "Business",
    Business
  ),
  tabPanel(
    "Consumer",
    Consumer
  ),
  tabPanel(
    "Conclusion", 
    Society 
  )
)


# Define server 
server <- function(input, output) {
  output$ConsumerChart <- renderPlot({
    category_counts <- table(consumer_df$categories)
    # Create a pie chart
    colors <- rainbow(length(category_counts))  
    pie(category_counts, labels = names(category_counts), 
        main = "Category Popularity")
  })
  
  output$salesplot <- renderPlot({
    #mean of sales for different impact
    sales_mega <- sales_df[sales_df$Influencer == "Mega", ]
    mega_mean <- round(mean(sales_mega$Sales))
    sales_macro <- sales_df[sales_df$Influencer == "Macro", ]
    macro_mean <- round(mean(sales_macro$Sales))
    sales_micro <- sales_df[sales_df$Influencer == "Micro", ]
    micro_mean <- round(mean(sales_micro$Sales))
    sales_nano <- sales_df[sales_df$Influencer == "Nano", ]
    nano_mean <- round(mean(sales_nano$Sales))
    
    #create a table for means and respective impact
    Impact <- c("Mega", "Macro", "Micro", "Nano")
    Mean_sales <- c(mega_mean, macro_mean, micro_mean, nano_mean)
    sales_impact <- data.frame(Impact, Mean_sales)
    
    #barplot for sales regarding different impact of the influencers
    sales_bar <- ggplot(sales_impact, aes(x = Impact, y = Mean_sales, fill = Impact)) +
      geom_bar(stat = "identity", colour = "black") +
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#FF0000")) +
      geom_text(aes(label= Mean_sales), hjust = 1.5, colour = "white" , size = 3.5) +
      theme_minimal() +
      coord_flip()
    plot(sales_bar)
  })
  
  output$industrypie <- renderPlot({
    #piechart for industries type by category_1 (smaller value is grouped as others)
    category_count <- table(instagram_df$Category_1)
    category_count <- data.frame(category_count)
    category_count <- category_count[-1, ]
    colnames(category_count)[1] = "Category"
    Frequency <- c(16, 26, 197, 12, 10, 18, 151, 39, 233, 43, 135, 70)
    instagram_cat_df <- pie(
      Frequency,
      labels = c("Art/Artist", "Beauty", "Cinema & Actors/actresses", "Family", "Fitness & Gym", 
                 "Humor & Fun & Happiness", "Lifestyle", "Modeling", "Music", "Shows", "Sports with a ball", 
                 "Others"),
      main = "Influencers distribution in different types of industries"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
