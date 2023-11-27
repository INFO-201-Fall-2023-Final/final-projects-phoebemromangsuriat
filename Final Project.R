library(dplyr)
library(stringr)

#renaming our datasets 
marketing_salesdf <- read.csv("~/Desktop/marketing_sales_data.csv")
instagram_influencerdf <- read.csv("~/Desktop/social media influencers-INSTAGRAM - -DEC 2022.csv", header=FALSE)

#renaming columns
names(instagram_influencerdf)[c(1, 2, 3, 4, 5, 6, 7, 8, 9)] <- c("Rank", "Name", "Instagram Name", "Category 1", "Category 2", "Followers", "Country", "Engagement", "Avg Engagement")
#deleting top row 
instagram_influencerdf <- instagram_influencerdf[-1, ]

#changing "followers" from string to numeric value 
instagram_influencerdf$Followers <- ifelse(
  str_detect(instagram_influencerdf$Followers, "K"),
  as.numeric(str_replace_all(instagram_influencerdf$Followers, "K", "")) * 1000,
  ifelse(
    str_detect(instagram_influencerdf$Followers, "M"), 
    as.numeric(str_replace_all(instagram_influencerdf$Followers, "M", "")) * 1000000,
  )
)

#creating a new column to categorize influencer impact 
instagram_influencerdf$Impact <- ifelse(
  instagram_influencerdf$Followers > 1000000,
  "Mega",
  ifelse(
    instagram_influencerdf$Followers >= 100000, 
    "Macro",
    ifelse(
      instagram_influencerdf$Followers >= 1000, 
      "Micro",
      ifelse(
        instagram_influencerdf$Followers < 1000, 
        "Nano",
      )
    )
  )
)

#merging our two datasets 
df <- merge(x=marketing_salesdf, y=instagram_influencerdf, by.x="Influencer", by.y="Impact", all=TRUE) 

#downloading the joint dataset into my computer 
write.csv(df, "Joint df.csv", row.names=FALSE) 

