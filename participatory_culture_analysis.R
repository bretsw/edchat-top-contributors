## --------------------------------------------------------------
## set workspace and load libraries
## --------------------------------------------------------------

#ls()  ## lists variables in the workspace
rm(list=ls())  ## clears all variables in the workspace

library(tidyverse)  # for data manipulation; includes library(dplyr); library(ggplot2)
library(stringr)  # for ease of working with string and character varbiables
library(lubridate)  # for working with dates
library(ggtern)


## --------------------------------------------------------------
## load, clean, view data
## --------------------------------------------------------------

## assumes filenames are of the form "edchat_raw_startYYMMDD_endYYMMDD.csv" and stored in named directory
#m <- stringr::str_extract_all(dir("~/Dropbox/encrypted_data/edchat_data/"), "edchat_raw\\S+", simplify=TRUE)
#m <- m[m[,1] != "", ]
#edchat_raw <- lapply(m, read.csv, header=TRUE, colClasses="character")  # edchat_list[[i]] = sample i  |  edchat_list[[i]][1,] = row 1 of sample i

## create dataframe
#n_samples <- length(edchat_raw)
#edchat_df <- data.frame()
#for (i in 1:n_samples) {
#        print(dim(edchat_raw[[i]]))
#        edchat_df <- rbind(edchat_df, edchat_raw[[i]])
#}
#edchat_df <- dplyr::mutate(edchat_df, id = row_number())
#dim(edchat_df)  # 1,309,933 x 19
#colnames(edchat_df)
#edchat_df <- edchat_df[,c(19, 1:18)]

# clean data
#edchat <- edchat_df[(!duplicated(edchat_df$id_str) &
#                             edchat_df$id_str!="" &
#                             !is.na(edchat_df$id_str)
#)
#, ]


edchat <- read.csv("~/Dropbox/encrypted_data/edchat_data/edchat_full_df.csv", header=TRUE, colClasses="character")
#dim(edchat)  # dim = 1,228,506 x 19
#length(edchat$id_str)  # n=1,228,506 (number of unique tweets)
#length(unique(edchat$id_str))  # n=1,228,506 (check to make sure all tweets are unique)
#length(unique(edchat$from_user_id_str))  # n=196,263 (number of unique tweeters)



## --------------------------------------------------------------
## Participatory Actions
## --------------------------------------------------------------


## 1 - Replies
replies <- edchat[which(
        edchat$in_reply_to_screen_name != "" & 
                edchat$in_reply_to_screen_name != edchat$from_user & 
                edchat$in_reply_to_status_id_str != ""
), ]
#dim(replies)  # n = 9,420


## 2 - Retweets  (conceptualized as RTs, MTs, and via tweets)
all_rt <- edchat[which(
        edchat$in_reply_to_screen_name == "" & 
                (grepl("^[Rr][Tt]", edchat$text) |
                         grepl("^[Rr][Ee][Tt][Ww][Ee][Ee][Tt]", edchat$text) |
                         grepl("^[Vv][Ii][Aa]", edchat$text) | 
                         grepl("^MT", edchat$text)
                 )
), ]
#dim(all_rt)  # n = 716,434

rt_names <- all_rt$text %>%
        stringr::str_split_fixed(pattern=": ", n=2) %>%
        sub("^[Rr][Ee][Tt][Ww][Ee][Ee][Tt][Ee][Dd] ", "", .) %>%
        sub("^[Rr][Ee][Tt][Ww][Ee][Ee][Tt] ", "", .) %>%
        sub("^[Rr][Tt] ", "", .) %>%
        sub("^[Vv][Ii][Aa] ", "", .) %>%
        sub("^MT ", "", .) %>%
        sub("^@", "", .) %>%
        unlist %>%
        .[,1] %>%
        stringr::str_split_fixed(pattern=" ", n=2) %>%
        .[,1]
#length(rt_names)  # n = 716,434

self_rt <- all_rt[all_rt[,"from_user"] == rt_names, ]
#dim(self_rt)  # n = 12,601

retweets <- all_rt[all_rt[,"from_user"] != rt_names, ] 
#dim(retweets)  # n = 703,833  


## 3 - Posts
edchat_not_posts <- rbind(replies, retweets)
#dim(edchat_not_posts)  # n = 713,253

posts <- edchat %>% 
        filter(!(id_str %in% edchat_not_posts$id_str))
#dim(posts)  # n = 515,253



## create summary table of participatory actions
tweet_type <- c("posts", "retweets", "replies")
tweet_number <- c(length(posts$id_str),
                  length(retweets$id_str), 
                  length(replies$id_str)
)
tweeter_number <- c(length(unique(posts$from_user_id_str)),
                    length(unique(retweets$from_user_id_str)), 
                    length(unique(replies$from_user_id_str))
)
n_tweets <- length(unique(edchat$id_str))  # n = 1,228,506
n_tweeters <- length(unique(edchat$from_user_id_str))  # n = 196,263

summary_df <- data.frame(tweet_type, tweet_number, tweeter_number) %>%
        mutate(tweet_p = round(100*tweet_number/n_tweets,digits=2),
               tweeter_p = round(100*tweeter_number/n_tweeters,digits=2),
               ratio = round(tweet_number/tweeter_number, digits=2)) %>%
        '['(,c(1,2,4,3,5,6)) %>%
        rbind(c("", n_tweets, 100, n_tweeters, 100, 
                round(n_tweets/n_tweeters,digits=2))) %>%
        print



## --------------------------------------------------------------
## add new column to edchat dataframe: 'type'
## --------------------------------------------------------------

edchat <- edchat %>% 
        mutate(type = case_when(id_str %in% posts$id_str ~ "post", 
                                id_str %in% retweets$id_str ~ "retweet", 
                                id_str %in% replies$id_str ~ "reply")
        )



## --------------------------------------------------------------
## participatory roles
## --------------------------------------------------------------

## reference https://www.r-bloggers.com/make-a-bar-plot-with-ggplot/
## reference http://ggplot2.tidyverse.org/reference/geom_bar.html 
## reference https://www.r-bloggers.com/pie-charts-in-ggplot2/


tweet_tweeter <- rep(c("tweets", "tweeters"), 3)  # 3x8
values <- as.numeric(c(summary_df$tweet_p[1], summary_df$tweeter_p[1],
                       summary_df$tweet_p[2], summary_df$tweeter_p[2],
                       summary_df$tweet_p[3], summary_df$tweeter_p[3]
))
summary_df$tweet_type <- as.character(summary_df$tweet_type)

types <- factor(c(rep(summary_df$tweet_type[1], 2),
                  rep(summary_df$tweet_type[2], 2),
                  rep(summary_df$tweet_type[3], 2)),
                levels=summary_df$tweet_type[1:3], 
                ordered=TRUE)

bar_data <- data.frame(tweet_tweeter, values)
bar_data$tweet_tweeter <- factor(bar_data$tweet_tweeter,levels = c("tweets", "tweeters"))

bar_plot <- ggplot(bar_data, aes(x=tweet_tweeter, y=values, fill=types)) +
        geom_col(position = "dodge") +  # position = "dodge" or "fill" or "stack"
        xlab("") + ylab("Proportion") + labs(fill="Tweet Type") +
        #ggtitle("Proportion of Tweets and Tweeters by Type") +
        #theme_bw() + # remove grey background (because Tufte said so)
        theme(panel.background = element_rect(fill = "white", colour = "black"))  # https://ggplot2.tidyverse.org/reference/theme.html
#bar_plot


## statistics on social interactions for all useres (users, posts, retweets, replies)
tweets_table <- edchat$from_user_id_str %>%
        table %>%
        as.data.frame
colnames(tweets_table) <- c("user", "all_tweets")  ## changes column names


post_table <- posts$from_user_id_str %>% table %>% as.data.frame
        colnames(post_table) <- c("user", "n_posts")
retweet_table <- retweets$from_user_id_str %>% table %>% as.data.frame
        colnames(retweet_table) <- c("user", "n_retweets")
reply_table <- replies$from_user_id_str %>% table %>% as.data.frame
        colnames(reply_table) <- c("user", "n_replies")

participants <- tweets_table %>% merge(post_table, all.x=TRUE) %>% 
        merge(retweet_table, all.x=TRUE) %>% 
        merge(reply_table, all.x=TRUE)
participants[is.na(participants)] <- 0
#dim(participants)  # n = 196,263


## create ternary plot
## https://CRAN.R-project.org/package=ggtern
## http://www.ggtern.com/docs/ 
## ggplot2 new version mess: https://github.com/tidyverse/ggplot2/releases
ternary_plot <- ggtern(data = participants, 
                       mapping = aes(x = n_posts,
                                     y = n_retweets, 
                                     z = n_replies)) + 
        geom_point(alpha=0.1)
        #theme_bw() + # remove grey background (because Tufte said so)
        #theme(panel.grid.major = element_blank()) + # remove x and y major grid lines (because Tufte said so)
        #theme(panel.background = element_rect(fill = "white", colour = "black"))
        #xlab("Posts") + ylab("Retweets") + zlab("Replies") +
        #labs(title="Edchat participation", subtitle="Tweeter contributions by proportion of posts, retweets, and replies")
#ternary_plot


## create scatterplot: posts vs. retweets
scatter_plot <- ggplot(data = participants,
                       aes(x = n_posts, 
                           y = n_retweets)) + 
        geom_point(alpha=.25) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .3)
        ) +
        xlab("Number of posts") + ylab("Number of retweets")       
#scatter_plot  # display the plot


## create reples histogram
replies_histogram <- ggplot(data = participants, 
                            aes(x = n_replies)) + 
        geom_histogram(binwidth=2, colour = "black") + 
        xlim(7, 700) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              panel.grid.major = element_line(colour = "grey95"),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .3)
              ) +
        xlab("Number of replies") + ylab("Number of tweeters")
#replies_histogram  # display the plot



## --------------------------------------------------------------
## descriptive statistics: tweets per tweeter
## --------------------------------------------------------------

type_mean <- c(mean(participants$n_posts), mean(participants$n_retweets), mean(participants$n_replies), mean(participants$all_tweets))
type_sd <- c(sd(participants$n_posts), sd(participants$n_retweets), sd(participants$n_replies), sd(participants$all_tweets))
type_median <- c(median(participants$n_posts), median(participants$n_retweets), median(participants$n_replies), median(participants$all_tweets))
type_min <- c(min(participants$n_posts), min(participants$n_retweets), min(participants$n_replies), min(participants$all_tweets))
type_max <- c(max(participants$n_posts), max(participants$n_retweets), max(participants$n_replies), max(participants$all_tweets))

type_max_z <- c(((max(participants$n_posts) - mean(participants$n_posts)) / sd(participants$n_posts)) %>% round(digits=2),
           ((max(participants$n_retweets) - mean(participants$n_retweets)) / sd(participants$n_retweets)) %>% round(digits=2),
           ((max(participants$n_replies) - mean(participants$n_replies)) / sd(participants$n_replies)) %>% round(digits=2),
           ((max(participants$all_tweets) - mean(participants$all_tweets)) / sd(participants$all_tweets)) %>% round(digits=2)
)

tweet_n <- summary_df$tweet_number %>% as.numeric
tweet_p <- summary_df$tweet_p %>% as.numeric
tweeter_n <- summary_df$tweeter_number %>% as.numeric
tweeter_p <- summary_df$tweeter_p %>% as.numeric
infrequent_n <- c(participants$n_posts %>% table %>% '['(1:8) %>% sum,
                participants$n_retweets %>% table %>% '['(1:8) %>% sum,
                participants$n_replies %>% table %>% '['(1:8) %>% sum,
                participants$all_tweets %>% table %>% '['(1:8) %>% sum)
infrequent_p <- round(100 * infrequent_n / tweeter_n[4], digits=2)
active_n <- tweeter_n[4] - infrequent_n
active_p <- round(100 * active_n / tweeter_n[4], digits=2)

## create summary table of statistics for tweets per tweeter
tweets_per_tweeter <- cbind(c("posts", "retweets", "replies", "ALL"),
                            type_mean,
                            type_sd,
                            type_median,
                            type_min,
                            type_max,
                            type_max_z,
                            active_n,
                            active_p
                            ) %>% as.data.frame
colnames(tweets_per_tweeter) <- c("Type", "Mean", "SD", "Median", "Min", "Max", "Max z", "Active n", "Active p")
#tweets_per_tweeter



## --------------------------------------------------------------
## tweets by date
## --------------------------------------------------------------

dates_all <- edchat$time %>% dmy_hms %>% as.Date(tz="America/New_York")
dates_span <- max(dates_all) - min(dates_all)  # Time difference of 247 days --> so a span of 248 days

dates_table <- dates_all %>% table %>% as.data.frame #%>% arrange(desc(Freq))
        colnames(dates_table) <- c("Date", "All")
dates_posts <- posts$time %>% dmy_hms %>% as.Date(tz="America/New_York") %>% table %>% as.data.frame
        colnames(dates_posts) <- c("Date", "Posts")
dates_retweets <- retweets$time %>% dmy_hms %>% as.Date(tz="America/New_York") %>% table %>% as.data.frame
        colnames(dates_retweets) <- c("Date", "Retweets")
dates_replies <- replies$time %>% dmy_hms %>% as.Date(tz="America/New_York") %>% table %>% as.data.frame
        colnames(dates_replies) <- c("Date", "Replies")

dates_table_full <- dates_table %>% 
        merge(dates_posts) %>%
        merge(dates_retweets) %>% 
        merge(dates_replies)
#dates_table_full

dates_dates <- dates_table_full$Date %>% rep(3)
dates_types <- rep("post", 248) %>% c(rep("retweet", 248)) %>% c(rep("reply", 248)) %>% factor(levels=c("post", "retweet", "reply"), ordered=TRUE)
dates_values <- c(dates_table_full$Posts, dates_table_full$Retweets, dates_table_full$Replies)

dates_df <- data.frame(dates_dates, dates_types, dates_values)

dates_plot <- ggplot(data = dates_df, 
                     aes(x=dates_dates,
                         y=dates_values,
                         fill=dates_types)) +
        geom_col(alpha=1, position = "stack") +
        scale_fill_manual(values = c("grey80", "grey60", "black")) +
        theme(panel.background = element_rect(fill = "white", colour = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x=element_blank()
        ) +
        xlab("Dates") + ylab("Count") + labs(fill="Tweet Type")
#dates_plot



## --------------------------------------------------------------
## select and examine cases
## --------------------------------------------------------------

active_posters <- participants %>% filter(n_posts >= 8) %>% arrange(desc(n_posts))
#        active_posters[1:10,]  # view first 10 rows
active_retweeters<- participants %>% filter(n_retweets >= 8) %>% arrange(desc(n_retweets))
#        active_retweeters[1:10,]  # view first 10 rows
active_repliers <- participants %>% filter(n_replies >= 8) %>% arrange(desc(n_replies))
#        active_repliers[1:10,]  # view first 10 rows

cases <- rbind(
        active_posters[1:3,],
        active_retweeters[1,],
        active_repliers[1:2,]
)
cases$user <- as.character(cases$user)
#cases

cases_full <- edchat %>%
        select(from_user_id_str, from_user) %>%
        group_by(from_user_id_str) %>%
        filter(from_user_id_str %in% cases$user) %>%
        ungroup %>%
        unique %>%
        as.data.frame %>%
        inner_join(cases, by=c("from_user_id_str" = "user")) %>%
        mutate(case = 1:6) %>%
        select(c(7, 1:6))
#cases_full

cases_full_tweets <- edchat %>% 
        filter(from_user_id_str %in% cases$user) %>%
        left_join(cases_full) %>%
        mutate(date = time %>% dmy_hms %>% as.Date(tz="America/New_York")) %>%
        select(-X)
#dim(cases_full_tweets)  # n = 47,293


## case timelines visualization
for (i in 1:6) {
        print(i)
        case_dates = cases_full_tweets %>% 
                filter(case == i) %>% 
                select(date)
        case_dates = case_dates[,1]
        print(max(case_dates) - min(case_dates))
        
        case_weekdays <- case_dates %>% weekdays %>% table %>% as.data.frame 
        case_weekdays_p <- case_weekdays %>% mutate(proportion = Freq / length(case_dates)) %>%
                arrange(desc(proportion)) %>% print
        
        dates_table_tmp = case_dates %>% table(dnn="days_tmp") %>% as.data.frame %>% select(days_tmp, "all" = Freq)
        
        dates_posts_tmp = cases_full_tweets %>% filter((case == i) & (type == "post")) %>% select(date)
        if (length(dates_posts_tmp[[1]]) > 0)
                {dates_posts_tmp = dates_posts_tmp %>% table(dnn="days_tmp") %>% as.data.frame %>% select(days_tmp, "posts" = Freq)} else
                        dates_posts_tmp = dates_posts_tmp %>% mutate("posts" = 0)

        dates_retweets_tmp = cases_full_tweets %>% filter((case == i) & (type == "retweet")) %>% select(date)
        if (length(dates_retweets_tmp[[1]]) > 0)
        {dates_retweets_tmp = dates_retweets_tmp %>% table(dnn="days_tmp") %>% as.data.frame %>% select(days_tmp, "retweets" = Freq)} else
                dates_retweets_tmp = dates_retweets_tmp %>% mutate("retweets" = 0)

        dates_replies_tmp = cases_full_tweets %>% filter((case == i) & (type == "reply")) %>% select(date)
        if (length(dates_replies_tmp[[1]]) > 0)
                {dates_replies_tmp = dates_replies_tmp %>% table(dnn="days_tmp") %>% as.data.frame %>% select(days_tmp, "replies" = Freq)} else
                        dates_replies_tmp = dates_replies_tmp %>% mutate("replies" = 0)

        dates_table_full_tmp = dates_table_tmp %>%  # would be cleaner to use `nrow()` in these conditionals
                {if (length(dates_posts_tmp$days_tmp) > 0) left_join(., dates_posts_tmp, by="days_tmp") else mutate(., posts=0)} %>%
                {if (length(dates_retweets_tmp$days_tmp) > 0) left_join(., dates_retweets_tmp, by="days_tmp") else mutate(., retweets=0)} %>%
                {if (length(dates_replies_tmp$days_tmp) > 0) left_join(., dates_replies_tmp, by="days_tmp") else mutate(., replies=0)}
        dates_table_full_tmp[is.na(dates_table_full_tmp)] <- 0
        
        dates_table_full_tmp %>% 
                summarize(mean_all = mean(all),
                          mean_posts = mean(posts),
                          mean_retweets = mean(retweets),
                          mean_replies = mean(replies))
                
        dates_dates_tmp = dates_table_full_tmp$days_tmp %>% rep(3) %>% as.Date
        n_tmp = length(dates_table_full_tmp$days_tmp)
        dates_types_tmp = rep("post", n_tmp) %>% c(rep("retweet", n_tmp)) %>% c(rep("reply", n_tmp)) %>% factor(levels=c("post", "retweet", "reply"), ordered=TRUE)
        dates_values_tmp = c(dates_table_full_tmp$posts, dates_table_full_tmp$retweets, dates_table_full_tmp$replies)
        
        dates_df_tmp = data.frame(dates_dates_tmp, dates_types_tmp, dates_values_tmp)
        
        case_timeline = ggplot(data = dates_df_tmp, 
                               aes(x = dates_dates_tmp,
                                   y = dates_values_tmp,
                                   fill=dates_types_tmp)) +
                geom_col(alpha=1, position = "stack", width=1) +
                xlim(as.Date("2017-10-01", "%Y-%m-%d"), as.Date("2018-06-05", "%Y-%m-%d")) +
                ylim(0, 250) +
                scale_fill_manual(values = c("grey80", "grey60", "black")) +
                theme(panel.background = element_rect(fill = "white", colour = "white"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.text.x = element_blank()
                ) +
                xlab("Dates") + ylab("Count") + labs(fill="Tweet Type")
        print(case_timeline)
}



## --------------------------------------------------------------
## take samples of each case and save as .csv files
## --------------------------------------------------------------

cases_full_tweets %>% filter(case == 1) %>% .[sample(nrow(.), 100), ] %>% write.csv("case1_tweets.csv", row.names=FALSE)
cases_full_tweets %>% filter(case == 2) %>% .[sample(nrow(.), 100), ] %>% write.csv("case2_tweets.csv", row.names=FALSE)
cases_full_tweets %>% filter(case == 3) %>% .[sample(nrow(.), 100), ] %>% write.csv("case3_tweets.csv", row.names=FALSE)
cases_full_tweets %>% filter(case == 4) %>% .[sample(nrow(.), 100), ] %>% write.csv("case4_tweets.csv", row.names=FALSE)
cases_full_tweets %>% filter(case == 5) %>% .[sample(nrow(.), 100), ] %>% write.csv("case5_tweets.csv", row.names=FALSE)
cases_full_tweets %>% filter(case == 6) %>% .[sample(nrow(.), 100), ] %>% write.csv("case6_tweets.csv", row.names=FALSE)
