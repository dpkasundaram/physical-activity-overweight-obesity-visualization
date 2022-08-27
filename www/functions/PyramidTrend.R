gender <- function(data, yearinput){
  df_Physical <- data$df_ExPattern
  
  brks <- seq(-60, 60, 5)
  lbls = paste0(as.character(c(seq(60, 0, -5), seq(5, 60, 5))), "%")
  
  p <- df_Physical %>% 
    filter(LocationDesc == "National" & !is.na(Gender) & YearStart == yearinput) %>%
    ggplot(aes(x = Exercise_Pattern, y = ifelse(test = Gender == "Male", yes = -Data_Value, no = Data_Value), fill = Gender)) +
    geom_bar(stat = "identity", width = .6) + 
    scale_y_continuous(breaks = brks,labels = lbls) +
    scale_x_discrete(limits = c("Less Active", "Moderately Active", "Highly Active","Inactive","Extremely Active")) +
    coord_flip() +
    labs(title="Levels of Physical Activity Pyramid",
         y = "Activity Percentage",
         x = "Exercise Pattern") +
    theme_tufte() + 
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle=65)) + 
    scale_fill_brewer(palette = "Dark2")
  
  return(p)
}

yearTrend <- function(data, Race){
  df_raw <- data$df_raw
  temp <- df_raw[,-c(2,7,8,9,10,11,12,14)] %>% 
    filter((Class=="Obesity / Weight Status" | Class =="Physical Activity") 
           & !(Question == "Percent of adults who engage in no leisure-time physical activity") 
           & LocationDesc == "National")
  
  temp <- temp %>%
    mutate(Category = case_when(
      Question == "Percent of adults who engage in no leisure-time physical activity" ~ "Sedantry",
      Question == "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)" ~ "Less Active",
      Question == "Percent of adults who achieve at least 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)" ~ "Highly Active",
      Question == "Percent of adults who engage in muscle-strengthening activities on 2 or more days a week" ~ "Moderately Active",
      Question == "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity and engage in muscle-strengthening activities on 2 or more days a week" ~ "Extremely Active",
      Question == "Percent of adults aged 18 years and older who have obesity" ~ "Obese",
      Question == "Percent of adults aged 18 years and older who have an overweight classification" ~ "Over Weight"))
  
  p1 <- temp %>% filter(`Race/Ethnicity` == Race & !(Category == "Highly Active") & !(Category == "Moderately Active")) %>%
    ggplot(aes(x = as.factor(YearStart), y = Data_Value,group =Category, color =Category)) +
    geom_line(size = 1) + geom_point(size =2) +
    labs(title="Trend of Excercise and Body Type Over 10 years ",
         y = "Activity Percentage",
         x = "Year") +
    theme_tufte() + 
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle=65)) + 
    scale_fill_brewer(palette = "Dark2")
  
  return(p1)
}
