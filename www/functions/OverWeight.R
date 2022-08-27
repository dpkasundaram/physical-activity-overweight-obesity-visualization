##OverWeight tab all plots

OverWeight <- function(data, yearinput){
  #OWList <- list()
  df_Obesity <- data$df_Obesity
  
  p1 <- df_Obesity %>% 
    filter(Question == "Percent of adults aged 18 years and older who have an overweight classification" & LocationDesc == "National") %>% 
    filter(!is.na(Education)) %>% 
    ggplot(aes(x = as.factor(YearStart), y = Data_Value,color = Stratification1,group =Stratification1)) +
    geom_line(size = 1) + geom_point(size =3)+
    theme_tufte() +
    labs(title = "Over Weight By Education", y = "Popluation Percentage", x = "Year", color = "Education") +
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank())
  
  p2 <- df_Obesity %>% 
    filter(Question == "Percent of adults aged 18 years and older who have an overweight classification" & LocationDesc == "National") %>% 
    filter(!is.na(`Race/Ethnicity`) & !(`Race/Ethnicity` == "Data not reported")& YearStart == yearinput) %>% 
    ggplot(aes(x = `Race/Ethnicity`, y = Data_Value,color = `Race/Ethnicity`, fill = `Race/Ethnicity`)) + 
    theme_minimal() +
    geom_point(size = 5) + geom_segment(aes(x= `Race/Ethnicity`,xend =`Race/Ethnicity`, y=0, yend = Data_Value),size = 2) +
    theme_tufte() +
    labs(title = "Over Weight By Race", y = "Popluation Percentage") +
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    geom_text(aes(label = Data_Value, vjust = -1,color=`Race/Ethnicity`))
  
  temp_pie <- df_Obesity %>% filter(Question == "Percent of adults aged 18 years and older who have an overweight classification" & LocationDesc == "National") %>% filter(!is.na(`Age(years)`) & !(`Age(years)` == "Data not reported")& YearStart == yearinput) 
  
  temp_pie$ymax = cumsum(temp_pie$Data_Value)
  
  # Compute the bottom of each rectangle
  temp_pie$ymin = c(0, head(temp_pie$ymax, n=-1))
  
  # Compute label position
  temp_pie$labelPosition <- (temp_pie$ymax + temp_pie$ymin) / 2
  
  # Compute a good label
  temp_pie$label <- paste0(temp_pie$Data_Value,"%")
  
  p3 <- temp_pie %>% ggplot(aes(x = "", y = (Data_Value), fill = factor(`Age(years)`))) + geom_bar( stat = "identity") +
    theme_tufte() +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5),
          axis.text.x = element_blank()) + 
    labs(fill="Age Group", 
         x=NULL, 
         y=NULL, 
         title="Overweight by Age") + 
    coord_polar("y", start = 90) +
    scale_fill_brewer()+
    geom_text(aes(y=labelPosition,label=label), size=3, color = "black")
  
  p4 <- df_Obesity %>% filter(Question == "Percent of adults aged 18 years and older who have an overweight classification" & LocationDesc == "National") %>% 
    filter(!is.na(`Income`) & !(Income == "Data not reported") 
           & YearStart == yearinput) %>% 
    ggplot(aes(x = `Income`, y = Data_Value, fill = `Income`)) + 
    geom_bar(stat = "identity")+ 
    theme_tufte() +
    labs(title = "Over Weight By Income", y = "Popluation Percentage") +
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank(),
          axis.text.x = element_blank()) + 
    scale_fill_brewer()+
    geom_text(aes(label = Data_Value, vjust = -0.25),color ="black")
  
  return(list(p1,p2,p3,p4))
  
}