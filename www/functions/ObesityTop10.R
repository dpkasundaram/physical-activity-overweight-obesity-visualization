ObesityTop10 <- function(data, yearinput) {

df_Obesity <- data$df_Obesity
obesity <- df_Obesity %>% 
  filter(Question == "Percent of adults aged 18 years and older who have obesity") 

names(obesity)[names(obesity) == "Data_Value"] <- "Obesity_Percentage"

obesity <- obesity %>% replace_na(list(Obesity_Percentage = 0))

USAobesityTop10 <- obesity %>% filter(YearStart == yearinput) %>% group_by(YearStart, LocationDesc) %>% summarize(totalobeesity_percentage = sum(Obesity_Percentage)) %>% arrange(desc(totalobeesity_percentage)) %>% top_n(10,totalobeesity_percentage)

ObeseStatesTop10 <- ggplot(USAobesityTop10, aes(x=reorder(LocationDesc,-totalobeesity_percentage), y=totalobeesity_percentage/20)) +
  geom_bar(stat="identity", fill="#B6B6F5") + coord_flip() 

p <- ObeseStatesTop10 + theme_minimal() +xlab("State") + ylab("Obesity Rate (%)") +
  ggtitle("The 10 States with the Highest Rates of Adult Obesity") +
  theme_tufte()+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +
  geom_text(aes(label = round((totalobeesity_percentage/20), digits = 1), hjust =2),color ="black")

return(p)
}

physicalTop10 <- function(data, yearinput) {
  
  df_Physical <-data$df_Physical 
  
  physical <- df_Physical %>% 
    filter(Question == "Percent of adults who engage in no leisure-time physical activity") 
  
  names(physical)[names(physical) == "Data_Value"] <- "PhysicalActivity_Percentage"
  
  physical <- physical %>% replace_na(list(PhysicalActivity_Percentage = 0))
  
  USAInactivityTop10 <-physical %>% filter(YearStart == yearinput)  %>% group_by(YearStart, LocationDesc) %>% summarize(total_percentage = sum(PhysicalActivity_Percentage)) %>% arrange(desc(total_percentage)) %>% top_n(10,total_percentage)
  
  InactiveStatesTop10 <- ggplot(USAInactivityTop10, aes(x=reorder(LocationDesc,-total_percentage), y=total_percentage/20)) +
    geom_bar(stat="identity", fill="#00CC99") + coord_flip()  
  
  p1 <- InactiveStatesTop10 + theme_minimal() +xlab("State") + ylab("% of Inactive Adults") +
    ggtitle("10 States with the Highest Physical Inactivity Rates") + 
    theme_tufte() +
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank()) +
    geom_text(aes(label = round((total_percentage/20), digits = 1), hjust =2),color ="black")
  
  return(p1)
}

jointdata <- function(data) {

df_whole <- data$df_whole

df_joindata = df_whole %>% filter(Question == "Percent of adults who engage in no leisure-time physical activity" | Question == "Percent of adults aged 18 years and older who have obesity")

p2 <- ggplot() + geom_col(data = df_joindata, aes(x = as.factor(YearStart), y = Data_Value, fill = Question), position = "dodge")   +  coord_flip() +
  labs(title = "Adults with obesity vs not engaging in physical activity",
       y= "Population %",
       x= "Year")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  theme(legend.position = "top")+
  scale_fill_brewer(palette = "Accent") 

return(p2)
}

geoani <- function(data){
  
  df_geo <- data$df_ExPattern
  df_geo <- df_geo %>% replace_na(list(Data_Value = 0))
  
  plotly_df = df_geo %>%
    select(YearStart, LocationDesc, LocationAbbr, Data_Value) %>%
    mutate(hover = paste0(LocationDesc, ":\n", Data_Value, "%"))
  
  graph_properties <- list(
    scope = 'usa',
    showland = TRUE,
    landcolor = toRGB("white"),
    color = toRGB("white")
  )
  
  font = list(
    family = "DM Sans",
    size = 15,
    color = "black"
  )
  
  label = list(
    bgcolor = "#EEEEEE",
    bordercolor = "transparent",
    font = font
  )
  
  
  physicalactivity_graph = plot_geo(plotly_df, 
                                    locationmode = "USA-states", 
                                    frame = ~YearStart) %>%
    add_trace(locations = ~LocationAbbr,
              z = ~Data_Value,
              zmin = 0,
              zmax = max(plotly_df$Data_Value),
              color = ~Data_Value,
              colorscale = "Electric",
              text = ~hover,
              hoverinfo = 'text') %>%
    layout(geo = graph_properties,
           title = "Percentage of Physical Activity in the US\n2011 - 2020",
           font = list(family = "DM Sans"),
           color = "Physical Activity %") %>%
    config(displayModeBar = FALSE) %>%
    style(hoverlabel = label) %>%
    colorbar(tickprefix = '%')
  
 return(physicalactivity_graph)
  
}

obesityani <- function(data){
  
df_Obesity <- data$df_Obesity

plotlyobese_df = df_Obesity %>%
  select(YearStart, LocationDesc, LocationAbbr, Data_Value) %>%
  mutate(hover = paste0(LocationDesc, ":\n", Data_Value, "%"))

graph_properties <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("white"),
  color = toRGB("white")
)

font = list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = font
)


obesity_graph = plot_geo(plotlyobese_df, 
                         locationmode = "USA-states", 
                         frame = ~YearStart) %>%
  add_trace(locations = ~LocationAbbr,
            z = ~Data_Value,
            zmin = 0,
            zmax = max(plotlyobese_df$Data_Value),
            color = ~Data_Value,
            colorscale = "Viridis",
            text = ~hover,
            hoverinfo = 'text') %>%
  layout(geo = graph_properties,
         title = "Percentage of Obesity in the US\n2011 - 2020",
         font = list(family = "DM Sans"),
         color = "Obesity %") %>%
  config(displayModeBar = FALSE) %>%
  style(hoverlabel = label) %>%
  colorbar(tickprefix = '%')

return(obesity_graph)
}

trendani <- function(data){
  df_Physical <- data$df_ExPattern
  
  outfile <- tempfile(fileext='.gif')
  
  p <- df_Physical %>% 
    filter(!(Exercise_Pattern == "Inactive") & LocationDesc == "National")%>% 
    group_by(YearStart,Exercise_Pattern) %>% 
    summarise(total_percent = sum(Data_Value)) %>%
    ggplot(aes(as.factor(YearStart), round((total_percent/4),2), group = Exercise_Pattern, color = Exercise_Pattern)
    ) +
    geom_line() +
    scale_color_viridis_d() +
    labs(title = "National Physical Activity Trend",x = "Year", y = "Avg Activity Value", color = "Exercise Pattern") +
    theme_tufte() +
    theme(legend.position = "top") + 
    geom_point(aes(size = round((total_percent/4),2)), show.legend = F) +transition_reveal(YearStart) +
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank())
  
  anim_save("outfile.gif", animate(p))
  return (list(src = "outfile.gif",
       contentType = 'image/gif'
  ))
  
}
