# rm (list = ls())
library(scales)

hourly_summary$MSF_8_W.Temp_Out <- hourly_summary$MSF_8_W.Temp_Out %>% as.numeric()
hourly_summary$MSF_8_W.Hi_Solar_Rad <- hourly_summary$MSF_8_W.Hi_Solar_Rad %>% as.numeric()

rm_cols <- hourly_summary %>% select_if(.,is.factor) %>% names()

hourly_summary_only_weather <- hourly_summary %>% select(-starts_with("inter"),-rm_cols) %>% 
  pivot_longer(cols = !datehour,values_to = "value", names_to= "weather_field") 

ggplot(hourly_summary_only_weather, aes(x=datehour, y=value)) + 
  geom_line(method=lm,se=FALSE,linetype="solid",
            color="#ee5859",size=1.2,fullrange=TRUE,formula = y ~ x)+
  # facet_grid(~period) +
  theme(axis.title.x  = element_text(size = 24),
        axis.title.y  = element_text(size = 24),
        plot.title = element_text(lineheight = 0.9,size = 35,face = "bold"),
        plot.subtitle = element_text(lineheight = 0.7,size = 24),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 24,angle = 45,vjust = -0,hjust = -.2),
        axis.text.y = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.major.x= element_line(size = 0.5, linetype = "dashed",
                                         colour = "#c1c1c1"),
        # panel.grid.minor.x= element_line(size = 0.5, linetype = "dashed",
        # colour = "#c1c1c1"),
        
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        panel.grid.minor.x = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        panel.spacing = unit(.5,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 22,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .45,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        strip.text = element_text(size=24),
        legend.text.align = 0)+
  facet_wrap(.~weather_field,ncol= 1,scales = "free")+
  scale_x_datetime(labels=date_format("%b"),date_breaks  ="1 month")+
  ylab("") +xlab("")+labs(title = "Weather dataset")


ggsave(path = "outputs/charts/",filename ="WEATHER_DATA.jpg" ,width=40,height=200,units="cm",scale = 1.8,dpi = 100,limitsize = FALSE)
