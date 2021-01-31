# charts ------------------------------------------------------------------


############################## CHARTS 1: Daily aggregations of rain (mm) per gauge ####################################

precipitation_data_for_charts <- full_data2 %>% select(c("date", "rain.GSB Cox's Bazaar-1227", "rain.GSB Teknaf-1226", 
                                                         "rain.UN Camp 16-1280", "rain.UN Chakmarkul-1278", "rain.UN Kuturc-1279"
))
precipitation_data_for_charts <- precipitation_data_for_charts %>% 
  setnames(old = c("rain.GSB Cox's Bazaar-1227", 
                   "rain.GSB Teknaf-1226", 
                   "rain.UN Camp 16-1280", 
                   "rain.UN Chakmarkul-1278", 
                   "rain.UN Kuturc-1279"),
           new = c("GSB Cox's Bazaar-1227", 
                   "GSB Teknaf-1226", 
                   "UN Camp 16-1280", 
                   "UN Chakmarkul-1278", 
                   "UN Kuturc-1279" ))


precipitation_data_for_charts_pivot_longer_with_NAs <- pivot_longer(precipitation_data_for_charts,cols = !date,names_to = "Device",values_to = "precipitation")


precipitation_data_for_charts_pivot_longer <- precipitation_data_for_charts_pivot_longer_with_NAs %>% filter(!is.na(precipitation))


precipitation_data_for_charts_pivot_longer$Month <- month(precipitation_data_for_charts_pivot_longer$date,label = T,abbr = T)

monthly_data <- precipitation_data_for_charts_pivot_longer %>% dplyr::group_by(Month, Device) %>% dplyr::summarise(
  Day_Count = n() 
)


ggplot(precipitation_data_for_charts_pivot_longer_with_NAs, aes(x=date, y=precipitation)) + 
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
  facet_wrap(.~Device,ncol= 3)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month",
               limits=c(as_date("2019-12-30"),as_date("2020-12-01")),expand = c(0,0))+
  ylab("Precipitation (mm)") +xlab("")+labs(title = "Daily aggregations of rain (mm) per gauge",
                                            subtitle = "Significant data gaps are acknowledged as a limitation")


ggsave(path = "outputs/charts/",filename ="precipitatiaon.jpg" ,width=40,height=20,units="cm",scale = 1.8,dpi = 100)



################## CHARTS 2: % days with sufficient measurements for daily aggregation by instrument #######################

df<-precipitation_with_all_date_time_pivot_longer
df$Device.name <- str_replace_all(df$Device.name ,
                                  "interval.","")

df <- df %>% mutate(
  time = format(time_date, format = "%H:%M:%S"),
  date = as.Date(time_date)
)

dat.summary = df %>% group_by(by8_hr=cut(time_date, "480 min"),date,Device.name) %>%
  summarise(count=sum(!is.na(Interval))) %>% ungroup() %>% mutate(
    available = if_else(count>0,"yes","no",NULL)
  )


dat.summary_2<- dat.summary %>% group_by(date,Device.name) %>%  summarise(
  useable_data = if_else(sum(available == "yes",na.rm=T)>1,1,0,NULL)) %>% ungroup() %>% mutate(
    Month = month(date,label = T,abbr = T),
    month_as_int = month(date)
  ) %>% group_by(Device.name,Month) %>% summarise(
    count= sum(useable_data)
  )

number_of_days <- data.frame(
  Month = dat.summary_2$Month %>% unique(),
  number_of_days = c(31,29,31,30,31,30,31,31,30,31,30))

dat.summary_2 <- dat.summary_2 %>% left_join(number_of_days) %>% mutate(
  percent = count/number_of_days*100
)
palette <- c ("#ee5859","#d1d3d4","#58585a", "#d2cbb8","#0067a9")
ggplot(dat.summary_2, aes(fill=Device.name, y=percent, x=Month)) + 
  geom_bar(position=position_dodge(.8),width = .8, stat="identity")+
  theme(axis.title.x = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(lineheight = 0.9,size = 13,face = "bold"),
        plot.subtitle = element_text(lineheight = 0.7,size = 8),
        # axis.line.x.top = element_line(),
        # axis.line.y.right = element_line(),
        # axis.line.y = element_line(),
        axis.text = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.minor.x= element_line(size = 0.5, linetype = "dashed",
                                         colour = "#c1c1c1"),
        # panel.grid.major.x = element_line(size = 0.5, linetype = "dashed",
        #                                   colour = "#c1c1c1"),
        # panel.grid.minor.y= element_blank(),
        panel.grid.minor.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        # panel.border = element_rect(colour = "#58585a", fill=NA, size=1),
        panel.spacing = unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 8,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .25,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        legend.text.align = 0)+ ylab("Available days (%)")+
  scale_fill_manual(values = palette)+labs(title = "% days with sufficient measurements for daily aggregation by instrument",
                                           subtitle = "For daily aggregation, only days with measurements spanning at least 8 hours are considered sufficient")

# scale_linetype_manual(values = line_typ)

ggsave(path = "outputs/charts/",filename ="monthly_count.jpg" ,width=13,height=7,units="cm",scale = 1.8,dpi = 400)

