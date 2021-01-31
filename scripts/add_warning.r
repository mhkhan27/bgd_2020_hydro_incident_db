
precipitation_interval_and_rendered <- precipitation_interval_and_rendered %>% mutate(
  t.GSB_cox_1 = if_else(is.na(`accumulated.GSB Cox's Bazaar-1227`), NA_real_, 1),
  t.GSB_cox_2 = t.GSB_cox_1+lag(t.GSB_cox_1),
  t.GSB_cox_3 = if_else(is.na(`accumulated.GSB Cox's Bazaar-1227`),4,t.GSB_cox_2),
  t.GSB_cox_4 = if_else(is.na(t.GSB_cox_3),"There is gap in previous data",NA_character_),
  `quality.GSB Cox's Bazaar-1227` = if_else(is.na(`quality.GSB Cox's Bazaar-1227`),t.GSB_cox_4,`quality.GSB Cox's Bazaar-1227`),
  
  t.GSB_tek_1 = if_else(is.na(`accumulated.GSB Teknaf-1226`), NA_real_, 1),
  t.GSB_tek_2 = t.GSB_tek_1+lag(t.GSB_tek_1),
  t.GSB_tek_3 = if_else(is.na(`accumulated.GSB Teknaf-1226`),4,t.GSB_tek_2),
  t.GSB_tek_4 = if_else(is.na(t.GSB_tek_3),"There is gap in previous data",NA_character_),
  `quality.GSB Teknaf-1226` = if_else(is.na(`quality.GSB Teknaf-1226`),t.GSB_tek_4,`quality.GSB Teknaf-1226`),
  
  t.cmp_16_1 = if_else(is.na(`accumulated.UN Camp 16-1280`), NA_real_, 1),
  t.cmp_16_2 = t.cmp_16_1+lag(t.cmp_16_1),
  t.cmp_16_3 = if_else(is.na(`accumulated.UN Camp 16-1280`),4,t.cmp_16_2),
  t.cmp_16_4 = if_else(is.na(t.cmp_16_3),"There is gap in previous data",NA_character_),
  `quality.UN Camp 16-1280` = if_else(is.na(`quality.UN Camp 16-1280`),t.cmp_16_4,`quality.UN Camp 16-1280`),
  
  t.chakmarkul_1 = if_else(is.na(`accumulated.UN Chakmarkul-1278`), NA_real_, 1),
  t.chakmarkul_2 = t.chakmarkul_1+lag(t.chakmarkul_1),
  t.chakmarkul_3 = if_else(is.na(`accumulated.UN Chakmarkul-1278`),4,t.chakmarkul_2),
  t.chakmarkul_4 = if_else(is.na(t.chakmarkul_3),"There is gap in previous data",NA_character_),
  `quality.UN Chakmarkul-1278` = if_else(is.na(`quality.UN Chakmarkul-1278`),t.chakmarkul_4,`quality.UN Chakmarkul-1278`),
  
  t.Kuturc_1 = if_else(is.na(`accumulated.UN Kuturc-1279`), NA_real_, 1),
  t.Kuturc_2 = t.Kuturc_1+lag(t.Kuturc_1),
  t.Kuturc_3 = if_else(is.na(`accumulated.UN Kuturc-1279`),4,t.Kuturc_2),
  t.Kuturc_4 = if_else(is.na(t.Kuturc_3),"There is gap in previous data",NA_character_),
  `quality.UN Kuturc-1279` = if_else(is.na(`quality.UN Kuturc-1279`),t.Kuturc_4,`quality.UN Kuturc-1279`),
  
) %>% select(-starts_with("t.")) %>% select(time_date,
                                            starts_with("accumulated."),
                                            starts_with("quality."),
                                            starts_with("precip."),
                                            everything())



for(i in quality_cols){
  precipitation_interval_and_rendered[1,i] <- precipitation_interval_and_rendered[1,i] == NA_character_ 
  }



