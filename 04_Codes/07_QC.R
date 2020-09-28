sanofi.delivery <- read.xlsx('06_Deliveries/Sanofi_Lantus_CHC_Projection_2017Q1_2020Q2_20200819.xlsx', sheet = 2, check.names = FALSE)

check.new <- sanofi.delivery %>% 
  group_by(YM, Market) %>% 
  summarise(Unit_new = sum(Unit, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(YM, Market)

check.old <- history.raw %>% 
  group_by(YM, Market) %>% 
  summarise(Unit_old = sum(Unit, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(YM, Market)

chk <- check.old %>% 
  full_join(check.new) %>% 
  mutate(diff = Unit_new - Unit_old)


check.new <- sanofi.delivery %>% 
  distinct(YM, Market, TC.IV.SHORT.DESC) %>% 
  arrange(YM, Market)

check.old <- history.raw %>% 
  distinct(YM, Market, `TC IV SHORT DESC`) %>% 
  arrange(YM, Market)




year  province     sales
<chr> <chr>        <dbl>
  1 2019  安徽       311263.
2 2019  北京     21312717.
3 2019  福建     11377274.
4 2019  广东       231811.
5 2019  江苏     18041260.
6 2019  山东      1964121.
7 2019  浙江     19339404 


chk <- raw.total %>% 
  filter(product == 'LANTUS             AVS') %>% 
  group_by(year, province) %>% 
  summarise(sales = sum(sales)) %>% 
  ungroup() %>% 
  mutate(sales_19 = c(311263, 21312717, 11377274, 231811, 18041260, 1964121, 19339404)) %>% 
  mutate(growth = sales / sales_19 - 1)


(501796+31649855+11858163+360957+27634749+ 13813179) / (311263+ 21312717+ 11377274+ 231811+ 18041260+ 19339404)





sanofi.history <- read.xlsx('02_Inputs/data/01_Total_Raw_Data.xlsx')

sanofi.chk <- sanofi.history %>% 
  bind_rows(raw.total) %>% 
  filter(product == 'LANTUS             AVS', 
         quarter %in% c('2019Q1', '2019Q2', '2020Q1', '2020Q2')) %>% 
  group_by(year, quarter, province, city, pchc, product, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(sanofi.chk, '05_Internal_Review/Sanofi_Lantus_CHC_Raw_Check_PCHC.xlsx')



chk <- universe.set %>% 
  left_join(proj.parm, by = c("month", "segment", "packid")) %>% 
  mutate(predict_sales = est * slope,
         predict_sales = if_else(predict_sales < 0, 0, predict_sales),
         final_sales = if_else(is.na(sales), predict_sales, sales)) %>% 
  filter(final_sales > 0) %>% 
  group_by(year, month, quarter, province, city, atc4, nfc, molecule, 
           product, packid) %>% 
  summarise(sales = sum(final_sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(province == '浙江')

80661389
187028217



chk <- proj.price %>% 
  filter(product == 'LANTUS             AVS') %>% 
  group_by(year, quarter, province, city, product, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(chk, '05_Internal_Review/Lantus_2020_Result_Check1.xlsx')



sample <- raw.total %>% 
  filter(product == 'LANTUS             AVS', quarter == '2020Q2') %>% 
  group_by(province, city, district, quarter) %>% 
  summarise(sample = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

projection <- proj.sample.m %>% 
  filter(product == 'LANTUS             AVS', quarter == '2020Q2') %>% 
  group_by(province, city, district, quarter) %>% 
  summarise(projection = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

pchc <- universe.pchc %>% 
  mutate(flag = if_else(pchc %in% raw.total$pchc, 1, 0), 
         quarter = '2020Q2') %>% 
  group_by(province, city, district, quarter) %>% 
  summarise(hosp_sample = sum(flag, na.rm = TRUE), 
            hosp_universe = n()) %>% 
  ungroup()

chk <- sample %>% 
  full_join(projection, by = c('province', 'city', 'district', 'quarter')) %>% 
  full_join(pchc, by = c('province', 'city', 'district', 'quarter'))

write.xlsx(chk, '05_Internal_Review/Lantus_2020Q2_Check_n.xlsx')

# 全省PCHC匹配 34m+
# 市内PCHC匹配 36m+
