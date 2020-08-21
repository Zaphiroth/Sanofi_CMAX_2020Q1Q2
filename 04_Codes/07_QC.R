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




