#Import Datasets

library(readr)
MissingMigrants <- read_csv("MissingMigrants-Global-2020-12-26T10-03-17.csv")
library(acled.api)
email = 'email'
access.key = ''
# The access key must be provided by acled. You have to register on their website via this access portal:
#https://developer.acleddata.com/

conflict_vitcitms <- acled.api(
  email.address = email,
  access.key = access.key,
)

#selecting variables (deaths for coflict in Africa, ME and Central Asia, deaths for migration in the Mediterranean)

library(tidyverse)
med_x <- conflict_vitcitms %>% filter(region == c("Northern Africa", "Middle Africa", "Western Africa", 'Middle East', 'Caucasus and Central Asia')) %>% 
  select(year, event_date, country, fatalities)
med_y = MissingMigrants %>% 
  select(Region, `Reported Year`,`Reported Date`, `Total Dead and Missing`) %>%
  filter(Region == 'Mediterranean')
med_x$event_date = as.Date(med_x$event_date)
med_y$`Reported Date` = as.Date(med_y$`Reported Date`, format='%B %d, %Y')
med_x = aggregate(fatalities~country+event_date + year, FUN = sum, data = med_x)
med_y = aggregate(`Total Dead and Missing`~`Reported Date` + `Reported Year`, FUN = sum, data = med_y)
med_df = merge(med_x, med_y, by.x = 'event_date', by.y = 'Reported Date',
               all = TRUE)
med_df = select(year, event_date, country, fatalities, `Total Dead and Missing`)

med_df <- med_df %>% 
  select(year, event_date, country, fatalities, `Total Dead and Missing`) %>%
  filter(year > 2014)
med_df$`Total Dead and Missing`[is.na(med_df$`Total Dead and Missing`) ]<-0
colnames(med_df)[4:5] = c('war_fatalities', 'migration_fatalities')

#Plot to see a sort of relation
ggplot(med_df, aes(x = war_fatalities, y = migration_fatalities, group = year)) +
  geom_point(aes(color = as.factor(year))) +
                   facet_wrap(.~ country) + 
                   theme_bw() + 
                   theme(legend.title = element_text('Year')) + xlab('Conflict Victims') + ylab('Migration Victims') + labs(title = 'Migration casualties in the Mediterranean and conflict victims by country', caption = 'SOURCE: ACLED and IOM')

exp_reg = lm(migration_fatalities~war_fatalities*country, data = med_df)
summary(exp_reg)

#Time series comparison 
med_x_ts = aggregate(fatalities ~ event_date + year, data = med_x, FUN = sum)
ts_comparison = merge(med_x_ts, med_y, by.x = 'event_date', by.y = 'Reported Date', all = TRUE)
ts_comparison <- ts_comparison %>%
  filter(year > 2014)
ts_comparison[4] = NULL
ts_comparison$`Total Dead and Missing`[is.na(ts_comparison$`Total Dead and Missing`)] <- 0
colnames(ts_comparison)[3:4] = c('War fatalities', 'Migration fatalities')
ts_comparison_plot <- reshape2::melt(ts_comparison, id.var = c('year', 'event_date'))
ggplot(ts_comparison_plot, aes(x = event_date, y = value, group = variable)) + 
  geom_line(aes(color = variable)) + scale_color_manual(values = c('red', 'black')
    
  ) + xlab('') + ylab('') + 
  labs(title = 'Timeseries comparison', caption = 'SOURCE: ACLED and IOM') + 
  geom_vline(aes(xintercept = year)) +
  theme_bw()
                            
#Crosscorrelation functions 
ccf_migration = ccf(ts_comparison$`War fatalities`, ts_comparison$`Migration fatalities`)

ccf_plot = data.frame(lag = ccf_migration[["lag"]],
           acf = ccf_migration[["acf"]])

ggplot(ccf_plot, aes(x = lag, y = acf)) + 
  geom_col() + geom_hline(yintercept = .05, color = 'red') + 
  geom_hline(yintercept = -.05, color = 'red') + labs(
    title = 'Cross correlation, fatalities at sea/at war'
  ) + theme_bw()

ts_comparison = mutate(ts_comparison, lag_9 = lag(`War fatalities`, 9))
summary(lm(`Migration fatalities`~`lag_9` + `War fatalities`, data = na.omit(ts_comparison)))

ggplot(ts_comparison, aes(x = lag_9, y = `Migration fatalities`)) +
  geom_point() + 
  theme_bw() + xlab('Conflict deaths') + 
  labs(title = 'Migration fatalities and war casualties lagged by 9 days', caption = 'SOURCE: IOM, ACLED')


#Country based lag and snall version plot

lag_list = function(x){
  a = function(y){
    mutate(y, lag1 = lag(`war_fatalities`, 1),
           lag2 = lag(`war_fatalities`, 2),
           lag3 = lag(`war_fatalities`, 3),
           lag4 = lag(`war_fatalities`, 4),
           lag5 = lag(`war_fatalities`, 5),
           lag6 = lag(`war_fatalities`, 6),
           lag7 = lag(`war_fatalities`, 7),
           lag8 = lag(`war_fatalities`, 8),
           lag9 = lag(`war_fatalities`, 9))
  }
  lapply(x, a)
}

lm_list = function(x){
  a = function(y){
    lm(migration_fatalities~.,data = na.omit(y[,-c(1:3)]))
    
  } 
  lapply(x, a)
}

clean = function(x){if(nrow(x) == 0) {rm(x)}
  else(x)}

med_list = split(med_df, f = med_df$country)
med_list = lag_list(med_list)
med_list = lapply(med_list, clean)
med_list <- med_list[!sapply(med_list,is.null)]
reg_lag = lm_list(med_list)
lagged_df = do.call(rbind, med_list)
lagged_df = reshape2::melt(lagged_df, id.vars = c('year', 'event_date', 'country',
                                                  'migration_fatalities'))
reduced = lagged_df %>% filter(country == c('Afghanistan', 
                                  'Iraq', 
                                  'Nigeria', 
                                  'Sudan', 
                                  'Syria', 
                                  'Yemen')) 

ggplot(reduced, aes(x = value, y = migration_fatalities)) + 
  geom_point(aes(color = variable), alpha = .6) + geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  facet_wrap(.~country+variable) + theme_bw() + labs(title = 'Scatterplot of lagged war casualties and migration fatalities', caption = 'SOURCE: IOM and Acled')



#residual syria 
syria_res = data.frame(
  Residuals = reg_lag$Syria$residuals, 
  Fitted = reg_lag$Syria$fitted.values)
ggplot(syria_res, aes(x = Fitted, y = Residuals)) + geom_point() +geom_hline(yintercept = 0, color = 'red') + 
  theme_bw() +
  labs(title = 'Residuals Vs. Fitted values, Syria')

save.image()


