
library(dplyr)
test <- data.frame(date  = c('1970-01-01','1970-02-03','1970-02-28',
                             '1970-01-15','1970-03-18','1970-03-30','1974-01-01'),
                   variable = c(rep('A',3),rep('B',4)),
                   value = 1:7) %>% mutate(date = as.Date(date))

align.month <- function (x){
  require(dplyr)
  require(lubridate)
  require(zoo)
  x <- x %>% mutate(year = year(date),month = month(date)) %>%
    group_by(variable,year,month) %>%
    arrange(date) %>%
    filter(row_number()==n()) %>%
    mutate(date = as.yearmon(date, frac = 1))
    return(x)
}

align.month(test)

case <- readRDS('output.RDS') %>% tbl_df() %>% filter(name == 'Argentina')
                   
