compute_rsi <- function(prices){
    library(dplyr)
    library(zoo)
    
    rsi <- prices %>% 
        mutate(Change = navpu/lag(navpu)-1) %>% 
        mutate(Advance = ifelse(Change>0, Change, 0)) %>% 
        mutate(Decline = ifelse(Change<0, -Change, 0)) %>% 
        replace_na(list(Advance = 0, Decline = 0)) %>% 
        mutate(row_n = row_number()) %>% 
        mutate(AvgGain = ifelse(row_n < 14, NA, rollmean(Advance, 14))) %>% 
        mutate(AvgLoss = ifelse(row_n < 14, NA, rollmean(Decline, 14))) %>% 
        mutate(RS = ifelse(row_n == 14, 
                           AvgGain/AvgLoss, 
                           ((lag(AvgGain)*13+Advance)/14)/((lag(AvgLoss)*13+Decline)/14)  )) %>% 
        mutate(RSI = 100 - 100/(1+RS)) %>% 
        select(date, RSI)
    
    return(rsi)
}



