compute_macd <- function(prices){
    
    macd <- prices %>% 
        mutate(row_n = row_number()) %>% 
        mutate(ema_12 = NA) %>% 
        mutate(ema_26 = NA) %>% 
        mutate(macd = NA) %>% 
        mutate(macd_row_n = row_n - 26+1) %>% 
        mutate(signal = NA)
        
    
    for(i in 1:nrow(macd)){
        if(i == 12){
            macd$ema_12[i] <- mean(macd$navpu[1:i])
        } else if(i > 12){
            macd$ema_12[i] <- macd$navpu[i]*2/(12+1) + macd$ema_12[i-1]*(1-2/(12+1))
        }
        
        if(i == 26){
            macd$ema_26[i] <- mean(macd$navpu[1:i])
        } else if(i > 26){
            macd$ema_26[i] <- macd$navpu[i]*2/(26+1) + macd$ema_26[i-1]*(1-2/(26+1))
        }
        
        macd$macd[i] <- macd$ema_12[i] - macd$ema_26[i]
        
    }
    
    for(i in 1:nrow(macd)){
        if(macd$macd_row_n[i] == 9){
            macd$signal[i] <- mean(macd$macd[27:i])
        } else if(macd$macd_row_n[i] > 9){
            macd$signal[i] <- macd$macd[i]*2/(9+1) + macd$signal[i-1]*(1-2/(9+1))
        }
    }
    

    
    macd <- macd %>% 
        select(date, macd, signal) %>% 
        mutate(histogram = macd - signal)
    
    return(macd)
}