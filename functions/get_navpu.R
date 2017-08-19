get_navpu <- function(fund_id, bank_id, start_date="01-01-1000", end_date="01-01-3000"){

    require(rjson)
    require(dplyr)
    require(lubridate)
    
    if(class(start_date) == "character") start_date = mdy(start_date)
    if(class(end_date) == "character") end_date = mdy(end_date)
    
    url <- paste0("http://www.uitf.com.ph/daily_navpu_details_json.php?",
                  "bank_id=",  bank_id,
                  "&fund_id=", fund_id,
                  "&fmonth=",  month(start_date),
                  "&fday=",    day(start_date),
                  "&fyear=",   year(start_date), 
                  "&tmonth=",  month(end_date),
                  "&tday=",    day(end_date),
                  "&tyear=",   year(end_date),
                  "&btn=",     "Filter")
    
    json_data = fromJSON(file = url)
    navpu_df <- json_data %>% 
        `[[`("thlabels") %>% 
        data_frame(raw_data = .) %>% 
        mutate(navpu = map_dbl(raw_data, function(x){ x %>% 
                str_extract("<b>.*</b>") %>% 
                str_replace("<b>","") %>% 
                str_replace("</b>","") %>% 
                as.numeric()})) %>% 
        mutate(date = map_chr(raw_data, function(x){ x %>% 
                str_extract("Date : .*") %>% 
                str_replace("Date : ","")})) %>% 
        mutate(date = mdy(date)) %>% 
        select(date, navpu)
    
    return(navpu_df)

}



