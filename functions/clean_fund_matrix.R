clean_fund_matrix <- function(fund_matrix){
    
    # function for cleaning fund name capitalization
    clean_fund_name <- function(names){
        caps_words <- c("AB","ATRAM","BDO","GS","ESG","US","BPI","ABF","U.S.",
                        "CBC","CTBC","II","III","PSEI","PNB","AUP","PSBank",
                        "RCBC","RBank","SB","UCPB","MM","US$")
        names %>% 
            map_chr(function(x){ 
                str_split(x, " ", simplify=TRUE) %>% 
                    map_chr(function(y){
                        ifelse(y %in% caps_words, y, str_to_title(y))
                    }) %>% 
                    str_c(collapse=" ")
            })
    }
    
    # coerce categorical columns
    fund_matrix <- fund_matrix %>% 
        mutate(Bank = as.factor(Bank)) %>% 
        mutate(Classification = as.factor(Classification)) %>% 
        mutate(`Risk Classification` = as.factor(`Risk Classification`)) %>% 
        mutate(Currency = as.factor(Currency)) %>% 
        mutate(`Fund Name` = clean_fund_name(`Fund Name`))
    
    # prepare complete fund name
    bank_alias <- list(
        "1" = "MetroBank",
        "2" = "PNB",
        "3" = "BPI",
        "4" = "AUB",
        "5" = "BankCom",
        "6" = "BDO",
        "7" = "ChinaBank",
        "8" = "EastWest",
        "9" = "LandBank",
        "10" = "PBCom",
        "11" = "RCBC",
        "12" = "Security Bank",
        "13" = "UnionBank",
        "14" = "UCPB",
        "26" = "DBP",
        "28" = "RCBC",
        "29" = "AB Capital",
        "30" = "PBB",
        "31" = "ATRAM",
        "32" = "PSBank",
        "33" = "Sterling",
        "34" = "Robinsons Bank",
        "36" = "CTBC",
        "39" = "Manulife"
    )
    
    fund_matrix <- fund_matrix %>% 
        mutate(bank_alias = map_chr(bank_id, function(x){ bank_alias[[as.character(x)]] })) %>%
        mutate(`Fund Name` = str_trim(str_replace(`Fund Name`, bank_alias, ""))) %>% 
        mutate(complete_name = paste(bank_alias, `Fund Name`, sep=" - "))

    return(fund_matrix)
}




