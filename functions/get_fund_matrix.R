
get_fund_matrix <- function() {
    
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(purrr)
    library(XML)
    library(RCurl)
    
    # download html page
    url1 <- "http://www.uitf.com.ph/fund-matrix.php?sortby=bank&sortorder=asc&class_id=&currency=&btn=Filter#gsc.tab=0"
    html_code <- htmlParse(getURL(url1), asText=TRUE)
    fund_matrix <- html_code %>% 
        getNodeSet(c('//*[@id="inside"]/table')) %>% 
        `[[`(1) %>% 
        readHTMLTable() %>% 
        as_data_frame()
    
    # fix column names
    names(fund_matrix) <- names(fund_matrix) %>% 
        str_replace("\n", "") %>% 
        str_replace("\t", "") %>% 
        str_replace_all("\\s+", " ") %>% 
        str_trim()
    
    # extract fund_id and bank_id from hyperlinks
    fund_links = xpathSApply(doc=html_code, path='//*[@id="inside"]/table/tbody/tr[*]/td[8]/div/a', saveXML) %>% 
        data_frame(Link = .)
    
    # augment fund_matrix
    fund_matrix <- fund_matrix %>% 
        bind_cols(fund_links) %>% 
        mutate(fund_id = map_int(Link, function(x){ x %>% 
                str_extract("fund_id=.*&") %>% 
                str_replace("fund_id=", "") %>% 
                str_replace("&", "") %>% 
                as.integer()})) %>% 
        mutate(bank_id = map_int(Link, function(x){ x %>% 
                str_extract('bank_id=.*" ') %>% 
                str_replace("bank_id=", "") %>% 
                str_sub(start=1, end=-3) %>% 
                as.integer()})) %>% 
        select(Bank, `Fund Name`, fund_id, bank_id)
    
    # download more details
    url2 <- "http://www.uitf.com.ph/print_matrix.php?sort=&sortby=bank&sortorder=asc&class_id=&currency="
    more_fund_details <- htmlParse(getURL(url2), asText=TRUE) %>% 
        getNodeSet(c("/html/body/table")) %>% 
        `[[`(1) %>% 
        readHTMLTable() %>% 
        select(-Remarks) %>% 
        as_tibble()
    
    
    # add details to fund matrix
    fund_matrix <- left_join(fund_matrix, more_fund_details, by=c("Bank","Fund Name"))
    
    return(fund_matrix)
    
}

