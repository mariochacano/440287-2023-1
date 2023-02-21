# ALL FUNCTIONS ARE DESCRIBED BY A DATAFRAME "DF" WITH ONE COLUMN DATA WICH ARE TBFS OR TTRS AND RETURN WITH A NEW COLUMN CALLED MR

i_kaesimo_MR <- function(df){
  n <- nrow(df)
  df %>% 
    mutate("pos"=as.numeric(rownames(.))) %>%
    mutate("MR"=(pos/(n+1))) %>% 
    dplyr::select(-pos)
  }

bernard_MR <- function(df){
  n <- nrow(df)
  df %>% 
    mutate("pos"=as.numeric(rownames(.))) %>%
    mutate("MR"=((pos-0.3)/(n+0.4))) %>% 
    dplyr::select(-pos)
  }

kaplan_meier_MR <- function(df){
  n <- nrow(df)
  df %>% 
    mutate("pos"=as.numeric(rownames(.))) %>%
    mutate("y1"=(((n+0.7)/(n+0.4))*((n-pos+0.7)/(n-pos+1.7))),
           "MR" = accumulate(y1, ~ 1-.y*(1-.x), .init = 0)[-1]) %>% 
    dplyr::select(-y1,-pos)
}

LSR <- function(df){ #Least Squares Regression
  df %>% mutate(
    "x"=log(.[1] %>% unlist()),
    "y"=log(log(1/(1-MR)))
  )
}
