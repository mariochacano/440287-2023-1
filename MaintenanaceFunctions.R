#Create Vectors
dftime_to_vec_seconds <- function(df){df$tdiff %>% lubridate::as.duration() %>% as.numeric()}
vec_seconds_to_hours <- function(vec){vec/3600}

get_df_UTs <- function(dataset){dataset %>% mutate("tdiff"=DET-lag(PAR)) %>% dplyr::select(tdiff) %>% drop_na()}

get_df_DTs <- function(dataset){dataset %>% mutate("tdiff"= PAR-DET) %>% dplyr::select(tdiff) %>% drop_na()}

get_df_TBMs <- function(dataset){dataset %>% mutate("tdiff"=DET-lag(DET)) %>% dplyr::select(tdiff) %>% drop_na()}

get_df_TTMs <- function(dataset){dataset %>% mutate("tdiff"= FIN-INI) %>% dplyr::select(tdiff) %>% drop_na()}

#Availability
Availability <- function(MTBM,MTTM){(MTBM/(MTBM+MTTM))*100}

#Mean Time Between Maintenance
MTBM <- function(MTBMc,MTBMp){1/((1/MTBMc)+(1/MTBMp))}

#Mean Time To Maintenance
MTTM <- function(MTBMc,MTBMp,MTTMc,MTTMp){((MTTMc/MTBMc)+(MTTMp/MTBMp))/((1/MTBMc)+(1/MTBMp))}
