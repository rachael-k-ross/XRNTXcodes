# -------------------------------------
# Script: 05_oud
# Author: Rachael Ross
# Purpose: Identify evidence of OUD in look back period
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load first ntx dates
# -------------------------------------
xrntx_1st <- readRDS(paste0(drv,"xrntx_1st.rds")) |>
  select(link_id,date,lookback_st)


# Load codes lists
# -------------------------------------
ouddx <- read.csv(paste0(codes,"oud_dx.csv")) |>
  select(code)


# Process diagnosis code data
# -------------------------------------

## Inpatient header file

filelist <- paste0(list.files(src, pattern = "*inpatient_header.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*IPH.*\\.parquet$", recursive = TRUE))
}

codevars <- c("ADMTG_DGNS_CD",paste0("DGNS_CD_",1:12))

ouddx_ip <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat_wide <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                      filter(link_id %in% xrntx_1st$link_id) |> 
                      filter(!is.na(DGNS_CD_1)) |>
                      select("link_id","SRVC_BGN_DT","SRVC_END_DT",
                             ADMSN_DT,DSCHRG_DT,starts_with("PRCDR_CD_DT_"),
                             starts_with("DGNS_CD"),"ADMTG_DGNS_CD",) |>
                   collect()) 
                   
  long <- dat_wide[, melt(.SD, measure.vars = list(codevars), value.name = c("DGNS_CD"), variable.factor=FALSE)
              ][DGNS_CD %in% ouddx$code
                ] |>
                   mutate(SRVC_BGN_DT=fifelse(is.na(SRVC_BGN_DT),SRVC_END_DT,SRVC_BGN_DT),
                          maxdate = pmax(SRVC_BGN_DT,SRVC_END_DT,ADMSN_DT,DSCHRG_DT,
                                         PRCDR_CD_DT_1,PRCDR_CD_DT_2,PRCDR_CD_DT_3,PRCDR_CD_DT_4,PRCDR_CD_DT_5,PRCDR_CD_DT_6,
                                         na.rm = TRUE),
                          SRVC_END_DT=fifelse(SRVC_BGN_DT>SRVC_END_DT,maxdate,SRVC_END_DT)) |>
                 select(link_id,SRVC_BGN_DT,SRVC_END_DT) |> distinct() 
  
  long
} |> distinct()


## Other services header file

filelist <- paste0(list.files(src, pattern = "*other_services_header.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*OTH.*\\.parquet$", recursive = TRUE))
}

ouddx_os <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                 filter(link_id %in% xrntx_1st$link_id) |> 
                 filter(DGNS_CD_1 %in% ouddx$code | DGNS_CD_2 %in% ouddx$code) |>
                 select("link_id","SRVC_BGN_DT","SRVC_END_DT") |>
                 distinct() |>
                 collect()) |>
                   mutate(SRVC_BGN_DT=fifelse(is.na(SRVC_BGN_DT),SRVC_END_DT,SRVC_BGN_DT),
                          SRVC_END_DT=fifelse(SRVC_BGN_DT>SRVC_END_DT,SRVC_BGN_DT,SRVC_END_DT))
  
  dat

} |> distinct()


# Identify oud in lookback
# -------------------------------------

ouddx_df <- rbind(ouddx_ip,ouddx_os)
setkey(ouddx_df,link_id,SRVC_BGN_DT,SRVC_END_DT) 

withoud <- foverlaps(xrntx_1st, ouddx_df, 
            by.x = c("link_id", "lookback_st", "date"),
            by.y = c("link_id", "SRVC_BGN_DT", "SRVC_END_DT"),
            type = "any", mult = "first",nomatch=NULL)[,.(link_id,date)] |> distinct()


# Save files
# -------------------------------------
saveRDS(withoud,paste0(drv,"withoud.rds"))

print(paste(nrow(withoud),"in withoud"))
toc()


