# -------------------------------------
# Script: 04_contenroll
# Author: Rachael Ross
# Purpose: Assess continuous enrollment prior to first ntx
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load first ntx dates
# -------------------------------------
xrntx_1st <- readRDS(paste0(drv,"xrntx_1st.rds")) |>
  select(link_id,date,enroll_st,enroll_end)


# Process monthly enrollment data
# -------------------------------------

filelist <- list.files(src, pattern = "*base.*\\.parquet$", recursive = TRUE) 
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*BSE.*\\.parquet$", recursive = TRUE))
}

days <- c(paste0("MDCD_ENRLMT_DAYS_0", 1:9),paste0("MDCD_ENRLMT_DAYS_", 10:12))
dual <- c(paste0("DUAL_ELGBL_CD_0", 1:9),paste0("DUAL_ELGBL_CD_", 10:12))
bnft <- c(paste0("RSTRCTD_BNFTS_CD_0", 1:9),paste0("RSTRCTD_BNFTS_CD_", 10:12))

enrollmos <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {
  
  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                 filter(link_id %in% xrntx_1st$link_id) |>
                 select("link_id","RFRNC_YR",
                        starts_with("MDCD_ENRLMT_DAYS"),-c("MDCD_ENRLMT_DAYS_YR"), 
                        starts_with("DUAL_ELGBL_CD"),-c("DUAL_ELGBL_CD_LTST"),
                        starts_with("RSTRCTD_BNFTS_CD"),-c("RSTRCTD_BNFTS_CD_LTST")) |>
                 collect())
  
  ### Make data long, flag enrolled months, filter on enrolled
  long <- dat[, melt(.SD, 
                          measure.vars = list(days,
                                              dual,
                                              bnft), 
                          value.name = c("MDCD_ENRLMT_DAYS", "DUAL_ELGBL_CD", "RSTRCTD_BNFTS_CD"), 
                          variable.factor=FALSE)
                  ][, `:=` (MONTH=as.numeric(variable),
                            variable=NULL,
                            YEAR=as.numeric(RFRNC_YR),
                            RFRNC_YR=NULL,
                            MDCD_ENRLMT_DAYS = ifelse(is.na(MDCD_ENRLMT_DAYS),0,MDCD_ENRLMT_DAYS),
                            nodual_flag = ifelse(grepl("^\\s*$", DUAL_ELGBL_CD)==TRUE,NA,
                                                 ifelse(DUAL_ELGBL_CD=="00",1,0)),
                            fullbnfts_flag = ifelse(grepl("^\\s*$", RSTRCTD_BNFTS_CD)==TRUE,NA,
                                                    ifelse(RSTRCTD_BNFTS_CD=="1",1,0)))
                  ] |>
    mutate(start=update(today(),month=MONTH,year=YEAR,day=1), # first day of the month
           start2=start, # duplicate variable to be used with merge later
           enrolled = case_when(is.na(MDCD_ENRLMT_DAYS) ~ 0, # create enrolled flag for month - mins days covered, no dual, full benefits
                                MDCD_ENRLMT_DAYS<mindayscover ~ 0,
                                nodual_flag %in% c(0,NA) ~ 0,
                                fullbnfts_flag %in% c(0,NA) ~ 0,
                                .default=1)) |>
    filter(enrolled==1) |> # filter to enrolled months
    select(link_id,start,start2,MONTH,YEAR,enrolled) |>
    distinct() # remove dups
  
  long
} |> distinct()
    

# Filter relevant months based on first ntx and filter to individuals with continuous enrollment
setkey(enrollmos,link_id,start,start2) 

contenroll <-  foverlaps(xrntx_1st, enrollmos, 
                     by.x = c("link_id", "enroll_st", "enroll_end"),
                     by.y = c("link_id", "start", "start2"),
                     type = "any", mult = "all")[, .(nummonths = sum(enrolled,na.rm = TRUE)), by=.(link_id,date)
                                                 ][nummonths>(enrollmo_pre + enrollmo_post)] |> select(-nummonths)                                                          

# Save files
# -------------------------------------
saveRDS(contenroll,paste0(drv,"contenroll.rds"))

print(paste(nrow(contenroll),"in contenroll"))
toc()
