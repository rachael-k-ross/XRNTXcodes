# -------------------------------------
# Script: 06_inj
# Author: Rachael Ross
# Purpose: Identify injection codes
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

inj_codes <- rbind(read.csv(paste0(codes,"inj_cpt.csv")) |>
                     mutate(code=str_pad(code, 5, pad="0")) |>
                     select(code),
                   read.csv(paste0(codes,"inj_pcs.csv")) |>
                     select(code))

# Procedure codes
# -------------------------------------

## Inpatient header file
filelist <- paste0(list.files(src, pattern = "*inpatient_header.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*IPH.*\\.parquet$", recursive = TRUE))
}
#print(filelist)

CD <- paste0("PRCDR_CD_", 1:6)
DT <- paste0("PRCDR_CD_DT_", 1:6)

inj_pxip <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat_wide <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                      filter(link_id %in% xrntx_1st$link_id) |> 
                      select("link_id",SRVC_BGN_DT,SRVC_END_DT,all_of(CD),all_of(DT)) |> 
                      filter(!is.na(PRCDR_CD_1)) |>
                      collect())

  dat_long <- dat_wide[, melt(.SD, 
                         measure.vars = list(CD,DT), 
                         value.name = c("PRCDR_CD", "PRCDR_CD_DT"), 
                         variable.factor=FALSE)
                  ][PRCDR_CD %in% inj_codes$code
                    ][,PRCDR_CD_DT := fifelse(is.na(PRCDR_CD_DT),SRVC_BGN_DT,
                                                fifelse(PRCDR_CD_DT<SRVC_BGN_DT,SRVC_BGN_DT,PRCDR_CD_DT)) #if date missing replace with service begin date
                        ]

  dat_long
} |> rename(date=PRCDR_CD_DT) |> select(link_id,date) |> distinct()

## Outpatient line file
filelist <- paste0(list.files(src, pattern = "*other_services_line.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*OTL.*\\.parquet$", recursive = TRUE))
}
#print(filelist)

inj_pxos <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                 filter(link_id %in% xrntx_1st$link_id) |> 
                 select("link_id",LINE_SRVC_BGN_DT,LINE_SRVC_END_DT,
                          starts_with("LINE_PRCDR_CD")) |>
                   filter(LINE_PRCDR_CD %in% inj_codes$code) |>
                   mutate(date = ifelse(is.na(LINE_PRCDR_CD_DT),LINE_SRVC_BGN_DT,
                                                ifelse(LINE_PRCDR_CD_DT<LINE_SRVC_BGN_DT,LINE_SRVC_BGN_DT,LINE_PRCDR_CD_DT))) |>
                   select(link_id,date) |>
                   collect())

  dat
} |> distinct()


# Save files
# -------------------------------------
saveRDS(inj_pxos,paste0(drv,"inj_pxos.rds"))
saveRDS(inj_pxip,paste0(drv,"inj_pxip.rds"))

print(paste(nrow(inj_pxos),"in inj_pxos"))
print(paste(nrow(inj_pxip),"in inj_pxip"))
toc()









