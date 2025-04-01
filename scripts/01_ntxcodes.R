# -------------------------------------
# Script: 01_ntxcodes
# Author: Rachael Ross
# Purpose: Get all XR-NTX codes
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load codes lists
# -------------------------------------
ndc_codes <- read.csv(paste0(codes,"ntx_ndc.csv")) |>
  mutate(code=str_pad(code, 11, pad="0")) |> 
  select(code) |> distinct()

hcpcs_codes <- read.csv(paste0(codes,"ntx_hcpcs.csv")) |>
  select(code)


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

xrntx_pxip <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat_wide <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                 select("link_id",SRVC_BGN_DT,SRVC_END_DT,all_of(CD),all_of(DT)) |> 
                 filter(!is.na(PRCDR_CD_1)) |>
                 collect())
  
  dat_long <- dat_wide[, melt(.SD, 
                         measure.vars = list(CD,DT), 
                         value.name = c("PRCDR_CD", "PRCDR_CD_DT"), 
                         variable.factor=FALSE)
                  ][PRCDR_CD %in% hcpcs_codes$code
                    ][,PRCDR_CD_DT := fifelse(is.na(PRCDR_CD_DT),SRVC_BGN_DT,
                                                fifelse(PRCDR_CD_DT<SRVC_BGN_DT,SRVC_BGN_DT,PRCDR_CD_DT)) #if date missing replace with service begin date
                        ]

  dat_long
} |> rename(date=PRCDR_CD_DT) |> select(link_id,date) |> distinct()
#print(nrow(xrntx_pxip))

## Other services line file
filelist <- paste0(list.files(src, pattern = "*other_services_line.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*OTL.*\\.parquet$", recursive = TRUE))
}
#print(filelist)

xrntx_pxos <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                   select("link_id",CLM_ID,LINE_SRVC_BGN_DT,LINE_SRVC_END_DT,
                          starts_with("LINE_PRCDR_CD")) |>
                   filter(LINE_PRCDR_CD %in% hcpcs_codes$code) |>
                   mutate(date = ifelse(is.na(LINE_PRCDR_CD_DT),LINE_SRVC_BGN_DT,
                                                ifelse(LINE_PRCDR_CD_DT<LINE_SRVC_BGN_DT,LINE_SRVC_BGN_DT,LINE_PRCDR_CD_DT))) |>
                   select(link_id,CLM_ID,date) |>
                   collect())

  dat
} |> distinct()


# Drug codes
# -------------------------------------

## Inpatient line file
filelist <- paste0(list.files(src, pattern = "*inpatient_line.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*IPL.*\\.parquet$", recursive = TRUE))
}

xrntx_ndcip <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                   select("link_id",LINE_SRVC_BGN_DT,LINE_SRVC_END_DT,NDC) |>
                   filter(NDC %in% ndc_codes$code) |>
                   #filter(NDC %in% "00409379501") |> # to confirm that it is working
                   mutate(date = ifelse(is.na(LINE_SRVC_BGN_DT),LINE_SRVC_END_DT,LINE_SRVC_BGN_DT)) |>
                   select(link_id,date) |>
                   collect())

  dat
  
} |> distinct()


## Other services line file
filelist <- paste0(list.files(src, pattern = "*other_services_line.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*OTL.*\\.parquet$", recursive = TRUE))
}

xrntx_ndcos <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                   select("link_id",CLM_ID,LINE_SRVC_BGN_DT,LINE_SRVC_END_DT,NDC) |>
                   filter(NDC %in% ndc_codes$code) |>
                   mutate(date = ifelse(is.na(LINE_SRVC_BGN_DT),LINE_SRVC_END_DT,LINE_SRVC_BGN_DT)) |>
                   select(link_id,CLM_ID,date) |>
                   collect())

  dat
  
} |> distinct()


## Rx line file
filelist <- paste0(list.files(src, pattern = "*rx_line.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*RXL.*\\.parquet$", recursive = TRUE))
}

xrntx_ndcrx <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                   select("link_id",RX_FILL_DT,NDC) |>
                   filter(NDC %in% ndc_codes$code) |>
                   mutate(date = RX_FILL_DT) |>
                   select(link_id,date) |>
                   collect())

  dat
  
} |> distinct()


# Save files
# -------------------------------------
saveRDS(xrntx_pxos,paste0(drv,"xrntx_pxos.rds"))
saveRDS(xrntx_pxip,paste0(drv,"xrntx_pxip.rds"))
saveRDS(xrntx_ndcip,paste0(drv,"xrntx_ndcip.rds"))
saveRDS(xrntx_ndcos,paste0(drv,"xrntx_ndcos.rds"))
saveRDS(xrntx_ndcrx,paste0(drv,"xrntx_ndcrx.rds"))


print(paste(nrow(xrntx_ndcrx),"in xrntx_ndcrx"))
toc()









