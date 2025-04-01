# -------------------------------------
# Script: 07_placeos
# Author: Rachael Ross
# Purpose: Identify place of service for OS codes
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load first ntx dates
# -------------------------------------
xrntx_1st <- readRDS(paste0(drv,"xrntx_1st.rds")) |>
  select(link_id,date)


# Load claim ids for os codes that occur on first day
# -------------------------------------
ndcos <- readRDS(paste0(drv,"xrntx_ndcos.rds")) |>
  inner_join(xrntx_1st, by=c("link_id","date"))

pxos <- readRDS(paste0(drv,"xrntx_pxos.rds")) |>
  inner_join(xrntx_1st, by=c("link_id","date"))

claims <- c(ndcos$CLM_ID,pxos$CLM_ID)


# OS header
# -------------------------------------

filelist <- paste0(list.files(src, pattern = "*other_services_header.*\\.parquet$", recursive = TRUE))
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*OTH.*\\.parquet$", recursive = TRUE))
}
#print(filelist)

poscd <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {

  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                 filter(CLM_ID %in% claims) |> 
                      select("link_id",POS_CD) |> 
                      filter(!is.na(POS_CD)) |>
                      collect())
  dat
} |> distinct()


# Save files
# -------------------------------------
saveRDS(poscd,paste0(drv,"day1_poscd.rds"))

print(paste(nrow(poscd),"in poscd"))
toc()









