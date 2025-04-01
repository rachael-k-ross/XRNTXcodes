# -------------------------------------
# Script: 02_firstntx
# Author: Rachael Ross
# Purpose: Identify first XR-NTX code for each beneficiary
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load code files
# -------------------------------------
xrntx_pxos <- readRDS(paste0(drv,"xrntx_pxos.rds"))
xrntx_pxip <- readRDS(paste0(drv,"xrntx_pxip.rds"))
xrntx_ndcip <- readRDS(paste0(drv,"xrntx_ndcip.rds"))
xrntx_ndcos <- readRDS(paste0(drv,"xrntx_ndcos.rds"))
xrntx_ndcrx <- readRDS(paste0(drv,"xrntx_ndcrx.rds"))


# Create first code file 
# -------------------------------------

xrntx_1st <- rbind(xrntx_pxip,
                   xrntx_pxos |> select(-CLM_ID),
                   xrntx_ndcip,
                   xrntx_ndcos |> select(-CLM_ID),
                   xrntx_ndcrx
                   )[, .(date = min(date)), link_id] |>
  # Create lookback window/window for enrollment
  mutate(lookback_st = date - days(lookback),
         mo0 = floor_date(date, unit="month"),
         enroll_st = mo0 - months(enrollmo_pre),
         enroll_end = mo0 + months(enrollmo_post),
         event2_st = date + days(event2_start),
         event2_end = date + days(event2_end)) 


# Save files
# -------------------------------------
saveRDS(xrntx_1st,paste0(drv,"xrntx_1st.rds"))

print(paste(nrow(xrntx_1st),"in xrntx_1st"))
toc()









