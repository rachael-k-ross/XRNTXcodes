# -------------------------------------
# Script: 08_event2
# Author: Rachael Ross
# Purpose: Identify 2nd treatment events
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load files
# -------------------------------------
xrntx_1st <- readRDS(paste0(drv,"xrntx_1st.rds")) |> select(link_id,date,starts_with("event2"))

## Codes
xrntx_pxos <- readRDS(paste0(drv,"xrntx_pxos.rds")) |> select(-CLM_ID) |> distinct() |> mutate(pxos = 1)
xrntx_pxip <- readRDS(paste0(drv,"xrntx_pxip.rds")) |> mutate(pxip = 1)
xrntx_ndcip <- readRDS(paste0(drv,"xrntx_ndcip.rds")) |> mutate(ndcip = 1)
xrntx_ndcos <- readRDS(paste0(drv,"xrntx_ndcos.rds")) |> select(-CLM_ID) |> distinct() |> mutate(ndcos = 1)
xrntx_ndcrx <- readRDS(paste0(drv,"xrntx_ndcrx.rds")) |> mutate(ndcrx = 1)
inj_pxos <- readRDS(paste0(drv,"inj_pxos.rds")) |> mutate(injos = 1)
inj_pxip <- readRDS(paste0(drv,"inj_pxip.rds")) |> mutate(injip = 1)
filelist <- list(xrntx_pxos,xrntx_pxip,xrntx_ndcip,xrntx_ndcos,xrntx_ndcrx,inj_pxos,inj_pxip)

allcodes <- reduce(filelist, ~full_join(.x,.y), by=c("link_id","date")) |> 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

ntxcodes <- allcodes |>
  filter(pxos==1 | pxip==1 | ndcos==1 | ndcip==1 | ndcrx==1)


# Find event2 and then all codes in event length
# -------------------------------------
event2 <- xrntx_1st |>
  full_join(ntxcodes |> rename(date_code=date) |> select(link_id,date_code), by=c("link_id"), relationship = "many-to-many") |>
  filter(date_code >= event2_st & date_code <= event2_end) |>
  filter(date_code==min(date_code), .by=link_id) |>
  select(link_id,date_code) |>
  rename(event2_day1 = date_code)

# event2_codes <- event2 |>
#   full_join(allcodes |> rename(date_code=date), by=c("link_id"), relationship = "many-to-many") |>
#   filter(date_code >= event2_day1 & date_code <= (event2_day1 + eventlength - 1)) |>
#   mutate(day = date_code - event2_day1 + 1,
#          .ntxpx = ifelse(pxos==1 | pxip==1,1,0),
#          .injpx = ifelse(injos==1 | injip==1,1,0),
#          .anypx = ifelse(.ntxpx==1 | .injpx==1,1,0))


# Create event2 flags
# -------------------------------------
# event2_flags <- event2_codes |>
#   summarise(event2wpx = max(.anypx), .by=link_id) |>
#   mutate(event2=1)

event2_flags <- event2 |>
  select(link_id) |>
  distinct() |>
  mutate(event2=1)  


# Save files
# -------------------------------------
saveRDS(event2_flags,paste0(drv,"event2.rds"))

print(paste(nrow(event2_flags),"in event2"))
toc()









