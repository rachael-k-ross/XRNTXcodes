# -------------------------------------
# Script: 03_age
# Author: Rachael Ross
# Purpose: Identify patients 18-64
# Notes:
# -------------------------------------

source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
tic()

# Load first ntx dates
# -------------------------------------
xrntx_1st <- readRDS(paste0(drv,"xrntx_1st.rds")) |>
  select(link_id,date) |>
  mutate(year=year(date))


# Process enrollment data for age
# -------------------------------------
filelist <- list.files(src, pattern = "*base.*\\.parquet$", recursive = TRUE) 
if(length(filelist)==0){
  filelist <- paste0(list.files(src, pattern = "*BSE.*\\.parquet$", recursive = TRUE))
}

# Get birthdate for each year
age <- foreach(i = 1:length(filelist), .combine='rbind') %dopar% {
  
  dat <- setDT(makelink(open_dataset(paste0(src, filelist[i]), format="parquet")) |>
                 filter(link_id %in% xrntx_1st$link_id) |>
                 mutate(year=as.numeric(RFRNC_YR)) |>
                 select("link_id",year,BIRTH_DT) |>
                 filter(!is.na(BIRTH_DT)) |>
                 collect())
  dat
  
} |> distinct()


withage <- xrntx_1st |>
  inner_join(age, by=c("link_id")) |>
  mutate(dem_age = as.period(interval(start = BIRTH_DT, end = date))$year) |>
  group_by(link_id) |>
  summarise(dem_age = median(dem_age,na.rm=TRUE)) |>
  mutate(incl_age = fifelse(is.na(dem_age),0,ifelse(18<=dem_age&dem_age<=64,1,0))) |>
  filter(incl_age ==1) |>
  select(link_id)

 
# Save files
# -------------------------------------
saveRDS(withage,paste0(drv,"age18_64.rds"))

print(paste(nrow(withage),"in withage"))
toc()