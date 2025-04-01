# -------------------------------------
# Script: 10_describe
# Author: Rachael Ross
# Purpose: Describe size/overlap of cohorts and occurrence of codes
# Notes:
# -------------------------------------

#source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
projpath <- "/home/rr3551/xrntx/"
  src <- "/mnt/processed-data/disability/"
  drv <- paste0(projpath,"data/intermediate/")
  clean <- paste0(projpath,"data/clean/")

# Load data files
# -------------------------------------
cohort <- setDT(readRDS(paste0(drv,"cohort.rds")))

# Summarize codes in trt event 1 
# -------------------------------------
create_summary <- function(data,start,end){
  tosummarize <- data |>
    filter(day>=start & day<=end) |>
    select(link_id,state,starts_with(".")) |>
    mutate(n=1)
  
  byid <- tosummarize[, lapply(.SD, max), by = .(link_id, state)
                    ] |>
  #rowwise() |>
  mutate(.bothpxndc = ifelse(.anypx==1 & .ndc_any==1,1,0),
         .bothntxpxndc = ifelse(.ntxpx==1 & .ndc_any==1,1,0)) |>
  #ungroup() |>
  setDT()
  
  bystate <- byid[, lapply(.SD, sum), by= .(state), .SDcols = !c("link_id")]
  
  return(bystate)
}

create_summary(cohort,0,0)
create_summary(cohort,-1*eventd_pre,eventd_post)

create_summary(cohort,-1*eventdalt_pre,eventdalt_post)

# Event 2
# -------------------------------------

# Primary
setDT(cohort |>
    filter(day>=(-1*eventd_pre) & day<=(eventd_post)) |>
    summarise(tocompare_wpx = max(.anypx), .by=c(link_id,state,event2)) |>
    mutate(n=1) |>
    select(state,tocompare_wpx,n,event2))[, lapply(.SD, sum), by= .(state,tocompare_wpx)]

# SE
setDT(cohort |>
    filter(day>=(-1*eventdalt_pre) & day<=(eventdalt_post)) |>
    summarise(tocompare_wpx = max(.anypx), .by=c(link_id,state,event2)) |>
    mutate(n=1) |>
    select(state,tocompare_wpx,n,event2))[, lapply(.SD, sum), by= .(state,tocompare_wpx)]

# Any NDC in encounter or px
setDT(cohort |>
    filter(day>=(-1*eventd_pre) & day<=(eventd_post)) |>
    summarise(tocompare_wpx = max(.anypx),
              tocompare_ndcenc = max(.ndc_enc),
              .by=c(link_id,state,event2)) |>
    mutate(n=1,
           tocompare_wpx_ndcenc = ifelse(tocompare_wpx==1 | tocompare_ndcenc==1,1,0)) |>
    select(state,tocompare_wpx_ndcenc,n,event2))[, lapply(.SD, sum), by= .(state,tocompare_wpx_ndcenc)]

