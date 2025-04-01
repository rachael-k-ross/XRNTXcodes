# -------------------------------------
# Script: 00_paths_fxs
# Author: Rachael Ross
# Purpose: Define paths, load libraries, set assumptions for XR-NTX describe project
# Notes:
# -------------------------------------

#################
# Libraries
#################
library(arrow)
library(tidyverse)
library(lubridate)
library(tictoc)
library(sqldf)
library(data.table)
library(stringr)

library(future)
library(furrr)
library(doParallel)
# options(cores=50)
registerDoParallel()
plan(multicore)
getDoParWorkers()

#################
# Paths
#################

# Get dattype from command line
dattype <- commandArgs(trailingOnly=TRUE)
#dattype <- "moud_sample"
#dattype <- "moud_full"
#dattype <- "disab_full"

# Root paths
projpath <- "/home/rr3551/xrntx/"

if (dattype=="moud_full") {
  src <- "/mnt/processed-data/moud/parsed/12692/"
  drv <- paste0(projpath,"data/sample/intermediate/")
  clean <- paste0(projpath,"data/sample/clean/")
} else if (dattype=="moud_sample") {
  src <- "/home/data/moud/global/sample/raw/"
  drv <- paste0(projpath,"data/sample/intermediate/")
  clean <- paste0(projpath,"data/sample/clean/")
} else if (dattype=="disab_full") {
  src <- "/mnt/processed-data/disability/"
  drv <- paste0(projpath,"data/intermediate/")
  clean <- paste0(projpath,"data/clean/")
} else if (dattype=="disab_sample") {
  src <- "/mnt/processed-data/disability/"
  drv <- paste0(projpath,"data/sample/intermediate/")
  clean <- paste0(projpath,"data/sample/clean/")
}

# Project code lists
codes <- paste0(projpath,"codes/")

#################
# Functions
#################

makelink <- function(dat){
  dat |> 
    mutate(link_id = ifelse(grepl("^\\s*$", BENE_ID)==TRUE|is.na(BENE_ID),
                            paste0(MSIS_ID,STATE_CD),
                            paste0(BENE_ID,STATE_CD)))
}


#################
# Assumptions
#################

enrollmo_pre <- 2 # Months required for continuous enrollment before
enrollmo_post <- 2 # Months required for continuous enrollment after
lookback <- 90 # Days in lookback period, for OUD evidence and defining covariates
mindayscover <- 15 # Minimum days with coverage in a month to be considered enrolled
#ntxinjlength <- 28 # Days of treatment for injection
event2_start <- 21 # Min days after first XR-NTX code to look for event 2
event2_end <- 42 # Max days after first XR-NTX code to look for event 2

eventd_pre <- 1 # Length in days of a treatment event
eventd_post <- 8 # Length in days of a treatment event

eventdalt_pre <- 8 # Length in days of a treatment event
eventdalt_post <- 16 # Length in days of a treatment event


