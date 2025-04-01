# -------------------------------------
# Script: 09_create
# Author: Rachael Ross
# Purpose: Create cohort
# Notes:
# -------------------------------------

library(ggplot2)
library(ggbreak)
library(scales)

#source("/home/rr3551/xrntx/scripts/00_paths_fxs.R")
#tic()

projpath <- "/home/rr3551/xrntx/"
  src <- "/mnt/processed-data/disability/"
  drv <- paste0(projpath,"data/intermediate/")
  clean <- paste0(projpath,"data/clean/")


# Load files for cohort
# -------------------------------------
xrntx_1st <- readRDS(paste0(drv,"xrntx_1st.rds"))
age18_64 <- readRDS(paste0(drv,"age18_64.rds"))
contenroll <- readRDS(paste0(drv,"contenroll.rds"))
withoud <- readRDS(paste0(drv,"withoud.rds"))

#nrow(xrntx_1st)
#length(unique(xrntx_1st$link_id))

# Explore what states exist in data
setDT(age18_64 |>
  mutate(state = str_sub(link_id, start= -2)))[,.N,.(state)]

# Create cohort
# -------------------------------------
cohort <- xrntx_1st |>
  inner_join(age18_64, by=c("link_id")) 

nrow(cohort)

cohort <- xrntx_1st |>
  inner_join(age18_64, by=c("link_id")) |>
inner_join(contenroll |> select(-date), by = c("link_id")) 

nrow(cohort)

cohort <- xrntx_1st |>
  inner_join(age18_64, by=c("link_id")) |>
  inner_join(withoud |> select(-date), by = c("link_id")) |>
  inner_join(contenroll |> select(-date), by = c("link_id")) |>
  mutate(state = str_sub(link_id, start= -2)) |>
  select(link_id,state,date)

nrow(cohort)

# Load other files
# -------------------------------------

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

##Event 2 data
event2 <- readRDS(paste0(drv,"event2.rds"))


# Examine procedure codes temporally related to day 0
# -------------------------------------

mind = 21
maxd = 21

injcodesbyday <- cohort |>
  full_join(allcodes |> filter(injos==1|injip==1) |> rename(date_code=date), by=c("link_id"), relationship = "many-to-many") |>
  filter(date_code >= (date - mind) & date_code <= (date + maxd)) |>
  mutate(day = date_code - date) |> 
  select(day) |> 
  mutate(Code="Non-specific") |> setDT()

#injbyday <- injcodesbyday[,.N,by=day][order(day)]

ntxpxcodesbyday <- cohort |>
  full_join(allcodes |> filter(pxos==1|pxip==1) |> rename(date_code=date), by=c("link_id"), relationship = "many-to-many") |>
  filter(date_code >= (date - mind) & date_code <= (date + maxd)) |>
  mutate(day = date_code - date) |> 
  select(day) |> 
  mutate(Code="XR-NTX") |> setDT()


combo <- rbind(injcodesbyday,ntxpxcodesbyday)[,.N,by=.(day,Code)][order(day,Code)] |>
  complete(day,Code) |>
  mutate(N=ifelse(is.na(N),0,N),
         Day=as.numeric(day),
         Percent = N/nrow(cohort)*100)

mycolors <- c("#5f9280",
              "#805f92")

# ggplot(data=combo, aes(x=Day,y=N,fill=Code)) +
#   geom_col(position = "dodge"
#     ) +
#     theme_classic() +
#   scale_fill_manual(values = mycolors) +
#     xlab("Day relative to first XR-NTX code") +
#     ylab("Number of patients")+
#     scale_x_continuous(minor_breaks = seq(-1*mind,maxd,1),
#                     labels = as.character(seq(-1*mind,maxd,7)), 
#                      breaks = seq(-1*mind,maxd,7),
#                      expand = c(0.01, 0.01)) +
#   scale_y_continuous(expand = c(0, 0),limits=c(0,4000)) +
#   scale_y_break(c(350, 2400), space=.1, scales=.5,
#                 ticklabels = c(0,100,200,300,2500,3000,3500,4000)) +
#     theme(
#       legend.position = "top",#c(0.8,0.8),
#       legend.title = element_blank(),
#       panel.grid.major.y = element_line(color = "lightgray",
#                                         linewidth = 0.3)
#     ) +
#   geom_hline(yintercept=2320, linewidth=.3) +
#   geom_hline(yintercept=366, linewidth=.3)


combo_alt <- combo %>%
  mutate(Percent=ifelse(Code=="XR-NTX" & day <0, NA, Percent))

ggplot(data=combo_alt, aes(x=Day,y=Percent,color=Code)) +
  geom_line(linewidth=1) +
  geom_point(size=2) +
    theme_classic() +
  scale_color_manual(values = mycolors, name="Injection code") +
    xlab("Day") +
    ylab("Percentage of patients (log scale)")+
    scale_x_continuous(minor_breaks = seq(-1*mind,maxd,1),
                    labels = as.character(seq(-1*mind,maxd,7)), 
                     breaks = seq(-1*mind,maxd,7),
                     expand = c(0.01, 0.01)) +
  #scale_y_continuous(expand = c(0, 0),limits=c(0,15.5)) +
  #scale_y_break(c(1.5, 5), space=.2, scales=.5,
  #              ticklabels = c(0,.5,1,1.5,10,12,14,16)) +
    theme(
      legend.position = c(0.2,0.8),
      #legend.title = "Injection procedure code",
      panel.grid.major.y = element_line(color = "lightgray",
                                        linewidth = 0.3),
      panel.grid.major.x = element_line(color = "darkgray",
                                        linewidth = 0.3),
      panel.grid.minor.x = element_line(color = "lightgray",
                                        linewidth = 0.15),
        #panel.border = element_rect(colour = "black", fill=NA),
      legend.title = element_text(size = 10),legend.text = element_text(size = 10),
      legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    ) +
  #scale_y_log10(
             #breaks=c(0,.1,.5,1,10,16)) + 
  scale_y_continuous(trans=scales::pseudo_log_trans(sigma=.2, base = 10),
                     breaks=c(0,.1,.5,1,15),
                     expand = c(0.02, 0.02)) +
  guides(color = guide_legend(reverse=TRUE))

pxcodesbyday <- cohort |>
  full_join(allcodes |> filter(injos==1|injip==1|pxos==1|pxip==1) |> rename(date_code=date), by=c("link_id"), relationship = "many-to-many") |>
  filter(date_code >= (date - mind) & date_code <= (date + maxd)) |>
  mutate(day = date_code - date) |> 
  select(day) |> setDT()

pxcodesbyday[,.N,by=.(day)][order(day)]


# Assess injection in preperiod
# -------------------------------------
# preinjflag <- cohort |>
#   full_join(allcodes |> filter(injos==1|injip==1) |> rename(date_code=date), by=c("link_id"), relationship = "many-to-many") |>
#   filter(date_code < date & date_code >= (date - preevent)) |>
#   select(link_id) |>
#   distinct() |>
#   mutate(.preinj = 1)

# Add codes to cohort file
# -------------------------------------

cohort_wflags <-  cohort |>
  
  # Add event 2 and pre inj flags
  left_join(event2, by=c("link_id")) |>
  #left_join(preinjflag, by=c("link_id")) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
  
  # Code flags
  full_join(allcodes |> rename(date_code=date), by=c("link_id"), relationship = "many-to-many") |>
  filter(date_code >= (date - eventdalt_pre) & date_code <= (date + eventdalt_post)) |>
  mutate(day = date_code - date,
         .ntxpx = ifelse(pxos==1 | pxip==1,1,0),
         .injpx = ifelse(injos==1 | injip==1,1,0),
         .anypx = ifelse(.ntxpx==1 | .injpx==1,1,0),
         
         .ndc_enc = ifelse(ndcos==1 | ndcip==1,1,0),
         .ndc_rx = ndcrx,
         .ndc_any = ifelse(.ndc_rx==1 | .ndc_enc==1,1,0),
         
         .enc_ip = ifelse(pxip==1 | ndcip==1,1,0),
         .enc_os = ifelse(pxos==1 | ndcos==1,1,0),
         .enc_any = ifelse(.enc_ip==1 | .enc_os==1,1,0),
         .rx = .ndc_rx) 


#table(cohort_wflags$day)



# Save files
# -------------------------------------
saveRDS(cohort_wflags,paste0(drv,"cohort.rds"))

print(paste(nrow(cohort_wflags |> filter(day==0)),"in cohort"))
toc()
     

               