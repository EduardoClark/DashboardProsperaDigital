#!/usr/bin/env Rscript
setwd('/srv/shiny-server/ProsperaDigital')

# Translate contacts dataset processor to R from STATA

# plyr BEFORE dplyr
library(plyr)
library(dplyr)
library(lazyeval)

# Directory
runs = read.csv("data/runs.csv", stringsAsFactors = FALSE)

# For miPrueba, miAlta flows and incentives, generate dummy for 'contact started/fin flow'
# http://stackoverflow.com/questions/32194070/creating-a-column-with-replaced-values-using-mutate-and-dplyr
runs <- group_by(runs, contact)
for (flow in c("miPrueba_siNo",
               "miPrueba_cat",
               "miPrueba_num",
               "miPrueba_text",
               "miPrueba_fechas",
               "miAlta_init",
               "miAlta_dueDate",
               "miAlta_apptDate",
               "incentivesCollect1",
               "incentivesCollect2",
               "incentivesCollect3",
               "incentivesCollect4",
               "incentivesCollect5",
               "incentivesCollectF1")) {
  
  started <- paste(flow, "_started", sep="")
  runs <- runs %>%
            mutate_(temp = interp(~ 1*(flow_name == flow), .values = list(flow=flow))) %>%
            arrange(contact, desc(temp)) %>%
            mutate(temp = first(temp))
  # Rename temp with name of flow
  names(runs)[names(runs)=="temp"] <- started

  finished <- paste(flow, "_finished", sep="")
  runs <- runs %>%
    mutate_(temp = interp(~ 1*((flow_name == flow) & (completed == 1)), .values = list(flow=flow))) %>%
    arrange(contact, desc(temp)) %>%
    mutate(temp = first(temp))
  names(runs)[names(runs)=="temp"] <- finished
}

# Dummy for "has activated AuxTexto"
runs <- runs %>%
          mutate(temp = 1*(flow_name=="auxTexto")) %>%
          arrange(contact, desc(temp)) %>%
          mutate(temp = first(temp))
names(runs)[names(runs)=="temp"] <- "auxTexto_started"

# Get number of times she has activated auxTexto
# This may become a general function later on...
## Extract table with run and flow_name
df <- select(runs, contact,
                   flow_name,
                   run,
                   created_on,
                   completed)

## By contact, count number of times she's been in auxTexto AND number of times
## she's sent messages through auxTexto (groups of contacts associated to vocales
## were reformulated on 09/04/2016 but were already in place since before, circa 
## march 23)
df <- df %>%
        distinct(run) %>%
        mutate(auxTexto_in = 1*(flow_name=="auxTexto"),
               auxTexto_out = 1*((flow_name=="auxTexto") &
                                 (completed==1) &
                                 (created_on > "2016-03-23T23:59:59.999Z"))) %>%
        group_by(contact) %>%
        mutate(auxTexto_starts = sum(auxTexto_in),
               auxTexto_sends = sum(auxTexto_out)) %>%
        # Go to contact-level dataframe
        select(contact,
               auxTexto_starts,
               auxTexto_sends) %>%
        distinct(contact)

## Now merge df and runs on contact
runs <- merge(runs, df, by="contact", all.x=TRUE)
rm(df)

# Get first interaction of contact with RapidPro
runs <- runs %>%
          group_by(contact) %>%
          arrange(contact, created_on) %>%
          mutate(contact_created_on = first(created_on))

# Get contacts' response rate (ratio of messages sent over messages sent and messages not sent)
rRate <- runs %>%
           summarise(input_ok = sum(step_input_ok),
                     input = sum(step_input)) %>%
           mutate(rRate = input_ok/input) %>%
           select(contact, rRate)
runs <- merge(runs, rRate, by="contact", all.x=TRUE)

# Select vars for merge with contacts
runs <- select(runs, auxTexto_started,
                     auxTexto_starts,
                     auxTexto_sends,
                     contact,
                     contact_created_on,
                     miPrueba_siNo_started,
                     miPrueba_siNo_finished,
                     miPrueba_cat_started,
                     miPrueba_cat_finished,
                     miPrueba_num_started,
                     miPrueba_num_finished,
                     miPrueba_text_started,
                     miPrueba_text_finished,
                     miPrueba_fechas_started,
                     miPrueba_fechas_finished,
                     miAlta_init_started,
                     miAlta_init_finished,
                     miAlta_dueDate_started,
                     miAlta_dueDate_finished,
                     miAlta_apptDate_started,
                     miAlta_apptDate_finished,
                     belongs_PREGNANT,
                     belongs_PUERPERIUM,
                     rRate)

# Process contacts
contacts <- read.csv("data/contacts.csv", stringsAsFactors = FALSE)

# Get dummy "has future appt date"
today <- paste(strftime(Sys.time(), format="%Y-%m-%d"), "T00:00:00.000Z", sep="" )
contacts <- mutate(contacts, future_apptDate = 0)

# I don't have time to form a nice for loop, as it should be done...
contacts <- contacts %>%
              mutate(future_apptDate = ifelse((future_apptDate == 0),
                                              1*(fields_rp_remapptdate1 > today),
                                              future_apptDate)) %>%
              mutate(future_apptDate = ifelse((future_apptDate == 0),
                                              1*(fields_rp_remapptdate2 > today),
                                              future_apptDate)) %>%
              mutate(future_apptDate = ifelse((future_apptDate == 0),
                                              1*(fields_rp_remapptdate3 > today),
                                              future_apptDate)) %>%
              mutate(future_apptDate = ifelse((future_apptDate == 0),
                                              1*(fields_rp_remapptdate4 > today),
                                              future_apptDate)) %>%
              mutate(future_apptDate = ifelse((future_apptDate == 0),
                                              1*(fields_rp_remapptdate5 > today),
                                              future_apptDate)) %>%
              mutate(future_apptDate = ifelse((future_apptDate == 0),
                                              1*(fields_rp_remapptdatefinal > today),
                                              future_apptDate))

test <- contacts %>%
          select(fields_rp_remapptdate1,
                 fields_rp_remapptdate2,
                 fields_rp_remapptdate3,
                 fields_rp_remapptdate4,
                 fields_rp_remapptdate5,
                 fields_rp_remapptdatefinal,
                 future_apptDate) %>%
          filter(future_apptDate==1)

## Get vocal group size

### Get every contact's associated vocal
contacts <- mutate(contacts, vocal=NA)
for (num in 0:10) {
  group <- paste("groups_", num, sep="")
  contacts <- contacts %>%
                mutate_(vocal = interp(~ ifelse(grepl("tel", group), group, vocal),
                                       .values=list(group=as.name(group),
                                                    vocal=as.name("vocal"))))
}

### Generate vocal group size in separate df
df <- select(contacts, vocal)
df <- df %>%
        rename(urns_0=vocal) %>%
        filter(!is.na(urns_0))
vocals <- contacts %>%
            filter((fields_rp_isvocal_decl==1) | (fields_rp_isaux_decl==1)) %>%
            select(urns_0)

vocals <- vocals %>%
            merge(df, by="urns_0", all.x=TRUE) %>%
            group_by(urns_0) %>%
            mutate(bfs = n()) %>%
            distinct(urns_0)
rm(df)

### Now merge with contacts
contacts <- left_join(contacts, vocals)

# Select columns for merge with runs
contacts <- contacts %>%
              rename(contact = uuid) %>%
              select(contact,
                     phone,
                     vocal,
                     bfs,
                     starts_with("groups_"),
                     starts_with("fields_rp"),
                     starts_with("fields_pd"),
                     starts_with("fields_ext"))

# merge (left join) contacts with runs
contacts <- left_join(contacts, runs)
rm(runs)

# Transform dataset to contact-level
contacts <- distinct(contacts, contact)


# Retrieve all miscellaneous info
## Vocales auxiliares have a recorded miAlta_endDate even when rp_isPregnant == 0
contacts <- mutate(contacts, is_vocAux = 1*((fields_rp_ispregnant==0) & (fields_rp_mialta_enddate != "")))

## Set vocales miAlta_dueDate and apptDate to missing if is_vocAux is set to 1
contacts <- contacts %>%
              mutate(miAlta_dueDate_started = ifelse(is_vocAux==1, NA, miAlta_dueDate_started),
                     miAlta_dueDate_finished = ifelse(is_vocAux==1, NA, miAlta_dueDate_finished),
                     miAlta_apptDate_started = ifelse(is_vocAux==1, NA, miAlta_apptDate_started),
                     miAlta_apptDate_finished = ifelse(is_vocAux==1, NA, miAlta_apptDate_finished))

# Get number of contacts with a due date and appt date
## Get latest appt date whenever it exists
contacts <- mutate(contacts, fields_rp_apptdate = "")

for (i in c(1:5, "final")) {
  current <- paste("fields_rp_remapptdate", i, sep="")
  contacts <- contacts %>%
                mutate_(fields_rp_apptdate =
                        interp(~ ifelse(current != "", current, fields_rp_apptdate),
                               .values=list(current=as.name(current), fields_rp_apptdate=as.name("fields_rp_apptdate"))))
}

## Get contacts that have or don't have due date, appt date (exclude vocaux)
contacts <- contacts %>%
              mutate(has_duedate = ifelse(is_vocAux == 0, 1*(fields_rp_duedate != ""), NA)) %>%
              mutate(has_apptdate = ifelse(is_vocAux == 0, 1*(fields_rp_apptdate != ""), NA))

# Get contacts that finished MIALTA
contacts <- mutate(contacts, finish_alta = 1*(fields_rp_mialta_enddate != ""))

# Get deserted info
contacts <- contacts %>%
              mutate(miPrueba_siNo_deserted = 1*(miPrueba_siNo_started == 1 &
                                               miPrueba_siNo_finished == 0),
                     miPrueba_cat_deserted = 1*(miPrueba_siNo_finished == 1 &
                                              miPrueba_cat_finished == 0),
                     miPrueba_num_deserted = 1*(miPrueba_cat_finished == 1 &
                                              miPrueba_num_finished == 0),
                     miPrueba_text_deserted = 1*(miPrueba_num_finished == 1 &
                                               miPrueba_text_finished == 0),
                     miPrueba_fechas_deserted = 1*(miPrueba_num_finished == 1 &
                                                 miPrueba_fechas_finished == 0),
                     miAlta_init_deserted = 1*(miAlta_init_started == 1 &
                                             miAlta_init_finished == 0),
                     miAlta_dueDate_deserted = 1*(miAlta_init_finished == 1 &
                                                miAlta_dueDate_finished == 0 &
                                                finish_alta == 0),
                     miAlta_apptDate_deserted = 1*(miAlta_dueDate_finished == 1 &
                                                 miAlta_apptDate_finished == 0 &
                                                 finish_alta == 0))
write.csv(contacts,"data/contacts.csv",
          row.names = FALSE,
          na = "",
          quote = FALSE)
remove(list=ls())
  
