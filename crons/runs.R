#!/usr/bin/env Rscript
setwd('/srv/shiny-server/ProsperaDigital')
# Translate runs dataset processor to R from STATA
library(plyr)
library(dplyr)

# Read runs Keep those runs that started after december 7, 2015, drop NAs and remove ALTA flows
runs <- read.csv("data/runs.csv", stringsAsFactors = FALSE) %>% 
  filter(created_on >= "2015-12-07T23:59:59.999Z") %>%
  filter(!is.na(run)) %>% filter(flow_name != "TelcelConfirm")

# Drop administrators
contacts <- read.csv("data/contacts.csv",stringsAsFactors = FALSE) %>%
              select(urns_0, uuid) %>%
              rename(contact = uuid)

runs <- left_join(runs, contacts) %>% filter((urns_0 != "tel:+525518800285") &
                     (urns_0 != "tel:+525571852348") &
                     (urns_0 != "tel:+525517692828"))

# Recode completion dummy
runs <- runs %>% mutate(completed = mapvalues(completed, c("True", "False"), c("1", "0"))) %>%
  mutate(completed=as.numeric(completed))

## group by step
## sort to get non-missing response-type first within group
## group by group, replace response_type (label) with first val in group
## drop observations with missing label
runs <- runs %>%
          group_by(node) %>%
          arrange(node, desc(response_type)) %>%
          mutate( response_type = first(response_type),
                  label = first(label) ) %>%
          ungroup()

# Tag interactive flows
runs <- runs %>%
          group_by(flow_uuid) %>%
          arrange(desc(origin)) %>%
          mutate(flow_interactive = 1*(first(origin) == "values")) %>%
          ungroup()

# Generate "flow contains date variable" dummy
has_date <- runs %>%
              filter(response_type == "f") %>%
              distinct(flow_uuid) %>%
              mutate(flow_has_date = 1) %>%
              select(flow_uuid, flow_has_date)

runs <- runs %>%
          merge(has_date, by="flow_uuid", all.x=TRUE) %>%
          mutate(flow_has_date = ifelse(is.na(flow_has_date), 0, flow_has_date))
print(table(runs$flow_has_date))
rm(has_date)


# Add dummy for "contact belongs to pregnant/puerperium messages"
belongs_to <- function(df, group) {
  # Adds dummy col to df with 1{belongs to group}
  # df is a data.frame, group is a string
  
  # Create column
  df["belongs"] <- 0
  
  # For all groups, substitute
  df <- within(df, belongs[groups_0==group] <- 1)
  df <- within(df, belongs[groups_1==group] <- 1)
  df <- within(df, belongs[groups_2==group] <- 1)
  df <- within(df, belongs[groups_3==group] <- 1)
  df <- within(df, belongs[groups_4==group] <- 1)
  df <- within(df, belongs[groups_5==group] <- 1)
  df <- within(df, belongs[groups_6==group] <- 1)
  df <- within(df, belongs[groups_7==group] <- 1)
  df <- within(df, belongs[groups_8==group] <- 1)
  df <- within(df, belongs[groups_9==group] <- 1)
  df <- within(df, belongs[groups_10==group] <- 1)
  #df <- within(df, belongs[groups_11==group] <- 1)
  
  return(df)
}

contacts <- read.csv("data/contacts.csv",stringsAsFactors = FALSE)
contacts <- contacts %>%
              rename(contact = uuid)
              select(contacts, contact,
                               starts_with("groups_"))
contacts <- contacts %>%
              # "contact in pregnant campaign" dummy  
              belongs_to("PREGNANT") %>%
              mutate(belongs_PREGNANT = belongs,
                     belongs_PREGNANT = ifelse(fields_rp_duedate != "",
                                               belongs_PREGNANT, 0)) %>%
              # "contact in puerperium campaign" dummy
              belongs_to("PUERPERIUM") %>%
              mutate(belongs_PUERPERIUM = belongs,
                     belongs_PUERPERIUM = ifelse(fields_rp_deliverydate != "",
                                                 belongs_PUERPERIUM, 0)) %>%
              # Select vars
              select(contact,
                     belongs_PREGNANT,
                     belongs_PUERPERIUM)

runs <- left_join(runs, contacts)

# Tag steps that require user input
# (and tag those that actually got that input)
runs <- runs %>%
  mutate(step_input_ok = (origin == "values") & (!is.na(text)),
         step_input = (step_input_ok == 1) | is.na(step_time))


# Tag flows by campaign
flows <- runs %>%
           select(flow_uuid,
                  flow_name) %>%
           distinct(flow_uuid) %>%
           arrange(flow_name)

flows["flow_campaign"] <- ""

## Concerns
for (i in 1:18) {
  flow <- paste("concerns", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Concerns")
}

## Incentives
flows <- within(flows, flow_campaign[flow_name == "incentivesInform"] <- "Incentives")
for (i in c(1:5, "F1")) {
  flow <- paste("incentivesCollect", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Incentives")
}

## Labor
for (i in 1:25) {
  flow <- paste("labor_prep", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Labor")
}
for (i in 1:6) {
  flow <- paste("labor_milk", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Labor")
}
for (i in c(1:4, "_init", "_pick")) {
  flow <- paste("labor_getDate", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Labor")
}
flows <- within(flows, flow_campaign[flow_name == "labor_toPuerperium"] <- "Labor")

## Planning
for (i in 1:7) {
  flow <- paste("prePiloto_planning", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Planning")
}

## Preventative
for (i in 1:15) {
  flow <- paste("prevent", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "Preventative")
}

## Puerperium
for (w in 1:13) {
  for (i in 1:25) {
    flow <- paste("week", as.character(w), ".", as.character(i), sep="")
    flows <- within(flows, flow_campaign[flow_name == flow] <- "Puerperium")
  }
}
for (v in 1:4) {
  for (i in 1:2) {
    flow <- paste("checkup", as.character(v), ".", as.character(i), sep="")
    flows <- within(flows, flow_campaign[flow_name == flow] <- "Puerperium")
  }
}

## Reminders
for (v in 1:5) {
  for (i in 1:3) {
    flow <- paste("reminders", as.character(v), ".", as.character(i), sep="")
    flows <- within(flows, flow_campaign[flow_name == flow] <- "Reminders")  
  }
}
for (v in c("Extra", "Final")) {
  for (i in 1:3) {
    flow <- paste("reminders", as.character(v), as.character(i), sep="")
    flows <- within(flows, flow_campaign[flow_name == flow] <- "Reminders")  
  }
}
## T2_aux
for (i in 1:5) { 
  flow <- paste("t2_aux", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "T2_Aux")
}

## T2_bfs
for (i in 1:2) { 
  flow <- paste("t2_bfs", as.character(i), sep="")
  flows <- within(flows, flow_campaign[flow_name == flow] <- "T2_Bfs")
}

flows <- select(flows, flow_uuid,
                       flow_campaign)

runs <- merge(runs, flows, by="flow_uuid", all.x=TRUE)

# Generate categorical var for choice taken in flow auxTexto
# The node in which the decision is made is be0126ac-18fd-421b-8b11-b305ee9bf818
# NOTE: groups of contacts associated to vocales were reformulated on 09/04/2016
# but were already in place since before, circa march 23

runs <- mutate(runs, auxTexto_decision = ifelse((node == "be0126ac-18fd-421b-8b11-b305ee9bf818") &
                                                (created_on > "2016-03-23T23:59:59.999Z") &
                                                (completed == 1),
                                                category_spa, ""))

# Analyze incentivesCollect flows
incentives <- runs %>%
                filter( (flow_name == "incentivesCollect1") |
                        (flow_name == "incentivesCollect2") |
                        (flow_name == "incentivesCollect3") |
                        (flow_name == "incentivesCollect4") |
                        (flow_name == "incentivesCollect5") |
                        (flow_name == "incentivesCollectfinal") ) %>%
                select(category_spa,
                       run,
                       label)

# Get questions

# Generate a new df with labels that start with "split.."
# Category_spa equals "other" once (error), before these flows were fully implemented
df_inc_q <- filter(incentives, (substr(label, 1, 5) == "split") & 
                               (category_spa != "Other"))

df_inc_q <- df_inc_q %>%

# Save question 1 (2) to category_spa if it label equals "split_rand_1_8"
  mutate(inc_q1 = ifelse(label == "split_rand_1_8", strtoi(category_spa), NA),
         inc_q2 = ifelse(label == "split_rand_1_6", strtoi(category_spa), NA)) %>%
  
  # Keep newly created vars and run
  select(run,
         inc_q1,
         inc_q2) %>%
  
  # Fill dataset with non-missing vals and set it to run-level
  group_by(run) %>%
  arrange(run, inc_q1) %>%
  mutate(inc_q1 = first(inc_q1)) %>%
  arrange(run, inc_q2) %>%
  mutate(inc_q2 = first(inc_q2)) %>%
  distinct(run)



# Get answers

# Generate a new df with labels that start with "Response.."
# and whose category_spa != "Other"
df_inc_r <- filter(incentives, (substr(label, 1, 8) == "Response") & 
                     (category_spa != "Other"))

df_inc_r <- df_inc_r %>%

              # Retrieve response number from label
              mutate(response_num = strtoi(substr(label, 10, nchar(label)-2))) %>%
              
              # Save response 1 (2) to response_num if response_num is less (greater or eq) to 9
              mutate(inc_r1 = ifelse(response_num < 9, strtoi(category_spa), NA),
                     inc_r2 = ifelse(response_num > 8, strtoi(category_spa), NA)) %>%
  
              # Keep newly created vars and run
              select(run,
                     inc_r1,
                     inc_r2) %>%
    
              # Fill dataset with non-missing vals and set it to run-level
              group_by(run) %>%
              arrange(run, inc_r1) %>%
              mutate(inc_r1 = first(inc_r1)) %>%
              arrange(run, inc_r2) %>%
              mutate(inc_r2 = first(inc_r2)) %>%
              distinct(run)


# Add questions and answers to incentives
incentives <- select(incentives, run)
incentives <- merge(incentives, df_inc_q, by="run", all.x=TRUE)
incentives <- merge(incentives, df_inc_r, by="run", all.x=TRUE)


# Add everything to runs
runs <- merge(runs, incentives, by="run", all.x=TRUE)
rm(df_inc_r, df_inc_q, incentives)

# Extract answers to concerns questions
#concerns <- filter(runs, )

write.csv(runs, "data/runs.csv")
remove(list=ls())
