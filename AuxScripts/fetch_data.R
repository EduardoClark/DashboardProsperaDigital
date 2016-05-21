# Fetch data required for Baby Center data presentation

rm(list=setdiff(ls(), "root"))

library(dplyr)
library(lazyeval)
library(scales)
library(ggplot2)

# Directory
here = paste(root, "pTasks/analysis/babyCenter", sep="")
runs_path = paste(root, "pProcessed/rapidpro/runs/runs.csv", sep="")
contacts_path = paste(root, "pProcessed/rapidpro/contacts/contacts.csv", sep="")
sample_path = paste(root, "processed/sample/cl_smp_20160314.csv", sep="")
encaseh_path = "c:/users/francisco del villar/desktop/encasehPD_indiv.csv"



# Get % of incorporated clinics

## Get clinics in treatment
df_sample <- read.csv(sample_path)
df_sample <- filter(df_sample, cl_treatmentArm > 0)

## Get clinics that appear under reports with indicator
df_report <- read.csv(paste(here, "/report.csv", sep=""))
df_report <- df_report %>%
               select(CLUES, fecha) %>%
               rename(clues = CLUES) %>%
               mutate(cl_incorporated = 1)

## Merge sample with field reports and set cl_incorp
## Test with all=TRUE and all.x=TRUE, it should yield the same numb of obs.
df_incorporated <- merge(df_sample, df_report, by="clues", all.x=TRUE)
df_incorporated <- mutate(df_incorporated, cl_incorporated = ifelse(substr(clues, 1, 1) == "H",
                                                                    1, cl_incorporated))

## Get % frequency table
### Save number of incorporated clinics by state
tab <- table(df_incorporated$cl_incorporated, df_incorporated$cl_ent_nombre_clCat)

props <- tab
props[1] <- 100*props[1]/64
props[2] <- 100*props[2]/51
props[3] <- 100*props[3]/37
props[4] <- 100*props[4]/113
props[5] <- 100*49/61


plot <- barplot(props, main="Clinics in Prospera Digital",
        names.arg=c(paste("CHIS\n", as.character(tab[1]), "/64", sep=""),
                    paste("GTO\n", as.character(tab[2]), "/51", sep=""),
                    paste("HGO\n", as.character(tab[3]), "/37", sep=""),
                    paste("MEX\n", as.character(tab[4]), "/113", sep=""),
                    paste("PUE\n", "49/61", sep="")),
        col = "orange")
dev.copy(png, "C:/Users/Francisco del Villar/Desktop/clinics.png")
dev.off()

# Clean
rm(list=c("df_incorporated",
          "df_report",
          "df_sample",
          "props",
          "tab",
          "plot"))



contacts <- read.csv(contacts_path)


# Graph Contacts expected due dates

## First, number of contacts that have been in these flows (i.e. !miss due date)
df <- contacts %>%
         filter(fields_rp_duedate != "") %>%
         select(fields_rp_duedate, fields_rp_deliverydate) %>%
         mutate(date = substr(fields_rp_duedate, 1, 10),
                deliver = substr(fields_rp_deliverydate, 1, 10),
                date = ifelse(deliver != "", deliver, date)) %>%
         filter(date >= "2015-12-07") %>%
         filter(date != "216-4-24T0") %>%
         select(date) %>%
         arrange(date)

df$date <- strptime(df$date, format="%Y-%m-%d")
today <- strptime(Sys.Date(), format="%Y-%m-%d")
## Now graph it!
ggplot(data=df, aes(x=date)) +
  geom_histogram(data = subset(df, date <= today),
                 binwidth=7*24*60*60,
                 fill="mediumspringgreen",
                 alpha=0.4) +
  geom_histogram(data = subset(df, date > today),
                 binwidth=7*24*60*60,
                 fill="blue",
                 alpha=0.4) +
  scale_x_datetime(labels = date_format("%b"),
                   breaks=date_breaks("1 month")) +
  xlab("") +
  ylab("No. Participants") +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/hist_1.png", bg="transparent")

rm(df)


# Graph Contacts' week of incorporation

## rp_duedate has a couple (around 70) weird values.
## pd1_duedate is ok, except for 9 outliers

# With pd1
df <- contacts %>%
  filter(fields_pd1_duedate != "") %>%
  select(fields_pd1_duedate, contact_created_on) %>%
  mutate(contact_created_on = substr(contact_created_on, 1, 10),
         contact_created_on = as.Date(contact_created_on, format="%Y-%m-%d")) %>%
  mutate(fields_pd1_duedate = substr(fields_pd1_duedate, 1, 10),
         fields_pd1_duedate = as.Date(fields_pd1_duedate, format="%Y-%m-%d"))

df$week_incorp <- df$fields_pd1_duedate - df$contact_created_on
df <- df %>%
        mutate(week_incorp = 40 - as.integer(week_incorp/7)) %>%
        # Remove outliers
        filter(week_incorp < 40)

ggplot(data=df, aes(x=week_incorp)) +
  geom_histogram(fill="mediumspringgreen",
                 alpha=0.4) +
  xlab("Gestation Week") +
  ylab("No. Participants") +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/hist_pd1.png", bg="transparent")


# With rp
df <- contacts %>%
  filter(fields_rp_duedate != "") %>%
  select(fields_rp_duedate, contact_created_on) %>%
  mutate(contact_created_on = substr(contact_created_on, 1, 10),
         contact_created_on = as.Date(contact_created_on, format="%Y-%m-%d")) %>%
  mutate(fields_rp_duedate = substr(fields_rp_duedate, 1, 10),
         fields_rp_duedate = as.Date(fields_rp_duedate, format="%Y-%m-%d"))

df$week_incorp <- df$fields_rp_duedate - df$contact_created_on
df <- df %>%
  mutate(week_incorp = 40 - as.integer(week_incorp/7)) %>%
  # Remove outliers
  filter(week_incorp < 40 & week_incorp > 0)

ggplot(data=df, aes(x=week_incorp)) +
  geom_histogram(fill="mediumspringgreen",
                 alpha=0.4) +
  xlab("Gestation Week") +
  ylab("No. Participants") +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/hist_rp.png", bg="transparent")




# Total contacts receiving flows (+ in pregnant and puerperium)
preg_contacts <- nrow(filter(contacts, belongs_PREGNANT == 1))
puerp_contacts <- nrow(filter(contacts, belongs_PUERPERIUM == 1))
total_contacts <- preg_contacts + puerp_contacts


# Contact response rate

contacts_in <- filter(contacts, (belongs_PREGNANT == 1) |
                                (belongs_PUERPERIUM == 1) )

rRate <- mean(contacts_in$rRate, na.rm=TRUE)
# 
#  ggplot(data=contacts_in, aes(x=rRate)) +
#    geom_histogram(binwidth=0.01)
rm(contacts_in)


# Now get contact response rate through time

runs <- read.csv(runs_path)
base <- filter(runs, belongs_PREGNANT == 1 | belongs_PUERPERIUM == 1)

## Add contact_created_on to runs

df <- select(contacts, contact,
                       contact_created_on,
                       phone,
                       fields_ext_folio,
                       rRate)
base <- merge(base, df, by="contact", all.x=TRUE)

## Now Convert datetimes to dates
base <- base %>%
          mutate(contact_created_on = substr(contact_created_on, 1, 10),
                 contact_created_on = as.Date(contact_created_on, format="%Y-%m-%d")) %>%
          mutate(created_on = substr(created_on, 1, 10),
                 created_on = as.Date(created_on, format="%Y-%m-%d"))

## Get elapsed time between start of flow and contact creation
base$run_elapsed <- base$created_on - base$contact_created_on

## Translate to biweekly info
base <- mutate(base, run_elapsed = as.integer(run_elapsed/14) + 1)

## Group by contacts and run elapsed and get mean response rate
rRate <- base %>%
           select(contact,
                  run_elapsed,
                  step_input, 
                  step_input_ok,
                  phone,
                  rRate) %>%
           group_by(contact, run_elapsed) %>%
           summarise(step_input = sum(step_input),
                     step_input_ok = sum(step_input_ok),
                     phone = first(phone),
                     rRate = first(rRate)) %>%
           filter(run_elapsed < 9) %>%
           mutate(rRate_biweek = step_input_ok/step_input)

ggplot(rRate, aes(x=run_elapsed, y=rRate_biweek)) +
  geom_point(shape=1, size=1, alpha=0.2) +
  xlab("Elapsed Time (biweekly)") +
  ylab("Participants' Response Rate") +
  scale_x_continuous(breaks = 1:8) +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/rRate_time_1.png", bg="transparent")

ggplot(rRate, aes(x=run_elapsed, y=rRate_biweek)) +
  geom_point(shape=1, size=1, alpha=0.2) +
  geom_smooth(method=lm, color="orange", alpha=0.2) +
  xlab("Elapsed Time (biweekly)") +
  ylab("Participants' Response Rate") +
  scale_x_continuous(breaks = 1:8) +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/rRate_time_2.png", bg="transparent")

# ggplot(rRate, aes(x=run_elapsed, y=rRate_biweek)) +
#   geom_point(shape=1, size=1, alpha=0.2) +
#   geom_smooth(color="orange", alpha=0.2) +
#   xlab("Elapsed Time (biweekly)") +
#   ylab("Participants' Response Rate") +
#   scale_x_continuous(breaks = 1:11) +
#   theme(panel.background = element_rect(fill="transparent", colour=NA),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background = element_rect(fill="transparent", colour=NA))
# ggsave("C:/users/francisco del villar/desktop/rRate_time_3.png", bg="transparent")


# Now get number of observation for each biweek
obs <- rRate %>%
       ungroup() %>%
       select(run_elapsed) %>%
       group_by(run_elapsed) %>%
       summarise(counts = n())

ggplot(obs, aes(x=run_elapsed, y=counts)) +
  geom_line(color = "mediumspringgreen") +
  xlab("Elapsed Time (biweekly)") +
  ylab("Observations") +
  scale_x_continuous(breaks = 1:8) +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/line_1.png", bg="transparent",
                                                        width=120,
                                                       height=40,
                                                        units="mm")
rm(rRate)


# Contact response rate by characteristics
encaseh <- read.csv(encaseh_path)

encaseh <- encaseh %>% 
             filter(folio_red != "") %>%
             rename(fields_ext_folio = folio_red) %>%
             mutate(in_using = 1)

rRate_enc <- base %>%
               distinct(contact) %>%
               mutate(fields_ext_folio = substr(as.character(fields_ext_folio), 1, 14)) %>%
               merge(encaseh, by="fields_ext_folio", all.x=TRUE) %>%
               filter(in_using == 1) %>%
               select(-in_using)

# Generate age groups (old vs. young)
rRate_enc <- rRate_enc %>%
               mutate(young = 1*(EDAD < 23))

ggplot(data=rRate_enc, aes(x=rRate)) +
  geom_histogram(data = subset(rRate_enc, young==1),
                 binwidth=0.05,
                 fill="green",
                 alpha=0.5) +
  geom_histogram(data = subset(rRate_enc, young==0),
                 binwidth=0.05,
                 fill="brown",
                 alpha=0.2) +
  xlab("Response Rate") +
  ylab("Participants") +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/hist_2.png", bg="transparent")

# Mean response rates
tmp <- filter(rRate_enc, young == 1)
mean_young <- mean(tmp$rRate)
tmp <- filter(rRate_enc, young == 0)
mean_old <- mean(tmp$rRate)


# Generate education groups 
rRate_enc <- rRate_enc %>%
  mutate(education = 1*((ULT_NIV == 3) | (ULT_NIV == 4)))

ggplot(data=rRate_enc, aes(x=rRate)) +
  geom_histogram(data = subset(rRate_enc, education==1),
                 binwidth=0.05,
                 fill="green",
                 alpha=0.5) +
  geom_histogram(data = subset(rRate_enc, education==0),
                 binwidth=0.05,
                 fill="brown",
                 alpha=0.2) +
  xlab("Response Rate") +
  ylab("Participants") +
  theme(panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA))
ggsave("C:/users/francisco del villar/desktop/hist_3.png", bg="transparent")

# Mean response rates
tmp <- filter(rRate_enc, education == 1)
mean_ed <- mean(tmp$rRate)
tmp <- filter(rRate_enc, education == 0)
mean_edno <- mean(tmp$rRate)


rm(tmp, rRate_enc)


# Flow completion rate

df <- distinct(base, run)
complRate <- mean(df$completed, na.rm=TRUE)


## Conditional on containing a date variable
df <- base %>%
        mutate(compl_interactive_date = ifelse( flow_has_date == 1,
                                                completed, NA) ) %>%
        distinct(run)
complRate_dates <- mean(df$compl_interactive_date, na.rm=TRUE)

## Flow completion rate by campaign
campaign_rates = NULL

for (camp in c("Concerns",
               "Incentives",
               "Labor",
               "Planning",
               "Preventative",
               "Puerperium",
               "Reminders",
               "T2_Aux",
               "T2_Bfs")) {
  df <- base %>%
    
    mutate_(compl_camp = interp(~ ifelse((flow_campaign == camp), completed, NA),
                                .values = list(camp=as.character(camp)))) %>%
    distinct(run)
  
  nrow = data.frame(campaign = paste("complRate_", camp, sep=""),
                    completion_rate = mean(df$compl_camp, na.rm=TRUE))
  campaign_rates <- rbind(campaign_rates, nrow)
}
rm(base, df, nrow, camp)



# Get answers to concerns flows

## Create new cols: concerns_q1 and concerns_q2
runs <- runs %>%
          mutate(desc_q1 = ifelse(flow_name == "concerns2",
                                      "Light bleeding", ""),
                 desc_q2 = ifelse(flow_name == "concerns2",
                                      "Bleeding w pain", ""),
                 desc_q1 = ifelse(flow_name == "concerns3",
                                      "Dark urine", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns4",
                                      "Vomit", desc_q1),
                 desc_q2 = ifelse(flow_name == "concerns4",
                                      "Regular vomit", desc_q2),
                 desc_q1 = ifelse(flow_name == "concerns5",
                                      "Fever/shivers", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns6",
                                      "Headache", desc_q1),
                 desc_q2 = ifelse(flow_name == "concerns6",
                                      "Strong headache", desc_q2),
                 desc_q1 = ifelse(flow_name == "concerns7",
                                      "Active baby 1", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns8",
                                      "Stomach pain", desc_q1),
                 desc_q2 = ifelse(flow_name == "concerns8",
                                      "Strong Stom. pain", desc_q2),
                 desc_q1 = ifelse(flow_name == "concerns9",
                                      "Dark line", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns10",
                                      "Sight problems", desc_q1),
                 desc_q2 = ifelse(flow_name == "concerns10",
                                      "Blurry/sparkling lights", desc_q2),
                 desc_q1 = ifelse(flow_name == "concerns11",
                                      "Swollen limbs", desc_q1),
                 desc_q2 = ifelse(flow_name == "concerns11",
                                      "Swell and pain", desc_q2),
                 desc_q1 = ifelse(flow_name == "concerns12",
                                      "Contraction 1", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns13",
                                      "Late bleeding", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns14",
                                      "Active baby 2", desc_q1),
                 desc_q1 = ifelse(flow_name == "concerns16",
                                      "Contraction 2", desc_q1) )

ynStep_summary <- function(runs, flow, qNum) {
  # returns a flow_level dataframe with the following info:
  #   Number of runs of flow
  #   Number of runs that arrived to question
  #   Number of runs that answered yes to question
  #   Number of runs that answered no to question
  #   Description of question
  #
  # qNum is an int
  # flow is a string
  # runs is a runs dataframe
  #
  # Assumes that flow is made according to conventions (labels) and
  # that question number is of type Yes/No
  
  # Focus on flow
  df <- filter_(runs, interp(~ flow_name == flow,
                             .values = list(flow=as.character(flow))))
  
  # Dummy for "contact arrived to question x", run-level
  df <- df %>%
          mutate_(arrived = interp(~ 1*(substr(label, 1, 11) ==
                                     paste("Response ", qNum, "_", sep="")),
                                   .values = list(qNum = as.character(qNum)))) %>%
          group_by(run) %>%
          arrange(run, desc(arrived)) %>%
          mutate(arrived = first(arrived))
  
  # Dummy for "answered yes to question x", run-level
  df <- df %>%
          mutate_(ansYes = interp(~ 1*((category_spa == "Si") &
                           (substr(label, 1, 11) == paste("Response ", qNum, "_", sep=""))),
                                  .values = list(qNum = as.character(qNum)))) %>%
          arrange(run, desc(ansYes)) %>%
          mutate(ansYes = first(ansYes))
  
  # Dummy for "answered no to question x", run-level
  df <- df %>%
    mutate_(ansNo = interp(~ 1*((category_spa == "No") &
                                (substr(label, 1, 11) == paste("Response ", qNum, "_", sep=""))),
                            .values = list(qNum = as.character(qNum)))) %>%
    arrange(run, desc(ansNo)) %>%
    mutate(ansNo = first(ansNo))
  
  # Go to run-level and then to flow level
  df <- df %>%
          select(completed,
                 flow_uuid,
                 flow_name,
                 run,
                 arrived,
                 ansYes,
                 ansNo) %>%
          distinct(run) %>%
          mutate(flow_runs = 1) %>%
          group_by(flow_uuid) %>%
          summarise(completion_rate = mean(completed),
                    flow_name = first(flow_name),
                    arrived = sum(arrived),
                    ansYes = sum(ansYes),
                    ansNo = sum(ansNo),
                    flow_runs = sum(flow_runs)) %>%
          ungroup()
  
  # Extract percentages
  df <- df %>%
          mutate(ansNA = (arrived - ansYes - ansNo)/arrived,
                 ansYes = ansYes/arrived,
                 ansNo = ansNo/arrived)
  for (name in c("arrived", "ansYes", "ansNo", "ansNA")) {
    colnames(df)[colnames(df) == name] <- paste(name, "_q", qNum, sep="")
  }

  # Now add question description
  df_using <- runs %>%
                filter_(interp(~ flow_name == flow,
                               .values = list(flow=as.character(flow)))) %>%
                select_(.dots = c("flow_uuid",
                                  paste("desc_q", qNum, sep=""))) %>%
                distinct(flow_uuid)
  df <- merge(df, df_using, by="flow_uuid", all.x=TRUE)
  
  return(df)
}

ynFlow_summary = function(runs, flow) {
  # Decorator on ynStep_summary(). Retrieves number of questions
  # in flow and merges results from ynStep_summary()
  
  # Retrieve number of questions in flow
  df <- runs %>%
          filter_(interp(~ flow_name == flow,
                         .values = list(flow=as.character(flow)))) %>%
          filter(substr(label, 1, 8) == "Response") %>%
          mutate(numQuestions = substr(label, 10, 11)) %>%
          mutate(numQuestions = ifelse(substr(numQuestions, 2, 2) == "_",
                                       as.integer(substr(numQuestions, 1, 1)),
                                       as.integer(numQuestions))) %>%
          arrange(desc(numQuestions))
  questions <- df$numQuestions[1]
  
  # Now merge info given by ynStep_summary for all steps
  result <- ynStep_summary(runs, flow, 1)
  
  if (questions > 1) {
    for (i in 2:questions) {
      new <- runs %>%
               ynStep_summary(flow, i) %>%
               select(-completion_rate, -flow_name, -flow_runs)
      result <- merge(result, new, by="flow_uuid")
    }
  }
  
  return(result)
}


# Now generate dataframe with Concerns campaign data
Concerns_data <- ynFlow_summary(runs, "concerns2")

for (i in c(3:14, 16)) {
  flow <- paste("concerns", i, sep="")
  new <- ynFlow_summary(runs, flow)
  Concerns_data <- bind_rows(Concerns_data, new)
}