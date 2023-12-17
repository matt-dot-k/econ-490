require(fixest)

source("./code/data.R")

# Run TWFE regressions for terror attacks
terror_mod_1 <- feols(
    fml = terror_attacks ~ military_forces + vehicles +
    terrain_loc + facilities_resources | country + month,
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

terror_mod_2 <- feols(
    fml = terror_attacks ~ terrain_loc + facilities_resources | country + month,
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

terror_mod_3 <- feols(
    fml = terror_attacks ~ military_forces + facilities_resources | country + month, # nolint
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

terror_mod_4 <- feols(
    fml = terror_attacks ~ facilities_resources | country + month,
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

# Run TWFE regressions for insurgency actions
insurgency_mod_1 <- feols(
    fml = insurgency_actions ~ military_forces + vehicles +
    terrain_loc + facilities_resources | country + month,
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

insurgency_mod_2 <- feols(
    fml = insurgency_actions ~ terrain_loc + facilities_resources | country + month, # nolint
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

insurgency_mod_3 <- feols(
    fml = insurgency_actions ~ military_forces + facilities_resources | country + month, # nolint
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

insurgency_mod_4 <- feols(
    fml = insurgency_actions ~ facilities_resources | country + month,
    data = inherent_resolve,
    vcov = NW(1) ~ country + month
)

# Get regression tables
terror_results <- etable(
    terror_mod_1, terror_mod_2, terror_mod_3, terror_mod_4,
    tex = TRUE,
    title = "The Effect of Targeting on ISIS Terror Attacks"
)

insurgency_results <- etable(
    insurgency_mod_1, insurgency_mod_2, insurgency_mod_3, insurgency_mod_4,
    tex = TRUE,
    title = "The Effect of Targeting on ISIS Insurgency Actions"
)

# Run event studies
c2_event_study <- feols(attacks ~ strikes + time * after, data = c2_data)

comms_event_study <- feols(attacks ~ strikes + time * after, data = comms_data)

loc_event_study <- feols(attacks ~ strikes + time * after, data = loc_data)

ied_event_study <- feols(attacks ~ strikes + time * after, data = ied_data)

# Get event study table
event_study_results <- etable(
    c2_event_study, comms_event_study, loc_event_study, ied_event_study,
    tex = TRUE,
    title = "Targeting During The Battle of Mosul and ISIS Activity"
)

# Put event study results into new dataframes for plotting
c2_study <- tibble(
    time = c2_data$time,
    target = c2_data$target,
    group = c2_data$group,
    predict(
        c2_event_study,
        newdata = c2_data[, c(2, 5:6)],
        interval = "confidence",
        level = 0.9)
)

comms_study <- tibble(
    time = comms_data$time,
    target = comms_data$target,
    group = c2_data$group,
    predict(
        comms_event_study,
        newdata = comms_data[, c(2, 5:6)],
        interval = "confidence",
        level = 0.9)
)

loc_study <- tibble(
    time = loc_data$time,
    target = loc_data$target,
    group = c2_data$group,
    predict(
        loc_event_study,
        newdata = loc_data[, c(2, 5:6)],
        interval = "confidence")
)

ied_study <- tibble(
    time = ied_data$time,
    target = ied_data$target,
    group = c2_data$group,
    predict(
        ied_event_study,
        newdata = ied_data[, c(2, 5:6)],
        interval = "confidence")
)

# Combine datasets for plot faceting
facilities_study <- rbind(
    c2_study, comms_study) %>%
    arrange(time)

logistics_study <- rbind(
    loc_study, ied_study) %>%
    arrange(time)
