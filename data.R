require(dplyr)
require(tidyr)
require(lubridate)

# Load in datasets
cjtf_data <- read.csv(
    file = "./data/strikedata.csv", # nolint
    header = TRUE) %>%
    as_tibble()

terrordb <- read.csv(
    file = "./data/globalterrorismdb.csv",
    header = TRUE) %>% # nolint
    as_tibble() %>%
    mutate(
        date = make_date(iyear, imonth, iday))

# Tidy RAND dataset
strike_data <- cjtf_data %>%
    select(
        Strike_Date, Country, City, Countofstrikes,
        Target_Count, Effect, Target_GroupRevised) %>%
    mutate(
        id = row_number(),
        Strike_Date = as_date(Strike_Date),
        Target_GroupRevised = as.factor(Target_GroupRevised)) %>%
    pivot_wider(
        names_from = Target_GroupRevised,
        values_from = Target_Count,
        values_fill = 0) %>%
    select(-id) %>%
    group_by(
        Strike_Date, city = City, country = Country) %>%
    reframe(
        strikes = unique(Countofstrikes),
        military_forces = sum(`Military Forces`),
        vehicles = sum(Vehicles),
        terrain_loc = sum(`Terrain and LOC`),
        facilities_resources = sum(`Facilities and Resources`)) %>%
    group_by(
        Strike_Date, country = country) %>%
    reframe(
        strikes = sum(strikes),
        military_forces = sum(military_forces),
        vehicles = sum(vehicles),
        terrain_loc = sum(terrain_loc),
        facilities_resources = sum(facilities_resources)) %>%
    group_by(
        month = ceiling_date(Strike_Date, unit = "month"),
        country = country) %>%
    reframe(
        strikes = sum(strikes),
        military_forces = sum(military_forces),
        vehicles = sum(vehicles),
        terrain_loc = sum(terrain_loc),
        facilities_resources = sum(facilities_resources)) %>%
    filter(
        month <= "2019-01-01")

# Dataframe to include Syria during earlier time periods
missing_dates <- tibble(
    month = rep(
        seq(
            as_date("2014-09-01"), as_date("2019-01-01"),
            by = "month"), each = 2),
    country = rep(c("Iraq", "Syria"), 53)
)

cjtf_data_complete <- left_join(
    missing_dates,
    cjtf_data,
    by = c("month", "country")) %>%
    replace_na(
        replace = list(
            strikes = 0,
            military_forces = 0,
            vehicles = 0,
            terrain_loc = 0,
            facilities_resources = 0)
)

# Attacks in  Iraq
terror_iraq <- terrordb %>%
    filter(
        country_txt == "Iraq",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        targtype1_txt != "Military" &
        targtype1_txt != "Militia/Non-State Actor",
        success == 1) %>%
    select(
        date, country_txt) %>%
    filter(
        date >= "2014-08-08" & date <= "2019-05-01") %>%
    group_by(
        date = date, country = country_txt) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        month = ceiling_date(date, "month"), country) %>%
    reframe(
        terror_attacks = sum(attack)) %>%
    mutate(
        terror_attacks = lead(terror_attacks, n = 1)) %>%
    select(
        month, country, terror_attacks) %>%
    filter(
        month <= "2019-01-01")

# Insurgency Actions in Iraq
insurgency_iraq <- terrordb %>%
    filter(
        country_txt == "Iraq",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        alternative_txt == "Insurgency/Guerilla Action",
        success == 1) %>%
    select(
        date, country_txt) %>%
    filter(
        date >= "2014-08-08" & date <= "2019-05-01") %>%
    group_by(
        date = date, country = country_txt) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        month = ceiling_date(date, "month"), country) %>%
    reframe(
        insurgency_actions = sum(attack)) %>%
    mutate(
        insurgency_attacks = lead(insurgency_actions, n = 1)) %>%
    select(
        month, country, insurgency_actions) %>%
    filter(
        month <= "2019-01-01") %>%
    left_join(
        terror_iraq, by = c("month", "country"))

# Attacks for Syria
terror_syria <- terrordb %>%
    filter(
        country_txt == "Syria",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        targtype1_txt != "Military" &
        targtype1_txt != "Militia/Non-State Actor",
        success == 1) %>%
    mutate(
        date = make_date(iyear, imonth, iday)) %>%
    select(
        date, country_txt) %>%
    filter(
        date >= "2014-08-08" & date <= "2019-05-01") %>%
    group_by(
        date = date, country = country_txt) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        month = ceiling_date(date, "month"), country) %>%
    reframe(
        terror_attacks = sum(attack)) %>%
    mutate(
        terror_attacks = lead(terror_attacks, n = 1)) %>%
    select(
        month, country, terror_attacks) %>%
    filter(
        month <= "2019-01-01")

# Insurgency actions for Syria
insurgency_syria <- terrordb %>%
    filter(
        country_txt == "Syria",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        alternative_txt == "Insurgency/Guerilla Action",
        success == 1) %>%
    select(
        date, country_txt) %>%
    filter(
        date >= "2014-08-08" & date <= "2019-05-01") %>%
    group_by(
        date = date, country = country_txt) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        month = ceiling_date(date, "month"), country) %>%
    reframe(
        insurgency_actions = sum(attack)) %>%
    mutate(
        insurgency_actions = lead(insurgency_actions, n = 1)) %>%
    select(
        month, country, insurgency_actions) %>%
    filter(
        month <= "2019-01-01") %>%
    left_join(
        terror_syria, by = c("month", "country"))

attacks_combined <- rbind(
    insurgency_iraq, insurgency_syria) %>%
    arrange(month)

# Dataframe to account for missing weeks
attack_data_complete <- left_join(
    missing_dates,
    attacks_combined,
    by = c("month", "country")) %>%
    replace_na(
        replace = list(
            terror_attacks = 0,
            insurgency_actions = 0)
)

# Merge the GTD and strike data
inherent_resolve <- left_join(
    attack_data_complete,
    cjtf_data_complete,
    by = c("month", "country")
)

# Get Mosul targeting data
c2_targets <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date),
        id = row_number()) %>%
    filter(
        id %in% grep("C2 infrastructure", Target_Group1),
        City == "Mosul")

loc_targets <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date),
        id = row_number()) %>%
    filter(
        id %in% grep("Terrain and LOC", Target_GroupRevised),
        City == "Mosul")

facility_targets <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date),
        id = row_number()) %>%
    filter(
        id %in% grep("Facilities and Resources", Target_GroupRevised),
        City == "Mosul")

ied_targets <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date),
        id = row_number()) %>%
    filter(
        id %in% grep("ied", Target_clean),
        City == "Mosul")

# Dates for specific strikes
c2_strike <- as_date("2017-01-03") %>%
    ceiling_date("week")

comms_strike <- as_date("2016-12-05") %>%
    ceiling_date("week")

loc_strike <- as_date("2016-12-05") %>%
    ceiling_date("week")

ied_strike <- as_date("2016-12-21") %>%
    ceiling_date("week")

# Missing dates for later merges
c2_missing <- tibble(
    week = seq.Date(
        (c2_strike - weeks(20)),
        (c2_strike + weeks(20)),
        by = "week"),
    city = rep("Mosul", length(week))
)

comms_missing <- tibble(
    week = seq.Date(
        (comms_strike - weeks(20)),
        (comms_strike + weeks(20)),
        by = "week"),
    city = rep("Mosul", length(week))
)

loc_missing <- tibble(
    week = seq.Date(
        (loc_strike - weeks(20)),
        (loc_strike + weeks(20)),
        by = "week"),
    city = rep("Mosul", length(week))
)

ied_missing <- tibble(
    week = seq.Date(
        (ied_strike - weeks(20)),
        (ied_strike + weeks(20)),
        by = "week"),
    city = rep("Mosul", length(week))
)

# Get strikes and attacks for 4 targeting windows
c2_strike_window <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date)) %>%
    filter(
        City == "Mosul") %>%
    group_by(
        Strike_Date, city = City) %>%
    reframe(
        strikes = unique(Countofstrikes)) %>%
    group_by(
        week = ceiling_date(Strike_Date, "week"), city) %>%
    reframe(
        strikes = sum(strikes)) %>%
    filter(
        week >= c2_strike - weeks(20) &
        week <= c2_strike + weeks(20)) %>%
    left_join(
        x = c2_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            strikes = 0)) %>%
    mutate(target = rep("C2 Node", length(week)))

c2_attack_window <- terrordb %>%
    filter(
        city == "Mosul",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        targtype1_txt != "Military" &
        targtype1_txt != "Militia/Non-State Actor",
        success == 1) %>%
    select(
        date, city, nkill, nkillter) %>%
    drop_na(
        nkill, nkillter) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        week = ceiling_date(date, "week"), city) %>%
    reframe(
        attacks = sum(attack)) %>%
    filter(
        week >= c2_strike - weeks(20) &
        week <= c2_strike + weeks(20)) %>%
    left_join(
        x = c2_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            attacks = 0))

comms_strike_window <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date)) %>%
    filter(
        City == "Mosul") %>%
    group_by(
        Strike_Date, city = City) %>%
    reframe(
        strikes = unique(Countofstrikes)) %>%
    group_by(
        week = ceiling_date(Strike_Date, "week"), city) %>%
    reframe(
        strikes = sum(strikes)) %>%
    filter(
        week >= comms_strike - weeks(20) &
        week <= comms_strike + weeks(20)) %>%
    left_join(
        x = comms_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            strikes = 0)) %>%
    mutate(target = rep("Communications Tower", length(week)))

comms_attack_window <- terrordb %>%
    filter(
        city == "Mosul",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        targtype1_txt != "Military" &
        targtype1_txt != "Militia/Non-State Actor",
        success == 1) %>%
    select(
        date, city, nkill, nkillter) %>%
    drop_na(
        nkill, nkillter) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        week = ceiling_date(date, "week"), city) %>%
    reframe(
        attacks = sum(attack)) %>%
    filter(
        week >= comms_strike - weeks(20) &
        week <= comms_strike + weeks(20)) %>%
    left_join(
        x = comms_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            attacks = 0))

loc_strike_window <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date)) %>%
    filter(
        City == "Mosul") %>%
    group_by(
        Strike_Date, city = City) %>%
    reframe(
        strikes = unique(Countofstrikes)) %>%
    group_by(
        week = ceiling_date(Strike_Date, "week"), city) %>%
    reframe(
        strikes = sum(strikes)) %>%
    filter(
        week >= loc_strike - weeks(20) &
        week <= loc_strike + weeks(20)) %>%
    left_join(
        x = loc_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            strikes = 0)) %>%
    mutate(target = rep("Roads/Supply Routes", length(week)))

loc_attack_window <- terrordb %>%
    filter(
        city == "Mosul",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        attacktype1_txt == "Bombing/Explosion",
        weapsubtype1_txt == "Vehicle") %>%
    select(
        date, city, nkill, nkillter) %>%
    drop_na(
        nkill, nkillter) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        week = ceiling_date(date, "week"), city) %>%
    reframe(
        attacks = sum(attack)) %>%
    filter(
        week >= loc_strike - weeks(20) &
        week <= loc_strike + weeks(20)) %>%
    left_join(
        x = loc_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            attacks = 0))

ied_strike_window <- cjtf_data %>%
    mutate(
        Strike_Date = as_date(Strike_Date)) %>%
    filter(
        City == "Mosul") %>%
    group_by(
        Strike_Date, city = City) %>%
    reframe(
        strikes = unique(Countofstrikes)) %>%
    group_by(
        week = ceiling_date(Strike_Date, "week"), city) %>%
    reframe(
        strikes = sum(strikes)) %>%
    filter(
        week >= ied_strike - weeks(20) &
        week <= ied_strike + weeks(20)) %>%
    left_join(
        x = ied_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            strikes = 0)) %>%
    mutate(
        target = rep("IED Facility", length(week)))

ied_attack_window <- terrordb %>%
    filter(
        city == "Mosul",
        gname == "Islamic State of Iraq and the Levant (ISIL)",
        attacktype1_txt == "Bombing/Explosion") %>%
    select(
        date, city, nkill, nkillter) %>%
    drop_na(
        nkill, nkillter) %>%
    mutate(
        attack = rep(1, length(date))) %>%
    group_by(
        week = ceiling_date(date, "week"), city) %>%
    reframe(
        attacks = sum(attack)) %>%
    filter(
        week >= ied_strike - weeks(20) &
        week <= ied_strike + weeks(20)) %>%
    left_join(
        x = ied_missing,
        by = c("week", "city")) %>%
    replace_na(
        replace = list(
            attacks = 0))

# Merge attack and targeting data for all 4 targets
c2_data <- left_join(
    c2_strike_window,
    c2_attack_window,
    by = c("week", "city")) %>%
    mutate(
        time = seq(-20, 20),
        after = if_else(week < c2_strike, 0, 1),
        group = if_else(after == 0, "Control", "Intervention")) %>%
    select(-week)

comms_data <- left_join(
    comms_strike_window,
    comms_attack_window,
    by = c("week", "city")) %>%
    mutate(
        time = seq(-20, 20),
        after = if_else(week < comms_strike, 0, 1),
        group = if_else(after == 0, "Control", "Intervention")) %>%
    select(-week)

loc_data <- left_join(
    loc_strike_window,
    loc_attack_window,
    by = c("week", "city")) %>%
    mutate(
        time = seq(-20, 20),
        after = if_else(week < loc_strike, 0, 1),
        group = if_else(after == 0, "Control", "Intervention")) %>%
    select(-week)

ied_data <- left_join(
    ied_strike_window,
    ied_attack_window,
    by = c("week", "city")) %>%
    mutate(
        time = seq(-20, 20),
        after = if_else(week < ied_strike, 0, 1),
        group = if_else(after == 0, "Control", "Intervention")) %>%
    select(-week)
