library(tidyverse)
library(readr)
library(janitor)

# download the data from FAA website:
# https://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download
# the original dataset is 521.6M, so won't upload to GitHub
master_df <- readr::read_delim(here::here("data-raw/ReleasableAircraft/MASTER.txt"), delim = ",") |>
  clean_names() |>
  mutate(last_action_date = as.Date(as.character(last_action_date), format = "%Y%m%d"),
         air_worth_date = as.Date(air_worth_date, format = "%Y%m%d"),
         cert_issue_date = as.Date(cert_issue_date, format = "%Y%m%d"),
         expiration_date = as.Date(expiration_date, format = "%Y%m%d"),
         n_number = str_trim(paste0("N", n_number), side = "both"),
         serial_number = str_trim(serial_number, side = "both"),
         type_registrant = as.numeric(type_registrant),
         year_mfr = as.numeric(year_mfr)) |>
  filter(status_code == "V ") |> # only take V = valid registration (98.6%)
  dplyr::select(-c(street2, fract_owner, other_names_1:other_names_5, kit_mfr:kit_model,
                   x35, status_code, mode_s_code, certification, mode_s_code_hex))


engine_dt <- readr::read_delim(here::here("data-raw/ReleasableAircraft/ENGINE.txt"), delim = ",") |>
  clean_names() |>
  mutate(mfr = str_trim(mfr, side = "both"),
         model = str_trim(model, side = "both"),
         type = as.numeric(type)) |>
  dplyr::select(code:type)

mfr_dt <- readr::read_delim(here::here("data-raw/ReleasableAircraft/ACFTREF.txt"), delim = ",") |>
  clean_names() |>
  mutate(mfr = str_trim(mfr, side = "both"),
         model = str_trim(model, side = "both"),
         type_acft = as.numeric(type_acft),
         type_eng = as.numeric(type_eng),
         no_eng = as.numeric(no_eng),
         no_seats = as.numeric(no_seats),
         ac_weight = extract_numeric(ac_weight),
         speed = as.numeric(speed)
         ) |>
  select(-c(tc_data_sheet, tc_data_holder, x14))

# aircraft document index file (DOCINDEX.txt) is useless. So does RESERVE.txt,DEREG.txt, DEALER.txt

aircraft <- master_df |>
  inner_join(mfr_dt, by = c("mfr_mdl_code" = "code")) |>
  inner_join(engine_dt, by = c("eng_mfr_mdl" = "code")) |>
  dplyr::select(n_number, serial_number, mfr_mdl_code, eng_mfr_mdl, mfr.x:type_eng, mfr.y:model.y, ac_cat:speed, year_mfr,
                last_action_date:cert_issue_date, expiration_date, air_worth_date,
                type_registrant:country) |>
  rename(acft_mfr = mfr.x, acft_model = model.x, build_cert_code = build_cert_ind,
         eng_mfr = mfr.y, eng_model = model.y, acft_weight = ac_weight) |>
  left_join(acft_type_df, by = c("type_acft" = "code")) |>
  left_join(eng_type_df, by = c("type_eng" = "code")) |>
  left_join(ac_cat_df, by = c("ac_cat" = "code")) |>
  left_join(registrant_df, by = c("type_registrant" = "code")) |>
  left_join(build_cert_df, by = c("build_cert_code" = "code")) |>
  left_join(acft_weight_df, by = c("acft_weight" = "code")) |>
  select(-type_acft, -type_registrant, -acft_weight) |>
  rename(acft_weight = acft_weight.y, tail_number = n_number) |>
  select(tail_number:acft_model, acft_type, eng_type, eng_mfr:eng_model, acft_cat,acft_weight, build_cert, no_eng:country)

# check:
aircraft |> filter(tail_number == "N101DU") |> View()

write_csv(aircraft, here::here("data/aircraft.csv"))
########################################################################################
########################################################################################
acft_type_df <- tribble(
  ~code, ~acft_type,
  1, "Glider",
  2, "Balloon",
  3, "Blimp Dirigible",
  4, "Fixed wing single engine",
  5, "Fixed wing multi engine",
  6, "Rotorcraft",
  7, "Weight shift control",
  8, "Powered Parachute",
  9, "Gyroplane",
)

ac_cat_df <- tribble(
  ~code, ~acft_cat,
  1, "Land",
  2, "Sea",
  3, "Amphibian"
)


eng_type_df <- tribble(
  ~code, ~eng_type,
  0, "None",
  1, "Reciprocating",
  2, "Turbo-prop",
  3, "Turbo-shaft",
  4, "Turbo-jet",
  5, "Turbo-fan",
  6, "Ramjet",
  7, "2 Cycle",
  8, "4 Cycle",
  9, "Unknown",
  10, "Electric",
  11, "Rotary"
)

registrant_df <- tribble(
  ~code, ~registrant,
  1, "Individual",
  2, "Partnership",
  3, "Corporation",
  4, "Co-Owned",
  5, "Government",
  7, "LLC",
  8, "Non Citizen Corporation",
  9, "Non Citizen Co-Owned",
)

build_cert_df <- tribble(
  ~code, ~ build_cert,
  0, "Type Certificated",
  1, "Not Type Certificated",
  2, "Light Sport",
)

acft_weight_df <- tribble(
  ~code, ~acft_weight,
  1, "Up to 12,499",
  2, "12,500 - 19,999",
  3, "20,000 and over",
  4, "UAV up to 55"
)
