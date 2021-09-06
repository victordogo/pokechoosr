## code to prepare `pokedex_min` dataset goes here
# this code also prepares internal dexes for each game instead of filtering
# pokedex_min each time


## POKEDEX_MIN
library(dplyr)

col_select <- c("pokedex_number", "name", "status",
                "type_1", "type_2", "hp", "attack",
                "defense", "sp_attack", "sp_defense",
                "speed", "against_normal", "against_fire", "against_water",
                "against_electric", "against_grass", "against_ice",
                "against_fight", "against_poison", "against_ground",
                "against_flying", "against_psychic", "against_bug",
                "against_rock", "against_ghost", "against_dragon",
                "against_dark", "against_steel", "against_fairy")

pokedex_min <- readr::read_csv("data-raw/pokedex.csv") |>
  select(-...1) |> # First column doesnt contain variables
  select(tidyselect::all_of(col_select)) |>
  rowwise() |>
  mutate(sum_against = sum(c_across(against_normal:against_fairy)),.keep="unused")


# rbgy and frlg dex

rbgy_dex <- pokedex_min %>% dplyr::filter(pokedex_number<=151,
                               !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))

# lets go eevee/pikachu dex

pe_dex <- pokedex_min %>% dplyr::filter(pokedex_number<=151,
                                      !stringr::str_detect(name,"Galarian|Ash")) %>%
  tibble::add_row(
    dplyr::filter(pokedex_min, name %in% c("Meltan","Melmetal"))
  )

# gold/silver/crystal dex

gsc_dex <- pokedex_min %>% dplyr::filter(pokedex_number<=251,
                                      !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))


# heartgold/soulsilver dex

hgss_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(1:251,458,461:474,
                                                            438,440,439,446,424,
                                                            430,429),
                                      !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))

# ruby/sapphire/emerald dex

rse_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(252:386,63:68,118:121,
                                                            129,130,183,
                                                            184,72:76,41:45,
                                                            169,81,82,100,101,
                                                            182,84,85,218,219,
                                                            88,89,109:112,
                                                            28,227,174,39,40,
                                                            37,38,25:27,54,55,
                                                            202,177,178,203,
                                                            231,232,127,214,
                                                            222,170:172,
                                                            116,117,230),
                                      !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))

# omega ruby/alpha sapphire dex

oras_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(252:386,63:68,118:121,
                                                                129,130,183,
                                                                184,72:76,41:45,
                                                                169,81,82,100,101,
                                                                182,84,85,218,219,
                                                                88,89,109:112,
                                                                28,227,174,39,40,
                                                                37,38,25:27,54,55,
                                                                202,177,178,203,
                                                                231,232,127,214,
                                                                222,170:172,
                                                                116,117,230,475,
                                                                462,407,477,433,
                                                                464,478),
                                          !stringr::str_detect(name,"Alolan|Galarian|Partner|Ash"))

# diamond/pearl/platinum dex

dpp_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(387:493,63:68,129,130,
                                                               315,41,42,169,
                                                               74:78,208,54,55,
                                                               265:269,214,190,92:95,
                                                               200,198,118,119,339,
                                                               340,358,307,308,185,
                                                               122,113,173,35,36,172,
                                                               25,26,163,164,143,
                                                               201,194,195,278,279,
                                                               203,298,183,184,223,
                                                               224,72,73,349,350,
                                                               226,215,207,299,280,
                                                               281,282,108,133:136,
                                                               196,197,333,334,175,176,
                                                               228,229,81,82,114,193,
                                                               357,111,112,355,356,
                                                               137,233,123,212,239,
                                                               125,240,126,220,221,
                                                               361,362,359),
                                         !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))

# black/white dex

bw_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% 494:649,
                                      !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))

# black2/white2 dex

b2w2_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(494:649,179:181,
                                                            54,55,298,183,
                                                            184,447,448,206,
                                                            109,110,81,82,
                                                            462,58,59,240,
                                                            126,467,239,125,
                                                            466,19,20,41,
                                                            42,169,88,89,95,
                                                            208,300,301,427,
                                                            428,173,35,36,
                                                            133:136,196,197,
                                                            470,471,27,28,
                                                            328:330,406,315,
                                                            407,415,416,214,
                                                            127,418,419,351,
                                                            299,476,304:306,
                                                            343,344,335,336,
                                                            451,452,227,322,
                                                            323,325,326,425,
                                                            426,353,354,278,
                                                            279,337,338,359,
                                                            114,465,207,472,
                                                            213,458,226,223,
                                                            224,222,120,121,
                                                            320,321,131,333,
                                                            363:365,334,37,
                                                            38,436,437,215,
                                                            461,225,220,221,
                                                            473,132,374:376,
                                                            86,87,287:289,
                                                            341,342,174,39,
                                                            40,108,463,193,
                                                            469,357,453:455,
                                                            246:248),
                                      !stringr::str_detect(name,"Mega|Alolan|Galarian|Partner|Ash"))

# xy dex

xy_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(650:721,1:18,
                                                            511:516,172,25,26,
                                                            399,400,206,298,
                                                            183,184,412:414,
                                                            283,284,129,130,
                                                            341,342,118,119,
                                                            318,319,54,55,83,
                                                            447,448,280:282,
                                                            475,406,407,315,
                                                            165,166,415,416,300,
                                                            301,84,85,311,312,
                                                            316,317,559,560,
                                                            63:65,43:45,182,
                                                            161,162,290:292,
                                                            352,543:545,531,
                                                            235,453,454,580,
                                                            581,313,314,
                                                            187:189,446,143,
                                                            293:295,307,308,
                                                            41,42,169,610:612,
                                                            425,426,619,620,
                                                            335,336,325,326,359,
                                                            337,338,371:373,
                                                            276:279,557,558,
                                                            72,73,320,321,
                                                            370,120,121,90,
                                                            91,211,116,117,
                                                            230,369,551:553,
                                                            449,450,111,112,
                                                            464,95,208,527,
                                                            528,66:68,104,105,
                                                            115,303,142,597,
                                                            598,209,210,309,
                                                            310,228,229,133:136,
                                                            196,197,470,471,
                                                            587,193,469,561,
                                                            622,623,299,476,
                                                            296,297,538,539,
                                                            396:398,434,435,
                                                            29:34,433,358,
                                                            439,122,577:579,
                                                            360,202,524:526,
                                                            302,128,241,179:181,
                                                            127,214,417,79,80,
                                                            199,102,103,441,
                                                            485,226,366:368,
                                                            222:224,170,171,
                                                            594,131,144:146,
                                                            50,51,328:330,
                                                            443:445,74:76,
                                                            218,219,213,451,
                                                            452,194,195,588,
                                                            589,616,617,69:71,
                                                            455,92:94,60:62,
                                                            186,23,24,618,339,
                                                            340,509,510,261,262,
                                                            504,505,624,625,
                                                            198,430,590,591,
                                                            270,271,272,418,
                                                            419,550,607:609,
                                                            479,81,82,462,
                                                            100,101,568,569,
                                                            220,221,473,613,
                                                            614,238,124,582:584,
                                                            459,460,225,215,
                                                            461,532:534,324,
                                                            27,28,304:306,
                                                            246:248,631,632,
                                                            167,168,21,22,
                                                            615,227,207,472,
                                                            163,164,174,39,40,
                                                            353,354,570,571,
                                                            574:576,438,185,
                                                            327,216,217,108,
                                                            463,123,212,132,
                                                            333,334,621,633:635,
                                                            147:149,150),
                                        !stringr::str_detect(name,"Alolan|Galarian|Partner|Ash"))

# sun/moon dex

sm_dex <- pokedex_min %>% dplyr::filter(pokedex_number %in% c(722:802,19,20,10:12,
                                                              165:168,172,25,26,
                                                              438,185,440,113,242,
                                                              446,143,79,80,199,278,
                                                              279,63:65,52,53,81,82,
                                                              462,88,89,58,59,96,97,
                                                              296,297,235,92:94,
                                                              425,426,200,429,41,
                                                              42,169,50,51,21,22,
                                                              627:630,56,57,225,
                                                              546:549,54,55,129,
                                                              130,339,340,66:68,
                                                              524:526,703,302,
                                                              327,72,73,456,457,
                                                              370,222,90,91,
                                                              371:373,506:508,
                                                              133:136,196,197,
                                                              470,471,700,174,39,
                                                              40,128,241,283,284,
                                                              46,47,60:62,186,118,
                                                              119,349,350,594,661:663,
                                                              104,105,115,240,126,467,
                                                              127,704:706,351,120,121,
                                                              408:411,564:567,708,
                                                              709,299,476,170,
                                                              171,718,568,569,
                                                              227,132,173,35,36,
                                                              374:376,137,233,
                                                              474,674,675,324,
                                                              239,125,466,74:76,
                                                              551:553,328:330,
                                                              443:445,707,359,
                                                              361,362,478,215,
                                                              461,27,28,37,38,
                                                              582:584,209,210,
                                                              422,423,369,318:321,
                                                              131,102,103,587,
                                                              123,212,198,198,
                                                              430,447,448,147:149,
                                                              142),
                                        !stringr::str_detect(name,"Galarian|Partner"))

# ultra sun/ultra moon dex

usum_dex <- sm_dex %>%
  tibble::add_row(
    dplyr::filter(pokedex_min, pokedex_number %in% c(427,428,686,687,570,571,676,
                                                     439,122,23,24,206,198,430,
                                                     714,715,701,669:671,238,
                                                     124,86,87,303,366:368,223,
                                                     224,458,226,179:181,550,636,
                                                     637,163,164,352,138:141,
                                                     345:348,696:699,246:248,
                                                     177,178,803,804,572,573,204,
                                                     205,605,606,228,229,702,
                                                     309,310,343,344,622,623,
                                                     353,354,592,593,559,560,
                                                     624,625,690:693,357,341,342,
                                                     619,620,214,190,424,667,668,
                                                     621,108,463,805,806,807),
                  !stringr::str_detect(name,"Galarian|Partner"))
  )

# sword/shield dex


usethis::use_data(pokedex_min,rbgy_dex,pe_dex,
                  gsc_dex,hgss_dex,rse_dex,
                  oras_dex,dpp_dex,bw_dex,
                  b2w2_dex, xy_dex, sm_dex,
                  usum_dex,
                  overwrite = TRUE, internal=TRUE)
