#' @title country_replace
#' @description A wrapper function for `cat_replace()` that only requires an inputted
#' vector of messy countries. `country_replace()` uses a built in clean list of
#' country names `country.names` as the reference clean vector.
#' @param messy_countries Vector containing the messy country names that will be replaced
#' by the closest match from `country.names`
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @return `country_replace()` returns a cleaned version of the bad vector, with each
#'  element replaced by the most similar element of the good vector.
#' @details Country names are often misspelled or abbreviated in datasets, especially datasets that have been
#' manually digitized or created. `country_replace()` is a warpper function of `cat_replace()` that quickly solves
#' this common issue of mispellings or different formats of country names across datasets. This wrapper
#' function uses a built in clean list of country names `country.names` as the reference clean vector and
#' replaces your inputted messy vector of names to their nearest match in `country.names`.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  lst <- c("Conagoa", "Blearaus", "Venzesual", "Uruagsya", "England")
#'  fixed <- country_replace(lst)
#'  }
#' }
#' @rdname country_replace
#' @export

country_replace <- function(messy_countries,threshold = NA, p = 0){

  cat_replace(messy_countries,country.names[[1]],method = "jw", threshold, p)
}


# I think we can delete this stuff below ----

# x<- "Afghanistan
# Albania
# Algeria
# Andorra
# Angola
# Antigua and Deps
# Argentina
# Armenia
# Australia
# Austria
# Azerbaijan
# Bahamas
# Bahrain
# Bangladesh
# Barbados
# Belarus
# Belgium
# Belize
# Benin
# Bhutan
# Bolivia
# Bosnia
# Bosnia Herzegovina
# Botswana
# Brazil
# Brunei
# Bulgaria
# Burkina
# Burundi
# Cambodia
# Cameroon
# Canada
# Cape Verde
# Central African Rep
# Chad
# Chile
# China
# Colombia
# Comoros
# Congo
# Democratic Republic of Congo
# Costa Rica
# Croatia
# Cuba
# Cyprus
# Czech Republic
# Denmark
# Djibouti
# Dominica
# Dominican Republic
# East Timor
# Ecuador
# Egypt
# El Salvador
# England
# Equatorial Guinea
# Eritrea
# Estonia
# Ethiopia
# Fiji
# Finland
# France
# Gabon
# Gambia
# Georgia
# Germany
# Ghana
# Greece
# Grenada
# Guatemala
# Guinea
# Guinea-Bissau
# Guyana
# Haiti
# Honduras
# Hungary
# Iceland
# India
# Indonesia
# Iran
# Iraq
# Ireland
# Republic of Ireland
# Israel
# Italy
# Ivory Coast
# Jamaica
# Japan
# Jordan
# Kazakhstan
# Kenya
# Kiribati
# Korea North
# Korea South
# North Korea
# South Korea
# Kosovo
# Kuwait
# Kyrgyzstan
# Laos
# Latvia
# Lebanon
# Lesotho
# Liberia
# Libya
# Liechtenstein
# Lithuania
# Luxembourg
# Macedonia
# Madagascar
# Malawi
# Malaysia
# Maldives
# Mali
# Malta
# Marshall Islands
# Mauritania
# Mauritius
# Mexico
# Micronesia
# Moldova
# Monaco
# Mongolia
# Montenegro
# Morocco
# Mozambique
# Myanmar
# Burma
# Namibia
# Nauru
# Nepal
# Netherlands
# Holland
# New Zealand
# Nicaragua
# Niger
# Nigeria
# Norway
# Oman
# Pakistan
# Palau
# Panama
# Papua New Guinea
# Paraguay
# Peru
# Philippines
# Poland
# Portugal
# Qatar
# Romania
# Russian Federation
# Rwanda
# St Kitts and Nevis
# St Lucia
# Saint Vincent and the Grenadines
# Samoa
# San Marino
# Sao Tome and Principe
# Saudi Arabia
# Senegal
# Serbia
# Seychelles
# Sierra Leone
# Singapore
# Slovakia
# Slovenia
# Solomon Islands
# Somalia
# South Africa
# South Sudan
# Spain
# Sri Lanka
# Sudan
# Suriname
# Swaziland
# Sweden
# Switzerland
# Syria
# Taiwan
# Tajikistan
# Tanzania
# Thailand
# Togo
# Tonga
# Trinidad and Tobago
# Tunisia
# Turkey
# Turkmenistan
# Tuvalu
# Uganda
# Ukraine
# United Arab Emirates
# United Kingdom
# United States
# Uruguay
# Uzbekistan
# Vanuatu
# Vatican City
# Venezuela
# Vietnam
# Yemen
# Zambia
# Zimbabwe"
#
# country.name <- unlist(stringr::str_split(x, "\n"))
