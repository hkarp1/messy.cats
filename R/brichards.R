library(easyr)
library(gtools)
library(messy.cats)
library(misty)
library(tidyr)
library(consort)

messy_short = c("Fiat 128", "red Honda Civic", "Toyota Corolla",
                "Toyota Corona", "Dodge Challenger", "red AMC Javelin", "Camaro Z28",
                "Pontiac Firebird", "black Fiat X1-9", "blue Porsche 914-2", "Lotus
Europa", "Ford Pantera L", "black Ferrari Dino", "black Maserati Bora",
                "black Volvo 142E")
clean_short = c("Honda Civic", "Toyota Corona", "AMC Javelin", "Pontiac
Firebird", "Fiat X1-9", "Porsche 914-2", "Lotus Europa", "Ford Pantera
L", "Ferrari Dino", "Maserati Bora", "Volvo 142E")
car_mtch = cat_match(messy_short, clean_short, method = "jaccard",
                 return_lists = 3, threshold = 0.2)

# split returned lists ----
list_split <- function(mtch){
  # find length of lists in match column of cat_match output
  list_ln = numeric(length = nrow(mtch))
  for (i in 1:nrow(mtch)) {
    list_ln[i] = length(mtch[[i,2]])
  }
  # find the longest list of matches in match column
  ln = list_ln[which.max(list_ln)]
  # manipulate match lists to clean strings
    # parse the lists removing opening parentheses
  mtch$rem = ifelse(left(mtch$match, 2) == "c(", mid(mtch$match, 3,
                                                     nchar(mtch$match)), mtch$match)
    # parse the lists removing closing parentheses
  mtch$rem = ifelse(right(mtch$rem, 1) == ")", left(mtch$rem,
                                                    nchar(mtch$rem)-1), mtch$rem)
    # parse the lists removing chr(34) which == \"
  mtch$rem = gsub(pattern = chr(34), replacement = "", x = mtch$rem)
    # above line leaves in \n characters which == chr(10)

  # do the same string manipulations as above but for the distances
    # parse the distances to remove opening parentheses
  mtch$distprs = ifelse(left(mtch$dists, 2) == "c(", mid(mtch$dists, 3,
                                                         nchar(mtch$dists)), mtch$dists)
    # parse the distances to remove closing parentheses
  mtch$distprs = ifelse(right(mtch$distprs , 1) == ")", left(mtch$distprs
                                                             , nchar(mtch$distprs )-1), mtch$distprs)
  # separate the cleaned matches into their own columns
  col_suffix = 1:ln
  # create ln mtch columns
  mtch1 = mtch %>% separate(col = rem, into = sprintf("mtch%s",col_suffix), sep = ",")
  # remove whitespace from mtch1:mtchln
  mtch1 = mtch1 %>% mutate(across(matches("mtch"), ~trimws(.x,which = "both")))
  # create ln dist columns
  mtch2 = mtch1 %>% separate(col = distprs, into = sprintf("dist%s",col_suffix), sep = ",")
  # remove whitespace from dist columns
  mtch3 = mtch2 %>% mutate(across(matches("mtch"), ~trimws(.x,which = "both")))
  struct_mtch = mtch3 %>% select(-c(match,dists)) %>% df.sort(bad)
}

test_match = cat_match(messy_caterpillars$CaterpillarSpecies,clean_caterpillars$species,
          return_lists = 5, threshold = 0.05)

test = list_split(test_match)


# plots ----

txt1 = paste0("TARGET: ", struct_mtch[8,1])
txt2 = paste(c("GUESS:", "DIST:"))
u = "\u2022"
brk = "\n"
sp = chr(32)
txt3 = paste0(u, sp, struct_mtch[8,2], brk, u, sp, struct_mtch[8,3],
              brk, u, sp, struct_mtch[8,4])
txt4 = paste0(u, sp, struct_mtch[8,5], brk, u, sp, struct_mtch[8,6],
              brk, u, sp, struct_mtch[8,7])
dgram = add_box(txt = txt1) %>%
  add_split(txt = txt2) %>%
  add_side_box(txt = c(txt3, txt4))
plot(dgram)
