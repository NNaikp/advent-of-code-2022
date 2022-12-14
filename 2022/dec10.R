
source("./2021/readXmas.R")
library(purrr)
library(stringr)

# Retrieve data  ----------------------------------------------------------

pzl_data <- get_pzl_data(10, year = 2022)
pzl_test <- readLines("./2022/dec10_test.txt")


# Solution 1/2 ----------------------------------------------------------

cycle <- strsplit(pzl_data," ") %>% 
  unlist(.) %>% 
  gsub("noop|addx","0",.) %>% 
  as.numeric() %>% 
  cumsum() + 1
  
sol1 <- sum(20*cycle[19],
            60*cycle[59],
            100*cycle[99],
            140*cycle[139],
            180*cycle[179],
            220*cycle[219])

# Solution 2/2 ----------------------------------------------------------

sprite <- c(0,1,2)

cycle2 <- cycle - 1

image_line <- c("#")

j <- 1

grid <- c(1:39,rep(0:39, times = 5))

for (i in grid) {
  if (i %in% (sprite + cycle2[j])) {
    image_line <- paste0(image_line,"#")
  } else {
    image_line <- paste0(image_line,".")
  }
  
  j <- j + 1
}


sol2 <- substring(image_line, c(1,41,81,121,161,201), c(40,80,120,160,200,240)) %>% 
  matrix(., length(.))




