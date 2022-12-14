
source("./2021/readXmas.R")

# Retrieve data  ----------------------------------------------------------

pzl_data <- get_pzl_data(14, year = 2022)

# Solution 1/2 ----------------------------------------------------------

rock <- function(x) {
  points <- strsplit(x," -> ")[[1]] %>% 
    lapply(strsplit,",") %>%
    lapply(`[[`,1) %>% 
    lapply(as.numeric)
}

cave <- matrix(rep(".", times = 300*200), ncol = 200)

draw_rocks <- function(cave,rocks) {
  
  for (i in rocks) {
    thisrock <- rock(i)
    
    for (j in 1:(length(thisrock) - 1)) {
      # browser()
      # Reverse x,y for matrix indices
      starty <- thisrock[[j]][[1]]
      startx <- thisrock[[j]][[2]]
      stopy <- thisrock[[j + 1]][[1]]
      stopx <- thisrock[[j + 1]][[2]]
      
      # Create walls (Use -400 to center in matrix columns)
      cave[startx:stopx,(starty:stopy - 400)] <- "#"
    }

  }
  
  return(cave)
}

cave_map <- draw_rocks(cave,pzl_data)


sand_simulator <- function(cave_map, sand_start) {
  
  # Initialise
  sand_start <- c(1,100)
  old_cave <- cave_map
  cave_map[sand_start[1],sand_start[2]] <- "+"
  dims <- dim(cave_map)
  dl <- c(1,-1)
  dr <- c(1,1)

  # browser()
  for (i in 1:5000) {
    
    sand <- sand_start
    print(i)
    
    while (any(cave_map != old_cave)) {
      # browser()
      while ((!cave_map[sand[1] + 1,sand[2]] %in% c("#","o") | 
             !cave_map[sand[1] + 1, sand[2] - 1]  %in% c("#","o") |
             !cave_map[sand[1] + 1, sand[2] + 1]  %in% c("#","o")) &
             !sand[1] > dims[1] &
             !sand[2] > dims[2] &
             !all(sand == (sand_start + c(1,0)))) {
        

        print("*.Moving sand.* |--D")
        cave_col <- cave_map[,sand[2]]
        
        first_block <- which(cave_col[sand[1]:length(cave_col)] %in% c("#","o"))[1]
        if (is.na(first_block)) {
          print(c(i, "Sand is falling off!"))
          return(cave_map)
        }
        
        # If sand has already falling on top of other sand and we are trying to move it
        # diagonally the first block will always be at place 2 since we are standing in place 1
        if (first_block != 2) {
          # Move sand to first block
          if (sand[1] == 1) {
            sand[1] <- first_block - 1
          } else {
            sand[1] <- sand[1] + first_block - 2
          }
        } 
        
        # Check if sand can travel down left/down right
        if (!cave_map[sand[1] + 1, sand[2] - 1]  %in% c("#","o")) {
          sand <- sand + dl
        } else if (!cave_map[sand[1] + 1, sand[2] + 1]  %in% c("#","o")) {
          sand <- sand + dr
        }
      
      }
      
      old_cave <- cave_map
      cave_map[sand[1],sand[2]] <- "o"
      break
    }
  
    if (all(old_cave == cave_map)) {
      print("The cave is full!")
      break
    }
  }

  return(cave_map)
}


cave_sand <- sand_simulator(cave_map, sand_start)

sol1 <- sum(cave_sand == "o")

# Solution 2/2 ----------------------------------------------------------

# Make cave bigger
cave2 <- matrix(rep(".", times = 200*1000), ncol = 1000)

draw_rocks2 <- function(cave,rocks) {
  
  for (i in rocks) {
    thisrock <- rock(i)
    
    for (j in 1:(length(thisrock) - 1)) {
      # browser()
      # Reverse x,y for matrix indices
      starty <- thisrock[[j]][[1]]
      startx <- thisrock[[j]][[2]] + 1
      stopy <- thisrock[[j + 1]][[1]]
      stopx <- thisrock[[j + 1]][[2]] + 1
      
      cave[startx:stopx,(starty:stopy)] <- "#"
    }
    
  }
  
  return(cave)
}

cave_map2 <- draw_rocks2(cave2,pzl_data)

# Create floor
floor_depth <- apply(cave_map2,1,function(x) any(x == "#")) %>% 
  max(which(.))

cave_map2[floor_depth + 2,] <- "#"


sand_simulator2 <- function(cave_map, sand_start = c(1,500)) {
  
  # Initialise
  old_cave <- cave_map
  cave_map[sand_start[1],sand_start[2]] <- "+"
  dims <- dim(cave_map)
  dl <- c(1,-1)
  dr <- c(1,1)
  
  # browser()
  for (i in 1:100000) {
    
    sand <- sand_start
    # print(i)
    
    while (any(cave_map != old_cave)) {
      # browser()
      s <- 1
      while ((!cave_map[sand[1] + 1,sand[2]] %in% c("#","o") | 
              !cave_map[sand[1] + 1, sand[2] - 1]  %in% c("#","o") |
              !cave_map[sand[1] + 1, sand[2] + 1]  %in% c("#","o")) &
             !sand[1] > dims[1] &
             !sand[2] > dims[2] & 
             !(all(sand != sand_start) & any((sand - 1) == 0))) {
        
        s <- s + 1
        
        # if (i == 9802 & s == 99) {
        #   browser()
        # }
        # print(s)
        cave_col <- cave_map[,sand[2]]
        
        first_block <- which(cave_col[sand[1]:length(cave_col)] %in% c("#","o"))[1]
        if (is.na(first_block)) {
          # print(c(i, "Sand is falling off!"))
          return(cave_map)
        }
        
        # If sand has already falling on top of other sand and we are trying to move it
        # diagonally the first block will always be at place 2 since we are standing in place 1
        if (first_block != 2) {
          # Move sand to first block
          if (sand[1] == 1) {
            sand[1] <- first_block - 1
          } else {
            sand[1] <- sand[1] + first_block - 2
          }
        } 
        
        # Check if sand can travel down left/down right
        if (!cave_map[sand[1] + 1, sand[2] - 1]  %in% c("#","o")) {
          sand <- sand + dl
        } else if (!cave_map[sand[1] + 1, sand[2] + 1]  %in% c("#","o")) {
          sand <- sand + dr
        }
        
      }
      
      old_cave <- cave_map
      cave_map[sand[1],sand[2]] <- "o"
      break
    }
    
    if (all(old_cave == cave_map)) {
      print("The cave is full!")
      break
    }
  }
  
  return(cave_map)
}


cave_sand2 <- sand_simulator2(cave_map2)


sol2 <- sum(cave_sand2 == "o")

#Export matrix to look at it
write.table(cave_sand2, 
            file = "./2022/dec14cave.txt", 
            row.names = FALSE,
            col.names = FALSE)



