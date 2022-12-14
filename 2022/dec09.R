
source("./2021/readXmas.R")
library(purrr)

# Retrieve data  ----------------------------------------------------------

pzl_data <- get_pzl_data(9, year = 2022)

# Solution 1/2 ----------------------------------------------------------

head(pzl_data)

# Function to check whether head is in the vicinity of tail
in_tail_vic <- function(head, tail) {
  
tail_vicinity <- list(c(-1,0), c(-1,1), c(0,1), c(1,1), c(1,0), c(1,-1), c(0,-1), c(-1,-1))
  
tail_square <- lapply(tail_vicinity, function(x) x + tail)
tail_square[9] <- list(tail)

check_head <- lapply(tail_square, function(x) all(x == head)) %>% 
  unlist(.) %>% 
  any()

return(check_head)
  
}

# Function to create trial for tail snake for longer moves
trail <- function(x,n) {
  split(x, cut(seq_along(x), n, labels = FALSE)) %>% 
    sapply(paste, collapse = ",") %>% 
    unname()
  }

#x,y : row,col

headxy <- c(0,0)
tailxy <- c(0,0)

tail_snake <- paste(tailxy, collapse = ",") #paste to 1 input


for (i in pzl_data) {
  
  input <- strsplit(i," ")[[1]]
  
  direction <- input[1]
  length <- input[2] %>% as.numeric()
  
  if (grepl("R",i)) {
    pos <- 2
    val <- length
  }
  if (grepl("L",i)) {
    pos <- 2
    val <- -length
  }
  if (grepl("U",i)) {
    pos <- 1
    val <- -length
  }
  if (grepl("D",i)) {
    pos <- 1
    val <- length
  }
  
  # move head
  headxy[pos] <- headxy[pos] + val

  # Check if head is still in tail vicinity
  tail_check <- in_tail_vic(headxy,tailxy)
  
  if (tail_check) {
    tailxy <- tailxy
    tail_snake <- c(tail_snake,paste(tailxy, collapse = ","))
  } else {
    
    head_dist <- headxy - tailxy
    
    # Check if we need to move diagonally
    if (headxy[1] != tailxy[1] & headxy[2] != tailxy[2]) {
      
      if (all(head_dist < 0)) {
        move_diag <- c(-1,-1)
      } else if (all(head_dist > 0)) {
        move_diag <- c(1,1)
      } else if (head_dist[1] < 0 & head_dist[2] > 0) {
        move_diag <- c(-1,1)
      } else {
        move_diag <- c(1,-1)
      }
      
      tailxy <- tailxy + move_diag
      tail_snake <- c(tail_snake, paste(tailxy, collapse = ","))
    
      
      if (in_tail_vic(headxy,tailxy)) {
        print(c(paste0("MOVE: ",i),paste0("HEAD: ",paste(headxy,collapse = ",")), paste0("TAIL: ", paste(tailxy,collapse = ","))))
        next
      }
    } 
    
    # Otherwise move tail in direction of head
    head_dist <- headxy - tailxy
    
    change <- head_dist[which(head_dist != 0)]
    
    move <- ifelse(change < 0, change + 1, change - 1) 

    # Save old position for tail snake
    old_pos <- tailxy
    
    # Update tail position
    tailxy[pos] <- tailxy[pos] + move
    
    # Add trail to tail_snake
    xy <- c(1,2)
    coords <- list(tailxy[-pos],old_pos[pos]:tailxy[pos])[c(xy[-pos],xy[pos])]

    tail_trail <- c(rbind(coords[[1]],coords[[2]]))
    tail_snake <- c(tail_snake, trail(tail_trail, length(tail_trail)/2))
    
  }
   
  # See all moves 
  print(c(paste0("MOVE: ",i),paste0("HEAD: ",paste(headxy,collapse = ",")), paste0("TAIL: ", paste(tailxy,collapse = ","))))
}
  
sol1 <- length(unique(tail_snake))


# Solution 2/2 ----------------------------------------------------------

rope_path <- function(x) {
  
  rope <- rep(list(c(0,0)),10)
  
  tail_snake <- paste(rope[[10]], collapse = ",")

  m <- 0
  for (i in x) {
    m <- m + 1
    # print(m)
    # if (m == 158) {
    #   browser()
    # }
    
    input <- strsplit(i," ")[[1]]
    
    direction <- input[1]
    length <- input[2] %>% as.numeric()
    
    if (grepl("R",i)) {
      pos <- 2
      val <- length
    }
    if (grepl("L",i)) {
      pos <- 2
      val <- -length
    }
    if (grepl("U",i)) {
      pos <- 1
      val <- -length
    }
    if (grepl("D",i)) {
      pos <- 1
      val <- length
    }
    
    # browser()
    for (j in 1:length(rope)) {
      if (j == 1) {
        # move head
        rope[[j]][[pos]] <- rope[[j]][[pos]] + val
      } else {
        
        # Check if head is still in tail vicinity
        tail_check <- in_tail_vic(rope[[j - 1]],rope[[j]])
      
      if (tail_check) {
        next
      } else {
        
          
          # Check if we need to move diagonally
          while (!any(rope[[j - 1]] - rope[[j]] == 0) & !in_tail_vic(rope[[j - 1]],rope[[j]])) {
            
            head_dist <- rope[[j - 1]] - rope[[j]]
            
            if (all(head_dist < 0)) {
              move_diag <- c(-1,-1)
            } else if (all(head_dist > 0)) {
              move_diag <- c(1,1)
            } else if (head_dist[1] < 0 & head_dist[2] > 0) {
              move_diag <- c(-1,1)
            } else {
              move_diag <- c(1,-1)
            }
            
            rope[[j]] <- rope[[j]] + move_diag
            
            if (j == length(rope)) {
              tail_snake <- c(tail_snake, paste(rope[[j]], collapse = ","))
            }
            
          } 
          
          if (in_tail_vic(rope[[j - 1]],rope[[j]])) {
            next
          }
          
          # Otherwise move tail in direction of head
          head_dist <- rope[[j - 1]] - rope[[j]]
          
          change <- head_dist[which(head_dist != 0)]
          
          move <- ifelse(change < 0, change + 1, change - 1) 
          
          # Save old position for tail snake
          old_pos <- rope[[j]]
          
          # Update tail position
          rope[[j]][[pos]] <- rope[[j]][[pos]] + move
          
          # Add trail to tail_snake
          if (j == length(rope)) {
            xy <- c(1,2)
            coords <- list(rope[[j]][-pos],old_pos[pos]:rope[[j]][pos])[c(xy[-pos],xy[pos])]
            
            tail_trail <- c(rbind(coords[[1]],coords[[2]]))
            tail_snake <- c(tail_snake, trail(tail_trail, length(tail_trail)/2))
          }
        
        }
      
      }
    }
    print(paste0("TAIL LENGTH: ",length(unique(tail_snake))))
  }
  
  
  return(tail_snake)
}

res <- rope_path(pzl_data)
ures <- unique(res)

sol2 <- length(unique(res))


values <- strsplit(res,",")

x <- sapply(values,`[[`,1) %>% as.numeric()
y <- sapply(values,`[[`,2) %>% as.numeric()

plot(x = x,
     y = y,
     type = "S")






