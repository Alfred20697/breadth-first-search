# Name: Alfred Loo Wee How
# Student ID: 1151100653

## Credits to : Mojtaba Eng (https://www.codeproject.com/Articles/682129/Solving-N-Queen-Problem-by-DFS-and-BFS-and-Show-Go)

InitializeBoard <- function(numberOfQueen) {
  # Initialize the place of queen on a board.
  #
  # Args:
  #   numberOfQueen: The number of queens to be placed on the board.
  #
  # Returns:
  #   newPosition: The place of queen on the board [0, 0, 0, 0].

  # Generate a vector of N zeroes, where N is the number of queen.
  newPosition <- rep(0, numberOfQueen)
  
  return (newPosition)
}

IsGoal <- function(board) {
  # Check if the goal of the board is met.
  #
  # Args:
  #   board: The current board condition.
  #
  # Returns:
  #   boolean value: TRUE, the board count (number of queen placed) is equal to the total number of queen. FALSE, otherwise.

  return (board$N == board$count)
}

IsSafe <- function(column, tempBoard) {
  # Check if the queen is safe to put on the column of the board.
  #
  # Args:
  #   column: the position of queen to be placed.
  #   tempBoard: The current board condition.
  #
  # Returns:
  #   boolean value: TRUE, the position is safe to put the queen. FALSE, otherwise.

  if (tempBoard$count > 0) {
    # Loop and checks if the position have queen before and if the queen attacks each other.
    # Credits to : Mojtaba Eng (https://www.codeproject.com/Articles/682129/Solving-N-Queen-Problem-by-DFS-and-BFS-and-Show-Go)
    for (i in 1:tempBoard$count) {
      if ((tempBoard$queenPosition[i] == column) ||
          (abs(column - tempBoard$queenPosition[i]) == (tempBoard$count - (i - 1)))) {
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

PlaceQueen <- function(column, tempBoard) {
  # Place the queen on the column of the board.
  #
  # Args:
  #   column: the position of queen to be placed.
  #   tempBoard: The current board condition.
  #
  # Returns:
  #   tempBoard: The current board condition, with the placed queen.

  # Check if the column count is between the total number of queen. If yes, place the queen on the column. Add the board count by 1.
  if ((column > 0) & (column <= tempBoard$N)) {
    tempBoard$queenPosition[tempBoard$count + 1] <- column
    tempBoard$count <- tempBoard$count + 1
  } else {
    print("column not in range")
  }
  return (tempBoard)
}

DisplayBoard <- function(queenPosition) {
  # Display the queen in a N x N matrix.
  #
  # Args:
  #   queenPosition: The position of queen on the board. E.g. [2, 4, 1, 3].
  #
  # Returns:
  #   NA

  n <- length(queenPosition)

  # Generate a board matrix with N x N dimension.
  boardMatrix <- matrix(nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      # Check if the queen is at the row, column. E.g. In [2, 4, 1, 3], the first queen is on row 1, column 2 while the second queen is on column 4, and so on.  
      if (j == (queenPosition[i])) {
        boardMatrix[i, j] <- 1
      } else {
        boardMatrix[i, j] <- 0
      }
    }
  }
  print(boardMatrix)
}

CopyBoard <- function(firstBoard) {
  # Copy the board from a board to another board.
  #
  # Args:
  #   firstBoard: The current board condition.
  #
  # Returns:
  #   newBoard: The copied board condition, with every information from the firstBoard.

  newBoard <- list(N = 0, count = 0, queenPosition = c())
  newBoard$N <- firstBoard$N
  newBoard$count <- firstBoard$count
  
  tempQP <- c()
  # Copy every queen position.
  for (i in 1:newBoard$N) {
    tempQP[i] <- firstBoard$queenPosition[i]
  }
  
  newBoard$queenPosition <- tempQP
  
  return (newBoard)
}

SolveBFS <- function(numberOfQueen) {
  # Main function for the program. Calculate the board position and return the result. 
  #
  # Args:
  #   numberOfQueen: The number of queens to be placed on the board.
  #
  # Returns:
  #   NA

  # Initialize an empty queue. 
  queue <- list()
  
  # Initialize a new list.
  newBoard <- list(N = 0, count = 0, queenPosition = c())

  newBoard$N <- numberOfQueen
  newBoard$count <- 0

  # Initialize a new board, returns [0, 0, 0, 0].
  newBoard$queenPosition <- InitializeBoard(numberOfQueen)
  
  # Enqueue the first board.
  queue[length(queue) + 1] <- list(newBoard)
  
  # Print the initial board.
  print("Initial board: ")
  DisplayBoard(queue[[1]]$queenPosition)
  
  # Repeat the search, until the goal is met, or the length of queue is empty.
  repeat {
    # Check if the queue is empty.
    if (length(queue) != 0) {
      # Dequeue the first board.
      firstBoard <- queue[[1]]
      queue[[1]] <- NULL

      # Check if the board meets the goal.
      if (IsGoal(firstBoard)) {
        # Print the result.
        print("Final board: ")
        DisplayBoard(firstBoard$queenPosition)
        break
      } else {
        # Copy the board.
        # Check if the queen is safe to place.
        # If the queen is safe to place, place the queen and enqueue the board. 
        for (i in 1:firstBoard$N) {
          tempBoard <- CopyBoard(firstBoard)
          if (IsSafe(i, tempBoard)) {
            placedBoard <- PlaceQueen(i, tempBoard)
            queue[length(queue) + 1] <- list(placedBoard)
          }
        }
      }
    } else {
      break
    }
  }
}

SolveBFS(8)