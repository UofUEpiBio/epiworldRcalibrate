




#
# pad_window <- function(window, target_rows = 59) {
#   current_rows <- nrow(window)
#
#   if (current_rows < target_rows) {
#     padding_rows <- target_rows - current_rows
#     padding <- matrix(-1, nrow = padding_rows, ncol = ncol(window))
#
#     if (!is.null(colnames(window))) {
#       colnames(padding) <- colnames(window)
#     }
#
#     window <- rbind(window, padding)
#   }
#
#   return(window)
# }
#
# # 2. generate_all_windows Function
# generate_all_windows <- function(matrix, source, min_window_size, max_window_size, target_rows = 59) {
#   windows <- list()
#
#   rows <- dim(matrix)[3]
#   cols <- dim(matrix)[2]
#
#   for (window_size in min_window_size:max_window_size) {
#     num_windows_for_size <- rows - window_size + 1
#
#     if (num_windows_for_size <= 0) {
#       next
#     }
#
#     third <- floor(num_windows_for_size / 3)
#
#     # First third windows
#     for (i in 1:third) {
#       start_col <- sample(0:(third - 1), 1)
#       window_cols <- (start_col + 1):(start_col + window_size)
#
#       if (max(window_cols) > rows) next
#       window <- t(matrix[, , window_cols])
#
#       window <- pad_window(window, target_rows)
#
#       windows <- append(windows, list(list(window = window, source = source)))
#     }
#
#     # Second third windows
#     for (i in 1:third) {
#       start_col <- sample((rows - window_size - third + 1):(rows - window_size), 1)
#       window_cols <- (start_col + 1):(start_col + window_size)
#
#       if (max(window_cols) > rows || start_col < 0) next
#       window <- t(matrix[, , window_cols])
#
#       window <- pad_window(window, target_rows)
#
#       windows <- append(windows, list(list(window = window, source = source)))
#     }
#
#     # Remaining windows
#     remaining_windows <- num_windows_for_size - 2 * third
#     if (remaining_windows > 0) {
#       for (i in 1:remaining_windows) {
#         start_col <- sample(third:(rows - window_size - third), 1)
#         window_cols <- (start_col + 1):(start_col + window_size)
#
#         if (max(window_cols) > rows || start_col < 0) next
#         window <- t(matrix[, , window_cols])
#
#         window <- pad_window(window, target_rows)
#
#         windows <- append(windows, list(list(window = window, source = source)))
#       }
#     }
#   }
#
#   return(windows)
# }
#
# # 3. process_sliding_windows Function
# process_sliding_windows <- function(matrices, sources, min_window_size, max_window_size, target_rows = 59) {
#   all_windows <- list()
#
#   for (matrix_idx in seq_along(matrices)) {
#     matrix <- matrices[[matrix_idx]]
#     source <- sources[matrix_idx, ]
#
#     windows <- generate_all_windows(matrix, source, min_window_size, max_window_size, target_rows)
#
#     all_windows <- append(all_windows, windows)
#   }
#
#   return(all_windows)
# }
# split_pad=process_sliding_windows(matrices=matrices,sources=sources,
#                                   min_window_size=15,max_window_size = 59,target_rows = 59)
# saveRDS(split_pad,
#         file = "RNN-MODELS/split_pad.rds",
#         compress = TRUE
# )
#
# #spiliting the dataset
# #
# #
# # process_sliding_windows <- function(matrices, sources, min_window_size, max_window_size) {
# #   all_windows <- list()
# # matrix_idx=1
# #   for (matrix_idx in seq_along(matrices)) {
# #     matrix <- matrices[[matrix_idx]]
# #
# #     rows <- dim(matrix)[3]
# #     cols <- dim(matrix)[2]
# #     source <- sources[matrix_idx,]
# #
# #     for (window_size in min_window_size:max_window_size) {
# #       num_windows_for_size <- rows - window_size + 1
# #
# #       third <- floor(num_windows_for_size / 3) # Use floor to handle integer division
# #
# #       for (i in 1:third) {
# #         start_col <- sample(0:(third - 1), 1) # R's sample handles random integers differently
# #         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
# #         # R indexing starts at 1
# #         all_windows <- append(all_windows, list(list(window = window, source = source))) # Nested lists for (window, source) tuple
# #       }
# #
# #       for (i in 1:third) {
# #         start_col <- sample((rows - window_size - third):(rows - window_size), 1)
# #         window <- t(matrix[, ,(start_col + 1):(start_col + window_size)])
# #         all_windows <- append(all_windows, list(list(window = window, source = source)))
# #       }
# #
# #       for (i in 1:(num_windows_for_size - 2 * third)) {
# #         start_col <- sample(third:(rows - window_size - third), 1)
# #         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
# #         all_windows <- append(all_windows, list(list(window = window, source = source)))
# #       }
# #     }
# #   }
# #
# #   return(all_windows)
# # }
# # process_sliding_windows(matrices=arrays_1d,sources,min_window_size=15,max_window_size = 59)
#
#
#
# process_sliding_windows_parallel <- function(matrices, sources, min_window_size, max_window_size, ncores) {
#   # Run the same code used in process_sliding_windows for each matrix, but in parallel.
#   all_windows_list <- parallel::mclapply(seq_along(matrices), function(matrix_idx) {
#     matrix <- matrices[[matrix_idx]]
#
#     rows <- dim(matrix)[3]
#     cols <- dim(matrix)[2]
#     source <- sources[matrix_idx,]
#
#     local_windows <- list()
#     for (window_size in min_window_size:max_window_size) {
#       num_windows_for_size <- rows - window_size + 1
#       third <- floor(num_windows_for_size / 3)  # Same logic: integer division
#
#       # 1) First set of windows
#       for (i in 1:third) {
#         start_col <- sample(0:(third - 1), 1)
#         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
#         local_windows <- append(local_windows, list(list(window = window, source = source)))
#       }
#
#       # 2) Second set of windows
#       for (i in 1:third) {
#         start_col <- sample((rows - window_size - third):(rows - window_size), 1)
#         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
#         local_windows <- append(local_windows, list(list(window = window, source = source)))
#       }
#
#       # 3) Remaining windows
#       for (i in 1:(num_windows_for_size - 2 * third)) {
#         start_col <- sample(third:(rows - window_size - third), 1)
#         window <- (matrix[,, (start_col + 1):(start_col + window_size)])
#         local_windows <- append(local_windows, list(list(window = window, source = source)))
#       }
#     }
#
#     return(local_windows)
#   }, mc.cores = ncores)
#
#   # Combine (flatten) all lists of windows into a single list
#   all_windows <- do.call(c, all_windows_list)
#   return(all_windows)
# }
#
# all_windows=process_sliding_windows_parallel(matrices, sources, min_window_size, max_window_size, ncores)
#
# # 1. Define the padarray() function in your script
# padarray <- function(x, pad_dims, pad_value = 0, pad_direction = "post") {
#   old_dims <- dim(x)
#   new_dims <- old_dims + pad_dims
#   new_matrix <- matrix(pad_value, nrow = new_dims[1], ncol = new_dims[2])
#   if (pad_direction == "post") {
#     new_matrix[1:old_dims[1], 1:old_dims[2]] <- x
#   } else {
#     stop("Only 'post' padding is implemented.")
#   }
#   return(new_matrix)
# }
#
# # 2. Define or load your pad_windows() function exactly as before:
# pad_windows <- function(all_windows, target_cols = 59) {
#   padded_windows <- list()
#   sources <- list()
#
#   for (i in seq_along(all_windows)) {
#     window <- all_windows[[i]][[1]]
#     source <- all_windows[[i]][[2]]
#
#     window <- as.matrix(window)
#     rows <- nrow(window)
#     cols <- ncol(window)
#
#     padding_size <- target_cols - cols
#
#     if (padding_size > 0) {
#       padded_window <- padarray(window, c(0, padding_size), -1, "post")
#     } else {
#       padded_window <- window
#     }
#
#     padded_windows[[i]] <- padded_window
#     sources[[i]] <- source
#   }
#
#   padded_windows <- array(
#     unlist(padded_windows),
#     dim = c(
#       length(padded_windows),
#       nrow(padded_windows[[1]]),
#       ncol(padded_windows[[1]])
#     )
#   )
#
#   sources <- unlist(sources)
#
#   return(list(padded_windows = padded_windows, sources = sources))
# }
#
# # # 3. Now call pad_windows() without error:
# # # my_result <- pad_windows(my_all_windows)
# #
# # padded_winodws=pad_windows(all_windows, target_cols = 59)
# #
# #
# #
# # 1. pad_window Function
# pad_window <- function(window, target_rows = 59) {
#   current_rows <- nrow(window)
#
#   if (current_rows < target_rows) {
#     padding_rows <- target_rows - current_rows
#     padding <- matrix(-1, nrow = padding_rows, ncol = ncol(window))
#
#     if (!is.null(colnames(window))) {
#       colnames(padding) <- colnames(window)
#     }
#
#     window <- rbind(window, padding)
#   }
#
#   return(window)
# }
#
# # 2. generate_all_windows Function
# generate_all_windows <- function(matrix, source, min_window_size, max_window_size, target_rows = 59) {
#   windows <- list()
#
#   rows <- dim(matrix)[3]
#   cols <- dim(matrix)[2]
#
#   for (window_size in min_window_size:max_window_size) {
#     num_windows_for_size <- rows - window_size + 1
#
#     if (num_windows_for_size <= 0) {
#       next
#     }
#
#     third <- floor(num_windows_for_size / 3)
#
#     # First third
#     for (i in 1:third) {
#       start_col <- sample(0:(third - 1), 1)
#       window_cols <- (start_col + 1):(start_col + window_size)
#       if (max(window_cols) > rows) next
#
#       window <- t(matrix[, , window_cols])
#       window <- pad_window(window, target_rows)
#       windows <- append(windows, list(list(window = window, source = source)))
#     }
#
#     # Second third
#     for (i in 1:third) {
#       start_col <- sample((rows - window_size - third + 1):(rows - window_size), 1)
#       window_cols <- (start_col + 1):(start_col + window_size)
#       if (max(window_cols) > rows || start_col < 0) next
#
#       window <- t(matrix[, , window_cols])
#       window <- pad_window(window, target_rows)
#       windows <- append(windows, list(list(window = window, source = source)))
#     }
#
#     # Remaining windows
#     remaining_windows <- num_windows_for_size - 2 * third
#     if (remaining_windows > 0) {
#       for (i in 1:remaining_windows) {
#         start_col <- sample(third:(rows - window_size - third), 1)
#         window_cols <- (start_col + 1):(start_col + window_size)
#         if (max(window_cols) > rows || start_col < 0) next
#
#         window <- t(matrix[, , window_cols])
#         window <- pad_window(window, target_rows)
#         windows <- append(windows, list(list(window = window, source = source)))
#       }
#     }
#   }
#
#   return(windows)
# }
#
# # 3. process_sliding_windows_mclapply Function (Parallel)
# process_sliding_windows_mclapply <- function(matrices,
#                                              sources,
#                                              min_window_size,
#                                              max_window_size,
#                                              target_rows = 59,
#                                              ncores = 1) {
#   all_windows_list <- mclapply(seq_along(matrices), function(matrix_idx) {
#     matrix <- matrices[[matrix_idx]]
#     source <- sources[matrix_idx, ]
#
#     # Generate windows for this matrix
#     generate_all_windows(matrix, source, min_window_size, max_window_size, target_rows)
#   }, mc.cores = ncores)
#
#   # Flatten the resulting list
#   all_windows <- do.call(c, all_windows_list)
#   return(all_windows)
# }
#
# # 4. Actually run the parallel sliding windows and save
# split_pad <- process_sliding_windows_mclapply(
#   matrices         = matrices,
#   sources          = sources,
#   min_window_size  = 15,
#   max_window_size  = 59,
#   target_rows      = 59,
#   ncores           = 20  # choose cores
# )
#
# # Option 1: Modify 'split_pad' *after* it's created
# for (i in seq_along(split_pad)) {
#   # Keep only the first column (as a column matrix)
#   split_pad[[i]]$window <- split_pad[[i]]$window[, 1, drop = FALSE]
# }

#
#
