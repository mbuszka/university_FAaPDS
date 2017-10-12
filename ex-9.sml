fun suffixes [] 	          = [[]]
  | suffixes (all as x::xs) = all :: suffixes xs
