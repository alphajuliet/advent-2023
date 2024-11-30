# util.jl
# Utility functions for Advent of Code

module Util

"""Read input data from a file"""
function read_data(f)
  data = open(f) do io
    read(io, String)
  end
  split(data, "\n")
end

end # module

# The End