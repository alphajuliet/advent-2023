# util.jl
# Utility functions for Advent of Code

module Util

"""Read input data from a file"""
function read_data(f)
  filter!(x -> !isempty(x), split(read(f, String), "\n"))
end

end # module

# The End