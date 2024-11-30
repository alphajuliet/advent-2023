# day01.jl

using Chain, Test

"""Read input data from a file"""
function read_data(f)
  data = open(f) do io
    read(io, String)
  end
  split(data, "\n")
end

"""Find the aggregate of all digits in a string"""
function find_numbers(str)
  digits = [m.match for m in eachmatch(r"\d", str)]
  parse(Int, first(digits) * last(digits))
end

function part1(f)
  lines = read_data(f)
  @chain lines begin
    filter(x -> length(x)>=1, _)
    map(find_numbers, _)
    sum
  end
end

@test part1("data/day01-test.txt") == 142

# The End