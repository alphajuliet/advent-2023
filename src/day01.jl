# day01.jl

module Day01

using Chain, Test, Main.Util

"""Find the aggregate of all digits in a string"""
function find_numbers(str)
  digits = [m.match for m in eachmatch(r"\d", str)]
  parse(Int, first(digits) * last(digits))
end

function convert_match(str)
  m = Dict("one" => "1", "two" => "2", "three" => "3", "four" => "4", "five" => "5",
           "six" => "6", "seven" => "7", "eight" => "8", "nine" => "9")
  if str ∈ keys(m)
    return m[str]
  else
    return str
  end
end

"""Find numbers as digits or words"""
function find_numbers2(str)
  digits = r"one|two|three|four|five|six|seven|eight|nine|\d"
  numbers = [m.match |> convert_match for m in eachmatch(digits, str, overlap = true)]
  parse(Int, first(numbers) * last(numbers))
end

function part1(f)
  lines = Util.read_data(f)
  @chain lines begin
    filter(x -> length(x)>=1, _)
    map(find_numbers, _)
    sum
  end
end

function part2(f)
  lines = Util.read_data(f)
  @chain lines begin
    filter(x -> length(x)>=1, _)
    map(find_numbers2, _)
    sum
  end
end

@test part1("data/day01-test.txt") == 142
@test part2("data/day01-test.txt") == 142
@test part2("data/day01-test2.txt") == 281

end # module

# The End