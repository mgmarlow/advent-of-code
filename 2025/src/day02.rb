content = File.read("data/day02.txt")
  .strip
  .split(",")
  .flat_map do |line|
    start, last = line.split("-")
    (start.to_i..last.to_i).to_a
  end

def part_one_id_valid?(id)
  v = id.to_s
  return true if v.length.odd?

  v[0...v.length/2] != v[v.length/2..]
end

# Couple of tricks:
# 
# Repeating sequences are divisible by subcomponents, e.g. 1188511885 / 1, / 11, / 118...
# 
# Or, double the string and trim the edges. An invalid (repeating)
# string will be contained in the result.
def part_two_id_valid?(id) = !(id.to_s * 2)[1..-2].include?(id.to_s)

puts content.reduce(0) { |acc, cur| part_one_id_valid?(cur) ? acc : acc + cur }

puts content.reduce(0) { |acc, cur| part_two_id_valid?(cur) ? acc : acc + cur }
