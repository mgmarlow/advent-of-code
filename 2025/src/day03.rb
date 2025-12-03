content = File.read("data/day03.txt")
  .split("\n")
  .map { |row| row.split("").map(&:to_i) }

# Original solution for part 1
def largest_joltage_two(row)
  lmax = row[0..-2].max
  lidx = row.index(lmax)
  rmax = row[lidx+1..].max
  (lmax.to_s + rmax.to_s).to_i
end

# Generic solution for N-lengthed joltage
def largest_joltage_n(row, size)
  stack = []
  remaining_dels = row.length - size

  row.each do |d|
    while stack.last && d > stack.last && remaining_dels > 0
      stack.pop
      remaining_dels -= 1
    end
    stack << d
  end

  if remaining_dels > 0
    stack[0...-remaining_dels].join.to_i
  else
    stack.join.to_i
  end
end

puts content.reduce(0) { |acc, row| acc + largest_joltage_n(row, 2) }
puts content.reduce(0) { |acc, row| acc + largest_joltage_n(row, 12) }

