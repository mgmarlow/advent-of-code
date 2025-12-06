content = File.read("data/day06.txt")
  .split("\n")
  .map(&:split)

nums = content[0..-2] # : Array[Array[String]]
  .map { |row| row.map(&:to_i) }
operations = content[-1]

result = operations.each_with_index.sum do |op, i|
  nums.map { |row| row[i] }.reduce(op.to_sym)
end
puts result

# Need to re-read the input data since splitting out the whitespace
# doesn't work for part 2 (and the items aren't consistently
# left/right aligned).
content = File.read("data/day06.txt")
  .split("\n")

str_nums = content[0..-2] # : Array[String]
operator_line = content.last

sum = 0
nums = [] # : Array[Integer]
(operator_line.length-1).downto(0) do |i|
  if /[*+]/.match(operator_line[i+1])
    # Empty column, skip.
    next
  end

  nums << str_nums
    .reduce("") { |acc, cur| cur[i] ? acc + cur[i] : acc }.to_i

  if /[*+]/.match(operator_line[i])
    sum += nums.reduce(operator_line[i].to_sym)
    nums.clear
  end
end
puts sum

