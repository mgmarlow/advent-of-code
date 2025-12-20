ranges, ids = File.readlines("data/day05.txt")
  .reject { |line| line.empty? }
  .partition { |line| line.include?("-") }

ranges = ranges.map { |range| range.split("-").map(&:to_i) }
ids = ids.map(&:to_i)

puts ids.sum { |id| ranges.any? { |(lower, upper)| id >= lower && id <= upper } ? 1 : 0 }

# Remove overlapping ranges so we can simply sum the range boundaries to
# figure out # of valid items
ranges = ranges.sort_by { |(lower, upper)| lower }
non_overlapping_ranges = [ranges.first]
ranges[1..].each do |(lower, upper)|
  (prevlower, prevupper) = non_overlapping_ranges.last
  if lower <= prevupper
    merged = [[prevlower, lower].min, [upper, prevupper].max]
    non_overlapping_ranges[-1] = merged
  else
    non_overlapping_ranges << [lower, upper]
  end
end

# +1 because ranges are inclusive
puts non_overlapping_ranges.sum { |(l, u)| u - l + 1 }

