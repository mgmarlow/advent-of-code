$content = File.read("data/day04.txt")
  .split("\n")
  .map { |row| row.split("") }

ADJACENTS = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

# Necessary since row[-1] will wrap around to the end of the list.
def within_bounds?(row, col) =
  row >= 0 && row < $content.length && col >= 0 && col < $content.first.length

def roll?(row, col) = within_bounds?(row, col) && $content.dig(row, col) == "@"

def accessible?(row, col) =
  ADJACENTS.sum { |(dr, dc)| roll?(row + dr, col + dc) ? 1 : 0 } < 4

sum = 0
$content.each_with_index do |row, ri|
  row.each_with_index do |col, ci|
    if col == "@" && accessible?(ri, ci)
      sum += 1
    end
  end
end
puts sum

sum = 0
removed_paper = true
while removed_paper do
  removed_paper = false
  $content.each_with_index do |row, ri|
    row.each_with_index do |col, ci|
      if col == "@" && accessible?(ri, ci)
        sum += 1
        $content[ri][ci] = "."
        removed_paper = true
      end
    end
  end
end
puts sum
