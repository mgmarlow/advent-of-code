class Grid
  attr_reader :start

  def initialize(source)
    @data = source.split("\n").map { |line| line.split("") }
    @start = [0, @data.first.index("S")]
  end

  def within_bounds?(row, col) =
    row >= 0 && row < rows && col >= 0 && col <= cols

  def get(row, col)
    return unless within_bounds?(row, col)

    @data[row][col]
  end

  def splitter?(row, col)
    get(row, col) == "^"
  end

  def rows
    @data.length
  end

  # Assuming equal-length rows
  def cols
    @data.first.length
  end
end

grid = Grid.new(File.read("data/day07.txt"))
_, start_index = grid.start

# Part one: number of splits
streams = [start_index] # : Array[Integer]
splits = 0
(1..grid.rows-1).each do |ri|
  streams = streams
    .flat_map
    .with_index do |ci, idx|
      if grid.splitter?(ri, ci)
        splits += 1
        [ci-1, ci+1]
      else
        ci
      end
    end
    .uniq
end
puts splits

# Part two: number of distinct timelines
# Interestingly this ends up just being a slight modification to
# the previous algorithm. We track all walked positions as possible
# options as the streams are traversed.
stream_counts = Hash.new(0)
stream_counts[start_index] = 1
(1..grid.rows-1).each do |ri|
  new_stream_counts = Hash.new(0)
  stream_counts.each do |ci, count|
    if grid.splitter?(ri, ci)
      new_stream_counts[ci-1] += count
      new_stream_counts[ci+1] += count
    else
      # We care about these paths now since they're one possible
      # timeline outcome
      new_stream_counts[ci] += count
    end
  end
  stream_counts = new_stream_counts
end
puts stream_counts.values.sum

