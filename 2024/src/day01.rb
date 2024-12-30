class DayOne
  def self.run
    solver = new(File.read("2024/data/day01.txt"))
    [solver.part_one, solver.part_two]
  end

  def initialize(contents)
    @contents = contents
  end

  def columns
    @columns ||= begin
      lleft = []
      lright = []

      @contents.split("\n").each do |line|
        left, right = line.strip.split("   ")
        lleft << left.to_i
        lright << right.to_i
      end

      [lleft, lright]
    end
  end

  def part_one
    columns[0].sort.zip(columns[1].sort).inject(0) { |r, v| r + (v[0] - v[1]).abs }
  end

  def part_two
    frequency = columns[1].inject({}) { |r, v| r[v] ||= 0; r[v] += 1; r }
    columns[0].inject(0) { |r, v| r + (v * frequency.fetch(v, 0)) }
  end
end
