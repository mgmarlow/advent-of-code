Point = Data.define(:x, :y)
Line = Data.define(:p1, :p2)

Rect = Data.define(:p1, :p2) do
  def x1 = [p1.x, p2.x].min
  def x2 = [p1.x, p2.x].max
  def y1 = [p1.y, p2.y].min
  def y2 = [p1.y, p2.y].max

  def area() = (x2 - x1 + 1) * (y2 - y1 + 1)
end

points = File.read("data/day09.txt")
  .split("\n")
  .map { |line| Point.new(*line.split(",").map(&:to_i)) }

# Part one, max area
all_rects = []
points.each do |p1|
  points.each do |p2|
    all_rects << Rect.new(p1, p2)
  end
end
puts all_rects.max_by { |r| r.area }.area

# Part two, bounded at green areas
edges = [*points[2..], points.first]
  .reduce([Line.new(points[0], points[1])]) do |acc, cur|
    acc << Line.new(acc.last.p2, cur)
  end

# TODO: filter rectangles that are fully within the polygon

