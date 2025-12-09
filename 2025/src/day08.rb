Point = Data.define(:x, :y, :z) do
  def distance(other)
    Math.sqrt((x - other.x)**2 + (y - other.y)**2 + (z - other.z)**2)
  end
end

points = File.read("data/day08.txt")
  .split("\n")
  .map { |line| Point.new(*line.split(",").map(&:to_i)) }

class DSU
  def initialize(n)
    @parent = (0...n).to_a
  end

  def find(x) =
    x == @parent[x] ? x : find(@parent[x])

  def union(a, b) =
    @parent[find(a)] = find(b)

  def all_connected?() =
    (0...@parent.size).map { |i| find(i) }.uniq.size == 1
end

# Calculate all of the distances ahead-of-time
edges = []
points.each_with_index do |p1, i|
  (i+1..points.length-1).each do |j|
    p2 = points[j]
    edges << [p1.distance(p2), i, j]
  end
end
edges.sort_by! { |dist| dist }

# Fixed # of edges (part 1, n=1000)
dsu = DSU.new(points.length)
edges.first(1000).each do |dist, a, b|
  dsu.union(a, b)
end

component_sizes = (0...points.length).reduce({}) do |acc, i|
  root = dsu.find(i)
  acc[root] ||= 0
  acc[root] += 1
  acc
end

puts component_sizes.values.sort[-3..].reduce(:*)

# Until everything is connected (part 2)
dsu = DSU.new(points.length)
last_visit = []
edges.each do |dist, a, b|
  break if dsu.all_connected?
  last_visit = [a, b]
  dsu.union(a, b)
end
puts points[last_visit.first].x * points[last_visit.last].x

