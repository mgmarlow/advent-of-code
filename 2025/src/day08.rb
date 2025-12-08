Point = Data.define(:x, :y, :z) do
  def distance(other)
    Math.sqrt((x - other.x)**2 + (y - other.y)**2 + (z - other.z)**2)
  end
end

points = File.read("data/day08.txt")
  .split("\n")
  .map { |line| Point.new(*line.split(",").map(&:to_i)) }

# https://en.wikipedia.org/wiki/Disjoint-set_data_structure
class DSU
  def initialize(n)
    @parent = (0...n).to_a
    @size = Array.new(n, 1)
  end

  def find(x) = @parent[x] = x == @parent[x] ? x : find(@parent[x])

  def size(x) = @size[find(x)]

  def union(a, b)
    ra = find(a)
    rb = find(b)
    return if ra == rb

    if @size[ra] < @size[rb]
      ra, rb = rb, ra
    end

    @parent[rb] = ra
    @size[ra] += @size[rb]
  end
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
  if dsu.size(a) == points.length
    break
  end
  last_visit = [a, b]
  dsu.union(a, b)
end
puts points[last_visit.first].x * points[last_visit.last].x

