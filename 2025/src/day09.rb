Point = Data.define(:x, :y)

Rect = Data.define(:p1, :p2) do
  def x1 = [p1.x, p2.x].min
  def x2 = [p1.x, p2.x].max
  def y1 = [p1.y, p2.y].min
  def y2 = [p1.y, p2.y].max

  def area() = (x2 - x1 + 1) * (y2 - y1 + 1)
end

points = File.readlines("data/day09.txt")
  .map { |line| Point.new(*line.split(",").map(&:to_i)) }

# Part one, max area
all_rects = []
points.each do |p1|
  points.each do |p2|
    all_rects << Rect.new(p1, p2)
  end
end
puts all_rects.max_by { |r| r.area }.area

# Part two

# Crucial to map coordinates to a smaller overall space so the
# flood-fill algorithm deals with smaller numbers.
def compress_coordinates(points)
  xs = points.map(&:x).uniq.sort
  ys = points.map(&:y).uniq.sort
  
  x_map = xs.each_with_index.to_h
  y_map = ys.each_with_index.to_h
  
  x_reverse = xs
  y_reverse = ys
  
  compressed_points = points.map do |p|
    Point.new(x_map[p.x], y_map[p.y])
  end
  
  [compressed_points, x_reverse, y_reverse]
end

def flood_fill_outside(points, max_x, max_y)
  # Build set of polygon edges
  edges = Set.new
  points.each_with_index do |p1, i|
    p2 = points[(i + 1) % points.size]
    
    if p1.x == p2.x
      ([p1.y, p2.y].min..[p1.y, p2.y].max).each do |y|
        edges.add(Point.new(p1.x, y))
      end
    else
      ([p1.x, p2.x].min..[p1.x, p2.x].max).each do |x|
        edges.add(Point.new(x, p1.y))
      end
    end
  end
  
  # Flood fill from outside (using compressed coordinates)
  outside = Set.new
  queue = [Point.new(-1, -1)]
  outside.add(queue.first)
  
  while !queue.empty?
    current = queue.shift
    
    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dx, dy|
      next_point = Point.new(current.x + dx, current.y + dy)
      
      # Out-of-bounds check
      next if next_point.x < -1 || next_point.x > max_x + 1
      next if next_point.y < -1 || next_point.y > max_y + 1

      # Skip if already visited or at polygon edge
      next if outside.include?(next_point)
      next if edges.include?(next_point)
      
      outside.add(next_point)
      queue.push(next_point)
    end
  end
  
  outside
end

def rect_valid?(rect, outside, x_coords, y_coords)
  # Check perimeter points in compressed space
  x1, x2 = rect.x1, rect.x2
  y1, y2 = rect.y1, rect.y2
  
  # Top and bottom edges
  (x1..x2).each do |x|
    return false if outside.include?(Point.new(x, y1))
    return false if outside.include?(Point.new(x, y2))
  end
  
  # Left and right edges
  ((y1+1)..(y2-1)).each do |y|
    return false if outside.include?(Point.new(x1, y))
    return false if outside.include?(Point.new(x2, y))
  end
  
  true
end

# Compress coordinates
compressed_points, x_coords, y_coords = compress_coordinates(points)

max_x = compressed_points.map(&:x).max
max_y = compressed_points.map(&:y).max

# Flood fill in compressed space
outside = flood_fill_outside(compressed_points, max_x, max_y)

# Find all valid rectangles in compressed space
max_area = 0

compressed_points.each_with_index do |p1, i|
  compressed_points[(i+1)..].each do |p2|
    rect = Rect.new(p1, p2)
    if rect_valid?(rect, outside, x_coords, y_coords)
      # Calculate actual area using original coordinates
      actual_width = x_coords[rect.x2] - x_coords[rect.x1] + 1
      actual_height = y_coords[rect.y2] - y_coords[rect.y1] + 1
      area = actual_width * actual_height
      max_area = [max_area, area].max
    end
  end
end

puts max_area
