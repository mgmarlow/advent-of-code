graph = File.read("data/day11.txt")
  .split("\n")
  .reduce({}) do |acc, line|
    node, *connections = line.split
    acc[node[..-2]] = connections
    acc
  end

def dfs(graph, startnode, endnode, memo = {})
  paths = []
  s = [[startnode, [startnode]]] # [node, path]

  until s.empty?
    node, path = s.pop

    if node == endnode
      paths << path
      next
    end

    (graph[node] || []).each do |nxt|
      s << [nxt, path + [nxt]]
    end
  end

  paths
end

puts dfs(graph, "you", "out").count

# Too slow:
# puts dfs(graph, "svr", "out")
#   .filter { |path| path.include?("dac") && path.include?("fft") }
#   .count

