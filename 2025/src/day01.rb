class Dial
  attr_reader :current

  def initialize
    @current = 50
  end

  def move(v)
    @current = (current + v) % 100
  end
end

class DayOne
  def initialize
    @lines = File.readlines("data/day01.txt")
      .map { |line| [line[0], line[1..].to_i] }
  end

  def part_one
    zeroes = 0
    dial = Dial.new
    @lines.each do |(dir, rot)|
      value = dir == "L" ? -1 * rot : rot
      dial.move(value)
      zeroes += 1 if dial.current == 0
    end
    zeroes
  end
  
  def part_two
    zeroes = 0
    dial = Dial.new
    @lines.each do |(dir, rot)|
      value = dir == "L" ? -1 * rot : rot

      initial = if dir == "R"
        dial.current == 0 ? 100 : 100 - dial.current
      else
        dial.current == 0 ? 100 : dial.current
      end

      zeroes += 1 + (rot - initial) / 100 if initial <= rot

      dial.move(value)
    end
    zeroes
  end
end

puts DayOne.new.part_one
puts DayOne.new.part_two
