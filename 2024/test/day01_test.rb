require "minitest/autorun"
require_relative "../src/day01"

class TestDayOne < Minitest::Test
  def setup
    @contents = <<-TEXT
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
    TEXT
    @solver = DayOne.new(@contents)
  end

  def test_part_one
    assert_equal 11, @solver.part_one
  end

  def test_part_two
    assert_equal 31, @solver.part_two
  end
end
