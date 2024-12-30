require 'dotenv/load'
require 'net/http'
require 'uri'
require "minitest/test_task"

Minitest::TestTask.create(:test) do |t|
  t.test_globs = ["2024/test/**/*_test.rb"]
end

def leftpad(s) = if s.length > 1 then s else "0#{s}" end

def get_input(day, year)
  uri = URI("https://adventofcode.com/#{year}/day/#{day}/input")
  response = Net::HTTP.get_response(uri, {
    'Cookie' => "session=#{ENV['SESSION_COOKIE']}"
  })
  response.body
end

task :grab, [:day, :year] do |t, args|
  input = get_input(args.day, args.year)
  dir = "#{args.year}/data"
  FileUtils.mkdir_p(dir)
  filename = "#{dir}/day#{leftpad(args.day)}.txt"
  File.write(filename, input)
end
