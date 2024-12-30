# Advent of code

1. Grab the session cookie from [Advent of Code](https://adventofcode.com/)

2. Pull input data:

```
bin/rake grab[1,2024]
```

3. Write a minitest and execute it:

```
bin/rake test
```

4. Run the problem against the real input data:

```
$ irb
irb> require_relative './2024/src/day01.rb'
irb> DayOne.run
```
