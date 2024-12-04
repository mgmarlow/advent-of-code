my @one = ();
my @two = ();

for 'data/day01.txt'.IO.lines -> $line {
  my ($first, $second) = $line.split("   ");
  @one.push($first);
  @two.push($second);
}

sub abs-diff { $^a + (@^b[0] - @^b[1]).abs }
say reduce &abs-diff, 0, |(@one.sort Z @two.sort);

my %similarity = @two.Bag;
sub score { $^a + ($^b * (%similarity{$^b} // 0)) }
say reduce &score, 0, |@one;
