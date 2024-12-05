my $memory = 'data/day03.txt'.IO.lines.join('');

say sum(
  map(
    { $_[0] * $_[1] },
    $memory ~~ m:g/ 'mul(' (\d+) ',' (\d+) ')' /
  ));
