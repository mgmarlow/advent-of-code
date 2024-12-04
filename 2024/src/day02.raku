sub safe (@report) {
  my $p = @report[0];
  my $sign = ($p - @report[1]).sign;

  for 1..(@report.elems - 1) {
    my $cur = @report[$_];
    if ($p - $cur).abs > 3 {
      return False;
    }
    if ($p - $cur).sign != $sign {
      return False;
    }
    $p = $cur;
  }

  return True
}

my @reports = map { $^a.split(" ") }, 'data/day02.txt'.IO.lines;
say (grep &safe, @reports).elems;
