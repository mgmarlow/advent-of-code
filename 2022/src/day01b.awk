# Maintain a sum of every line
{ sum += $0 }

# ... that is flushed at every empty line
/^$/ {
    # ... after it's appended to an array (NR = num row, a built-in
    # variable that increments with every line).
    sums[NR] = sum
    sum = 0
}

# We'll print out the array of sums at the end of parsing so we can
# pass it to sort.
END {
    for (sum in sums) {
        print sums[sum]
    }
}

# This program alone doesn't compute the answer for us, so we'll need
# to pipe it to a few other commands:
#
# awk -f 2022/day1p2.awk 2022/day1.txt \
#     | sort -n \
#     | tail -3 \
#     | awk '{sum += $0} END {print sum}'
