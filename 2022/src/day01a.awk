# Maintain a sum of every line
{ sum += $0 }

# ... that is flushed at every empty line
/^$/ {
    # ... after being compared to our current max
    if (sum > max) {
        max = sum
    }

    sum = 0
}

# ... that we print after the file is read
END { print max }
