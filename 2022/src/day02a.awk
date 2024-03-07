# Arrays in Awk are associative arrays, so we can use them as maps
BEGIN {
    values["X"] = 1 # rock
    values["Y"] = 2 # paper
    values["Z"] = 3 # scissors
}

function result(a, b) {
    if (a == "A" && b == "X")
        return 3
    else if (a == "A" && b == "Y")
        return 6
    else if (a == "A" && b == "Z")
        return 0
    else if (a == "B" && b == "X")
        return 0
    else if (a == "B" && b == "Y")
        return 3
    else if (a == "B" && b == "Z")
        return 6
    else if (a == "C" && b == "X")
        return 6
    else if (a == "C" && b == "Y")
        return 0
    else if (a == "C" && b == "Z")
        return 3
}

# Add up the score for every line
{ score += result($1, $2) + values[$2] }

END { print score }
