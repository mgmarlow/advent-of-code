/[0-9]+/ {
  one[NR] = $1
  two[NR] = $2
  similarity[$2]++
}

END {
  sort(one)
  sort(two)

  sum = 0
  for (i = 1; i <= NR; i++) {
    sum += abs(one[i] - two[i])    
  }
  print(sum)

  totalsim = 0
  for (i = 1; i <= NR; i++) {
    v = one[i]
    totalsim += v * similarity[v]
  }
  print(totalsim)
}

function sort(arr, n, tmp) {
  n = length(arr)
  for (i = 1; i < n; i++) {
    for (j = 1; j <= n - i; j++) {
      if (arr[j] > arr[j+1]) {
        tmp = arr[j]
        arr[j] = arr[j+1]
        arr[j+1] = tmp
      }
    }
  }
}

function abs(v) {
  return v < 0 ? -v : v
}
