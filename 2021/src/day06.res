let data = Node.Fs.readFileSync("./data/day06.txt", #utf8)

let getFishCounts = fishes => {
  let empty = Belt.Array.make(9, 0)
  fishes->Belt.Array.reduce(empty, (acc, cur) => {
    acc[cur] = acc[cur] + 1
    acc
  })
}

let cycleCounts = (acc: array<float>, cur: float, timer: int) => {
  if timer == 0 {
    // Old fish go to a 6 timer, new fish get added to 8 timer
    acc[6] = acc[6] +. cur
    acc[8] = acc[8] +. cur
  } else {
    // Otherwise, countdown
    acc[timer-1] = acc[timer-1] +. cur
  }

  acc
}

let rec cycle = (fishes: array<float>, days) => {
  if days == 0 {
    fishes
  } else {
    let empty = Belt.Array.make(9, 0.0)
    let newFishes = fishes
      ->Belt.Array.reduceWithIndex(empty, cycleCounts)

    cycle(newFishes, days-1)
  }
}

let answer = String.split_on_char(',', String.trim(data))
  -> Belt.List.toArray
  -> Belt.Array.map(Belt.Int.fromString)
  -> Belt.Array.map(Belt.Option.getUnsafe)
  -> getFishCounts
  -> Belt.Array.map(Belt.Float.fromInt)
  -> cycle(256)
  -> Belt.Array.reduce(0.0, (acc, cur) => acc +. cur)

Js.log(answer)
