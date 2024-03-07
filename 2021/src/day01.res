let data = Node.Fs.readFileSync("./data/day01.txt", #utf8)

let readings: array<int> = String.split_on_char('\n', data)
  -> Belt.List.map(reading => Belt.Int.fromString(reading))
  -> Belt.List.keep(Belt.Option.isSome)
  -> Belt.List.map(Belt.Option.getUnsafe)
  -> Belt.List.toArray

let createWindow = arr => Belt.Array.mapWithIndex(arr, (index, reading) => {
  if index - 1 >= 0 && index + 1 < Belt.Array.length(arr) {
    let window = arr[index - 1] + reading + arr[index + 1]
    Some(window)
  } else {
    None
  }
})

let windows = readings
  -> createWindow
  -> Belt.Array.keep(Belt.Option.isSome)
  -> Belt.Array.map(Belt.Option.getUnsafe)

let sumIncreases = arr => Belt.Array.reduceWithIndex(arr, 0, (acc, cur, index) => {
  if index - 1 >= 0 && cur > arr[index - 1] {
    acc + 1
  } else {
    acc
  }
  })

Js.log(readings->sumIncreases)
Js.log(windows->sumIncreases)
