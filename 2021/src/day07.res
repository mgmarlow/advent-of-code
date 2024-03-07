open Belt

let data = Node.Fs.readFileSync("./data/day07.txt", #utf8)
  -> String.trim

let positions = String.split_on_char(',', data)
  -> List.map(Int.fromString)
  -> List.map(Option.getUnsafe)
  -> List.toArray

let sorted = positions
  -> SortArray.Int.stableSort

let getMedian = arr => {
  arr[Array.length(arr) / 2]
    -> Option.getUnsafe
}

let rec moveTo = (arr, toPosition, fuel, fuelCost, increases) => {
  let fuelExpended = ref(0)

  let newPositions = arr->Array.map(p => {
    if p < toPosition {
      fuelExpended := fuelExpended.contents + fuelCost
      p + 1
    } else if p > toPosition {
      fuelExpended := fuelExpended.contents + fuelCost
      p - 1
    } else {
      p
    }
  })

  if fuelExpended.contents == 0 {
    fuel
  } else if increases {
    moveTo(newPositions, toPosition, fuel + fuelExpended.contents, fuelCost + 1, increases)
  } else {
    moveTo(newPositions, toPosition, fuel + fuelExpended.contents, fuelCost, increases)
  }
}

let optimal = getMedian(sorted)
Js.log(positions->moveTo(optimal, 0, 1, false))

let uniq = arr => arr->Array.keepWithIndex((item, i) => {
  switch arr[i - 1] {
  | Some(n) => n != item
  | _ => true
  }
})

let exhaustivelyGetLowestFuel = arr => {
  let fuelCost = ref(max_int)

  // This can be optimized by picking a smarter starting position
  arr->uniq->Array.forEach(position => {
    let cost = moveTo(arr, position, 0, 1, true)
    if cost < fuelCost.contents {
      fuelCost := cost
    }
  })

  fuelCost.contents
}

Js.log(exhaustivelyGetLowestFuel(sorted))
