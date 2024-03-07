open Belt

let data = Node.Fs.readFileSync("./data/day09.txt", #utf8)
  -> String.trim

let heightmap = String.split_on_char('\n', data)
  -> List.toArray
  -> Array.map(subArr => {
    subArr
      -> Js.String2.split("")
      -> Array.map(Int.fromString)
      -> Array.map(Option.getUnsafe)
  })

let isLower = (item, other) =>
  switch other {
  | Some(n) => item < n
  | _ => true
  }

let get2dIsLower = (arr, item, i, j) =>
  switch arr[i] {
  | Some(row) => isLower(item, row[j])
  | _ => true
  }

let isLowerThanSurrounding = (heightmap, height, i, j) =>
  heightmap->get2dIsLower(height, i - 1, j) &&
    heightmap->get2dIsLower(height, i + 1, j) &&
    heightmap->get2dIsLower(height, i, j + 1) &&
    heightmap->get2dIsLower(height, i, j - 1)

let getRiskSum = arr =>
  arr->Array.reduceWithIndex(0, (acc, heights, i) => {
      heights->Array.reduceWithIndex(acc, (subacc, h, j) => {
        if heightmap->isLowerThanSurrounding(h, i, j) {
            subacc + h + 1
        } else {
          subacc
        }
      })
    })

Js.log(heightmap->getRiskSum)
