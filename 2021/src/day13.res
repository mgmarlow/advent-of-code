let data = Node.Fs.readFileSync("./data/day13.txt", #utf8)->String.trim

let flipHorizontal = m =>
  m->Belt.Array.map(row =>
    row->Belt.Array.mapWithIndex((i, _) => {
      let ri = row->Belt.Array.length - 1 - i
      row[ri]
    })
  )

let flipVertical = m => m->Belt.Array.reverse

type axis =
  | X
  | Y

let splitOnAxis = (m, i, axis) => {
  if axis == Y {
    let head = m->Js.Array2.slice(~start=0, ~end_=i)
    let tail = m->Js.Array2.slice(~start=i + 1, ~end_=m->Js.Array2.length)

    (head, tail)
  } else {
    let head = []
    let tail = []

    m->Js.Array2.forEach(row => {
      let headRow = row->Js.Array2.slice(~start=0, ~end_=i)
      let tailRow = row->Js.Array2.slice(~start=i + 1, ~end_=row->Js.Array2.length)

      head->Js.Array2.push(headRow)->ignore
      tail->Js.Array2.push(tailRow)->ignore
    })

    (head, tail)
  }
}

let overlay = (h, t) => h->Js.Array2.mapi((row, i) => row->Js.Array2.mapi((v, j) => v + t[i][j]))

let fold = (m, i, axis) => {
  let (head, tail) = splitOnAxis(m, i, axis)

  if axis == Y {
    overlay(flipVertical(head), tail)
  } else {
    overlay(flipHorizontal(head), tail)
  }
}

let sum = m => m->Js.Array2.reduce((acc, row) => acc + row->Js.Array2.reduce((colAcc, col) => {
      if col > 0 {
        colAcc + 1
      } else {
        colAcc
      }
    }, 0), 0)

let parseInstruction = str => {
  let rawCoords = str->Js.String2.split(" ")
  let coords = rawCoords[2]->Js.String2.split("=")

  let axis = if coords[0] == "y" {
    Y
  } else {
    X
  }

  (Belt.Int.fromString(coords[1])->Belt.Option.getUnsafe, axis)
}

let coords = []
let foldInstructions = []

data
->Js.String2.split("\n")
->Js.Array2.forEach(raw => {
  if raw->Js.String2.startsWith("fold") {
    foldInstructions->Js.Array2.push(raw)->ignore
  } else if raw != "" {
    let coord = {
      let values =
        raw
        ->Js.String2.split(",")
        ->Js.Array2.map(Js.Float.fromString)
        ->Js.Array2.map(Belt.Int.fromFloat)

      // Reverse to translate to i, j
      (values[1], values[0])
    }

    coords->Js.Array2.push(coord)->ignore
  }
})

let max = coords->Js.Array2.reduce((acc, cur) => {
  let (i, j) = cur
  let greater = i > j ? i : j

  if greater > acc {
    greater
  } else {
    acc
  }
}, 0)

let matrix: array<array<int>> = []
for _ in 0 to max {
  let row = Belt.Array.make(max + 1, 0)
  matrix->Js.Array2.push(row)->ignore
}

coords->Js.Array2.forEach(c => {
  let (i, j) = c
  matrix[i][j] = 1
})

// Answer 1
Js.log(sum(fold(matrix, 655, X)))

let pp = m =>
  m
  ->Js.Array2.map(row => row->Js.Array2.map(v => v > 0 ? "X" : ".")->Js.Array2.joinWith(""))
  ->Js.Array2.forEach(Js.log)

let finalMatrix =
  foldInstructions->Js.Array2.map(parseInstruction)->Js.Array2.reduce((acc, cur) => {
    let (v, axis) = cur
    fold(acc, v, axis)
  }, matrix)

// Answer 2
pp(finalMatrix->flipHorizontal->flipVertical)
