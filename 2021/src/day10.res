open Belt

exception Bad_expression(string)
exception Bad_token(string)

let data = Node.Fs.readFileSync("./data/day10.txt", #utf8)->String.trim

let getCompletedToken = c =>
  switch c {
  | "(" => ")"
  | "[" => "]"
  | "{" => "}"
  | "<" => ">"
  | _ => raise(Bad_token(c))
  }

let completes = (last, actual) => getCompletedToken(last) == actual

let getCompleted = line => {
  let stack = line->List.toArray
  let queue = []
  let autocomplete = []

  while Array.length(stack) > 0 {
    let last = stack->Js.Array2.pop->Option.getUnsafe

    if last == ")" || last == "}" || last == ">" || last == "]" {
      let _ = queue->Js.Array2.push(last)
    } else if Array.length(queue) > 0 {
      let closing = queue->Js.Array2.pop->Option.getUnsafe
      if !completes(last, closing) {
        raise(Bad_expression(closing))
      }
    } else {
      let _ = autocomplete->Js.Array2.push(getCompletedToken(last))
    }
  }

  autocomplete
}

let isValid = (line: list<string>) => {
  try {
    let _ = getCompleted(line)
    true
  } catch {
  | _ => false
  }
}

let tallyComplete = arr => {
  arr->Array.reduce(0.0, (acc, cur) => {
    let charScore = switch cur {
    | ")" => 1.0
    | "]" => 2.0
    | "}" => 3.0
    | ">" => 4.0
    | _ => 0.0
    }

    acc *. 5.0 +. charScore
  })
}

let getInvalidPoints = c =>
  switch c {
  | ")" => 3
  | "]" => 57
  | "}" => 1197
  | ">" => 25137
  | _ => 0
  }

let characterArrays =
  data->Js.String2.split("\n")->Array.map(s => s->Js.String2.split("")->List.fromArray)

let invalidPoints =
  characterArrays
  ->Array.map(line => {
    try {
      let _ = getCompleted(line)
      0
    } catch {
    | Bad_expression(origin) => getInvalidPoints(origin)
    | _ => 0
    }
  })
  ->Array.reduce(0, (acc, cur) => acc + cur)

Js.log(invalidPoints)
// 392139

let completedTallies =
  characterArrays
  ->Array.keep(isValid)
  ->Array.map(getCompleted)
  ->Array.map(tallyComplete)
  ->SortArray.stableSortBy((a, b) => {
    if a > b {
      1
    } else if a < b {
      -1
    } else {
      0
    }
  })

Js.log(completedTallies[Array.length(completedTallies) / 2])
// 4001832844
