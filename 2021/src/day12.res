let data = Node.Fs.readFileSync("./data/day12.txt", #utf8)->String.trim

let isBigCave = str => str->Js.String.toUpperCase == str
let isSmallCave = str => !(str->isBigCave)
let overVisitLimit = (arr, v) => {
  if v == "start" || v == "end" {
    arr->Js.Array2.includes(v)
  } else {
    arr->Js.Array2.includes(v) && arr->Js.Array2.includes("twice")
  }
}

let toGraphTuple = str => {
  let contents = str->Js.String2.split("-")
  (contents[0], contents[1])
}

let addToGraph = (graph, k, v) => {
  switch graph->Js.Dict.get(k) {
  | Some(value) => {
      let newValue = Js.Array2.concat(value, [v])
      graph->Js.Dict.set(k, newValue)
    }
  | None => graph->Js.Dict.set(k, [v])
  }
}

let assembleGraph = arr =>
  arr->Belt.Array.reduce(Js.Dict.empty(), (acc, cur) => {
    let (node, edge) = cur

    acc->addToGraph(node, edge)
    // Remember to include backlinks
    acc->addToGraph(edge, node)

    acc
  })

let lines = data->Js.String2.split("\n")
let graph = lines->Belt.Array.map(toGraphTuple)->assembleGraph

let rec getAllPaths = (graph, node, visitedSmallCaves) => {
  if node == "end" {
    1
  } else {
    switch graph->Js.Dict.get(node) {
    | Some(edges) => edges->Js.Array2.reduce((acc, cur) => {
        switch visitedSmallCaves->overVisitLimit(cur) {
        | true => acc
        | false => {
            let caves = if cur->isSmallCave && visitedSmallCaves->Js.Array2.includes(cur) {
              visitedSmallCaves->Js.Array2.concat(["twice"])
            } else if cur->isSmallCave {
              visitedSmallCaves->Js.Array2.concat([cur])
            } else {
              visitedSmallCaves
            }

            acc + graph->getAllPaths(cur, caves)
          }
        }
      }, 0)
    | None => 0
    }
  }
}

Js.log(graph->getAllPaths("start", ["start"]))
