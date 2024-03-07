let data = Node.Fs.readFileSync("./data/day14.txt", #utf8)->String.trim

let readRule = line => {
  open Js.String2

  let el = line->split("->")
  (el[0]->trim, el[1]->trim)
}

let getTemplateInfo = raw => {
  let template = ref("")
  let ruleList = []

  String.split_on_char('\n', raw)->Belt.List.forEachWithIndex((i, line) => {
    if i == 0 {
      template := line
    } else if line != "" {
      let rule = readRule(line)
      ruleList->Js.Array2.push(rule)->ignore
    }
  })

  let rules = Js.Dict.fromArray(ruleList)

  (template.contents, rules)
}

let counts = template => {
  template->Js.String2.split("")->Js.Array2.reduce((acc, cur) => {
    let count = switch acc->Js.Dict.get(cur) {
    | Some(n) => n + 1
    | None => 1
    }

    acc->Js.Dict.set(cur, count)

    acc
  }, Js.Dict.empty())
}

let rec growPolymer = (template, rules, step) => {
  open Js.String2

  if step <= 0 {
    template
  } else {
    let newTemplate = ref("")

    for i in 0 to template->length - 1 {
      let first = template->charAt(i)
      let second = template->charAt(i + 1)
      let pair = first ++ second

      newTemplate := newTemplate.contents ++ first
      switch rules->Js.Dict.get(pair) {
      | Some(c) => newTemplate := newTemplate.contents ++ c
      | _ => ()
      }
    }

    growPolymer(newTemplate.contents, rules, step - 1)
  }
}

let (template, rules) = getTemplateInfo(data)
let t10 = growPolymer(template, rules, 10)
Js.log(t10->counts)

// For part 2, we can't store the entire template due to its
// rate of growth. Instead, we only store counts of pairs
// in a hash.
let increment = (~by=1.0, dict, key) => {
  let count = switch dict->Js.Dict.get(key) {
  | Some(n) => n +. by
  | None => by
  }

  dict->Js.Dict.set(key, count)
}

let getPairsFromTemplate = template => {
  let els = template->Js.String2.split("")

  els->Belt.Array.reduceWithIndex(Js.Dict.empty(), (acc, cur, i) => {
    if i + 1 < els->Array.length {
      let pair = cur ++ els[i + 1]
      acc->increment(pair)
    }

    acc
  })
}

let rec countPairs = (templatePairs, rules, step) => {
  if step <= 0 {
    templatePairs
  } else {
    let newPairs = templatePairs->Js.Dict.entries->Js.Array2.reduce((acc, cur) => {
        let (pair, count) = cur

        switch rules->Js.Dict.get(pair) {
        | Some(c) => {
            let (left, right) = {
              let pairArray = pair->Js.String2.split("")
              (pairArray[0], pairArray[1])
            }

            acc->increment(left ++ c, ~by=count)
            acc->increment(c ++ right, ~by=count)
          }
        | None => acc->increment(pair, ~by=count)
        }

        acc
      }, Js.Dict.empty())

    countPairs(newPairs, rules, step - 1)
  }
}

let countCharacters = pairs => {
  let entries = pairs->Js.Dict.entries

  entries->Js.Array2.reducei((acc, cur, i) => {
    let (key, count) = cur

    // Disregard left since pairs intersect
    let (_, right) = {
      let components = key->Js.String2.split("")
      (components[0], components[1])
    }

    acc->increment(right, ~by=count)

    acc
  }, Js.Dict.empty())
}

let greatest = dict => {
  dict->Js.Dict.keys->Js.Array2.reduce((acc, cur) => {
    switch dict->Js.Dict.get(cur) {
    | Some(n) => n > acc ? n : acc
    | None => acc
    }
  }, 0.0)
}

let least = dict => {
  let keys = dict->Js.Dict.keys

  keys->Js.Array2.reduce((acc, cur) => {
    switch dict->Js.Dict.get(cur) {
    | Some(n) => n < acc ? n : acc
    | None => acc
    }
  }, dict->Js.Dict.get(keys[0])->Belt.Option.getUnsafe)
}

let pairs40 = countPairs(getPairsFromTemplate(template), rules, 40)
let counts40 = countCharacters(pairs40)
Js.log(counts40)
Js.log(greatest(counts40) -. least(counts40))
