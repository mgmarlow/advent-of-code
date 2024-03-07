let data = Node.Fs.readFileSync("./data/day02.txt", #utf8)

let getCommandTuple = line => {
  let raw = String.split_on_char(' ', line)
  (List.nth(raw, 0), Belt.Int.fromString(List.nth(raw, 1)))
}

type sub = {
  mutable depth: int,
  mutable horizontalPosition: int,
  mutable aim: int
}

let initialSub: sub = {
  depth: 0,
  horizontalPosition: 0,
  aim: 0
}

let handleCommand = (sub, cmd, amt) => {
  switch cmd {
  | "forward" => {
    sub.horizontalPosition = sub.horizontalPosition + amt
    sub.depth = sub.depth + sub.aim * amt
    sub
  }
  | "down" => {
    sub.aim = sub.aim + amt
    sub
  }
  | "up" => {
    sub.aim = sub.aim - amt
    sub
  }
  | _ => {
    sub
  }
  }
}

let subFinal = String.split_on_char('\n', String.trim(data))
  -> Belt.List.map(getCommandTuple)
  -> Belt.List.map(((cmd, amt)) => (cmd, Belt.Option.getUnsafe(amt)))
  -> Belt.List.reduce(initialSub, (acc, (cmd, amt)) => acc->handleCommand(cmd, amt))

Js.log(subFinal.horizontalPosition * subFinal.depth)
