open Jest

let startingTemplate = "NNCB"

let rules = Js.Dict.fromList(list{
  ("CH", "B"),
  ("HH", "N"),
  ("CB", "H"),
  ("NH", "C"),
  ("HB", "C"),
  ("HC", "B"),
  ("HN", "C"),
  ("NN", "C"),
  ("BH", "H"),
  ("NC", "B"),
  ("NB", "B"),
  ("BN", "B"),
  ("BB", "N"),
  ("BC", "B"),
  ("CC", "N"),
  ("CN", "C"),
})

describe("day 14", () => {
  open Expect

  test("1 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 1)
    expect(polymer)->toBe("NCNBCHB")
  })

  test("2 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 2)
    expect(polymer)->toBe("NBCCNBBBCBHCB")
  })

  test("3 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 3)
    expect(polymer)->toBe("NBBBCNCCNBBNBNBBCHBHHBCHB")
  })

  test("4 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 4)
    expect(polymer)->toBe("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
  })

  test("#getPairsFromTemplate", () => {
    let pairs = Day14.getPairsFromTemplate("NNCB")
    expect(pairs)->toEqual(Js.Dict.fromList(list{("NN", 1.0), ("NC", 1.0), ("CB", 1.0)}))
  })

  test("#countPairs", () => {
    let pairs = Day14.getPairsFromTemplate("NNCB")
    let finalPairs = Day14.countPairs(pairs, rules, 1)
    expect(finalPairs)->toEqual(
      Js.Dict.fromList(list{
        ("NC", 1.0),
        ("CN", 1.0),
        ("NB", 1.0),
        ("BC", 1.0),
        ("CH", 1.0),
        ("HB", 1.0),
      }),
    )
  })

  test("#countPairs 2 step", () => {
    let pairs = Day14.getPairsFromTemplate("NNCB")
    let finalPairs = Day14.countPairs(pairs, rules, 2)
    expect(finalPairs)->toEqual(
      Js.Dict.fromList(list{
        ("NB", 2.0),
        ("BC", 2.0),
        ("CC", 1.0),
        ("CN", 1.0),
        ("BB", 2.0),
        ("CB", 2.0),
        ("BH", 1.0),
        ("HC", 1.0),
      }),
    )
  })
})
