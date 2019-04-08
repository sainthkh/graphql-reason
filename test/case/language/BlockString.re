open Test.Framework;

let concatLines = lines => String.concat("\n", lines);

describe("Language.BlockString.dedent", ({test}) => {
  test("removes uniform indentation from a string", ({expect}) => {
    let rawValue = concatLines([
      "",
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL.",
    ]);
    expect.string(Language.BlockString.dedent(rawValue)).toEqual(
      concatLines([
        "Hello,", 
        "  World!", 
        "", 
        "Yours,", 
        "  GraphQL."
      ])
    );
  });

  test("removes empty leading and trailing lines", ({expect}) => {
    let rawValue = concatLines([
      "",
      "",
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL.",
      "",
      "",
    ]);
    expect.string(Language.BlockString.dedent(rawValue)).toEqual(
      concatLines([
        "Hello,", 
        "  World!", 
        "", 
        "Yours,", 
        "  GraphQL."]
      )
    );
  });

  test("removes blank leading and trailing lines", ({expect}) => {
    let rawValue = concatLines([
      "  ",
      "        ",
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL.",
      "        ",
      "  ",
    ]);
    expect.string(Language.BlockString.dedent(rawValue)).toEqual(
      concatLines([
        "Hello,", 
        "  World!", 
        "", 
        "Yours,", 
        "  GraphQL."
      ])
    );
  });

  test("retains indentation from first line", ({expect}) => {
    let rawValue = concatLines([
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL.",
    ]);
    expect.string(Language.BlockString.dedent(rawValue)).toEqual(
      concatLines([
        "    Hello,", 
        "  World!", 
        "", 
        "Yours,", 
        "  GraphQL."
      ])
    );
  });

  test("does not alter trailing spaces", ({expect}) => {
    let rawValue = concatLines([
      "               ",
      "    Hello,     ",
      "      World!   ",
      "               ",
      "    Yours,     ",
      "      GraphQL. ",
      "               ",
    ]);
    expect.string(Language.BlockString.dedent(rawValue)).toEqual(
      concatLines([
        "Hello,     ",
        "  World!   ",
        "           ",
        "Yours,     ",
        "  GraphQL. ",
      ])
    );
  });
});