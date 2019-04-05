open Test.Framework;

let lexOne = str => {
  let lexer = Language.Lexer.make(Util.Source.make(~body=str, ()), ());
  Language.Lexer.advance(lexer);
};

let matchOptions: (option('a), option('b)) => bool = (result, expected) => {
  switch(result, expected) {
  | (None, None) => true
  | (Some(r), Some(e)) => r == e
  | (_, _) => false
  }
};

type tokenExpected = {
  kind: Type.Token.kind,
  start: int,
  end_: int,
  value: option(string),
};

describe("Lexer", ({test}) => {
  test("lexes punctuation", ({expect}) => {
    let compare = (result: Type.Token.t, expected: tokenExpected) => {
      expect.bool(result.kind == expected.kind).toBe(true);
      expect.int(result.start).toBe(expected.start);
      expect.int(result.end_).toBe(expected.end_);
      expect.bool(matchOptions(result.value, expected.value)).toBe(true);
    };
    
    compare(lexOne("!"), {
      kind: Type.Token.Bang,
      start: 0,
      end_: 1,
      value: None,
    });
    
    compare(lexOne("$"), {
      kind: Type.Token.Dollar,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("("), {
      kind: Type.Token.ParenLeft,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne(")"), {
      kind: Type.Token.ParenRight,
      start: 0,
      end_: 1,
      value: None,
    });

    /*
    compare(lexOne("..."), {
      kind: Type.Token.Spread,
      start: 0,
      end_: 3,
      value: None,
    });*/

    compare(lexOne(":"), {
      kind: Type.Token.Colon,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("="), {
      kind: Type.Token.Equals,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("@"), {
      kind: Type.Token.At,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("["), {
      kind: Type.Token.BracketLeft,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("]"), {
      kind: Type.Token.BracketRight,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("{"), {
      kind: Type.Token.BraceLeft,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("|"), {
      kind: Type.Token.Pipe,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(lexOne("}"), {
      kind: Type.Token.BraceRight,
      start: 0,
      end_: 1,
      value: None,
    });
  })
})
