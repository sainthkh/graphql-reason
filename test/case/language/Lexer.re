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

/**
 * NOTE: Unlike graphql-js, it tests one text in one test. Because it makes it easier to find problems. 
 */
describe("Lexer", ({describe, test}) => {
  describe("lexes punctuation", ({test}) => {
    let compare = (text, expected: tokenExpected) => {
      test("Test Punctuation: " ++ text, ({expect}) => {
        let result: Type.Token.t = lexOne(text);

        expect.bool(result.kind == expected.kind).toBe(true);
        expect.int(result.start).toBe(expected.start);
        expect.int(result.end_).toBe(expected.end_);
        expect.bool(matchOptions(result.value, expected.value)).toBe(true);
      });
    };

    compare("!", {
      kind: Type.Token.Bang,
      start: 0,
      end_: 1,
      value: None,
    });
    
    compare("$", {
      kind: Type.Token.Dollar,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("(", {
      kind: Type.Token.ParenLeft,
      start: 0,
      end_: 1,
      value: None,
    });

    compare(")", {
      kind: Type.Token.ParenRight,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("...", {
      kind: Type.Token.Spread,
      start: 0,
      end_: 3,
      value: None,
    });

    compare(":", {
      kind: Type.Token.Colon,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("=", {
      kind: Type.Token.Equals,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("@", {
      kind: Type.Token.At,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("[", {
      kind: Type.Token.BracketLeft,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("]", {
      kind: Type.Token.BracketRight,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("{", {
      kind: Type.Token.BraceLeft,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("|", {
      kind: Type.Token.Pipe,
      start: 0,
      end_: 1,
      value: None,
    });

    compare("}", {
      kind: Type.Token.BraceRight,
      start: 0,
      end_: 1,
      value: None,
    });
  });
});
