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

type moreTokenExpected = {
  kind: Type.Token.kind,
  start: int,
  end_: int,
  line: int,
  column: int,
  value: option(string),
};

/**
 * NOTE: Unlike graphql-js, it tests one text in one test. Because it makes it easier to find problems. 
 */
describe("Lexer", ({describe, test}) => {
  /**
   * NOTE: Some tests are added to test cases separately. 
   */
  describe("records line and column", ({test}) => {
    let compare = (text, expected: moreTokenExpected) => {
      test("Text: " ++ text, ({expect}) => {
        let result: Type.Token.t = lexOne(text);
        
        expect.bool(result.kind == expected.kind).toBe(true);
        expect.int(result.start).toBe(expected.start);
        expect.int(result.end_).toBe(expected.end_);
        expect.int(result.line).toBe(expected.line);
        expect.int(result.column).toBe(expected.column);
        expect.bool(matchOptions(result.value, expected.value)).toBe(true);      
      })
    };

    compare("foo", {
      kind: Type.Token.Name,
      start: 0,
      end_: 3,
      line: 1,
      column: 1,
      value: Some("foo"),
    });

    compare("\n var", {
      kind: Type.Token.Name,
      start: 2,
      end_: 5,
      line: 2,
      column: 2,
      value: Some("var"),
    });

    compare("\n \r\n  bazz", {
      kind: Type.Token.Name,
      start: 6,
      end_: 10,
      line: 3,
      column: 3,
      value: Some("bazz"),
    });

    compare("\n \r\n \r  foo\n", {
      kind: Type.Token.Name,
      start: 8,
      end_: 11,
      line: 4,
      column: 3,
      value: Some("foo"),
    });
  });

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
