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
  let expectSyntaxError = (
    text: string, 
    errMsg: string, 
    line: int, 
    col: int
  ) => {
    test("Test: " ++ text, ({expect}) => {
      switch(lexOne(text)) {
      | _token => expect.bool(false).toBe(true) // shouldn't be here.
      | exception Error.GraphQLError.Exception(err) => {
        expect.string(err.message).toEqual("Syntax Error: " ++ errMsg);
        
        switch(err.locations) {
        | None => expect.bool(false).toBe(true) // shouldn't be here.
        | Some(locations) => {
          expect.int(Array.length(locations)).toBe(1);
          let loc = locations[0];
          expect.int(loc.line).toBe(line);
          expect.int(loc.column).toBe(col);
        }
        }
      }
      }
    })
  };

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

  let compare = (text, expected: tokenExpected) => {
    test("Test Punctuation: " ++ text, ({expect}) => {
      let result: Type.Token.t = lexOne(text);

      expect.bool(result.kind == expected.kind).toBe(true);
      expect.int(result.start).toBe(expected.start);
      expect.int(result.end_).toBe(expected.end_);
      expect.bool(matchOptions(result.value, expected.value)).toBe(true);
    });
  };
  
  describe("lexes numbers", ({test}) => {
    compare("4", {
      kind: Type.Token.Int,
      start: 0,
      end_: 1,
      value: Some("4"),
    });

    compare("4.123", {
      kind: Type.Token.Float,
      start: 0,
      end_: 5,
      value: Some("4.123"),
    });

    compare("-4", {
      kind: Type.Token.Int,
      start: 0,
      end_: 2,
      value: Some("-4"),
    });

    compare("9", {
      kind: Type.Token.Int,
      start: 0,
      end_: 1,
      value: Some("9"),
    });

    compare("0", {
      kind: Type.Token.Int,
      start: 0,
      end_: 1,
      value: Some("0"),
    });

    compare("-4.123", {
      kind: Type.Token.Float,
      start: 0,
      end_: 6,
      value: Some("-4.123"),
    });

    compare("0.123", {
      kind: Type.Token.Float,
      start: 0,
      end_: 5,
      value: Some("0.123"),
    });

    compare("123e4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 5,
      value: Some("123e4"),
    });

    compare("123E4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 5,
      value: Some("123E4"),
    });

    compare("123e-4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 6,
      value: Some("123e-4"),
    });

    compare("123e+4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 6,
      value: Some("123e+4"),
    });

    compare("-1.123e4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 8,
      value: Some("-1.123e4"),
    });

    compare("-1.123E4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 8,
      value: Some("-1.123E4"),
    });

    compare("-1.123e-4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 9,
      value: Some("-1.123e-4"),
    });

    compare("-1.123e+4", {
      kind: Type.Token.Float,
      start: 0,
      end_: 9,
      value: Some("-1.123e+4"),
    });

    compare("-1.123e4567", {
      kind: Type.Token.Float,
      start: 0,
      end_: 11,
      value: Some("-1.123e4567"),
    });
  });

  describe("lex reports useful number errors", ({test}) => {
    expectSyntaxError("00", "Invalid number, unexpected digit after 0: \"0\".", 1, 2);
    expectSyntaxError("+1", "Cannot parse the unexpected character \"+\".", 1, 1);
    expectSyntaxError("1.", "Invalid number, expected digit but got: <EOF>.", 1, 3);
    expectSyntaxError("1.e1", "Invalid number, expected digit but got: \"e\".", 1, 3);
    expectSyntaxError(".123", "Cannot parse the unexpected character \".\".", 1, 1);
    expectSyntaxError("1.A", "Invalid number, expected digit but got: \"A\".", 1, 3);
    expectSyntaxError("-A", "Invalid number, expected digit but got: \"A\".", 1, 2);
    expectSyntaxError("1.0e", "Invalid number, expected digit but got: <EOF>.", 1, 5);
    expectSyntaxError("1.0eA", "Invalid number, expected digit but got: \"A\".", 1, 5);
  });

  describe("lexes punctuation", ({test}) => {
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
