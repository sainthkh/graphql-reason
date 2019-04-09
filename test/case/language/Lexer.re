open Test.Framework;

let lexOne = str => {
  let lexer = Language.Lexer.make(Util.Source.make(~body=str, ()), ());
  Language.Lexer.advance(lexer);
};

let lexSecond = str => {
  let lexer = Language.Lexer.make(Util.Source.make(~body=str, ()), ());
  ignore(Language.Lexer.advance(lexer));
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

  let compare = (text, expected: tokenExpected) => {
    test("Test: " ++ text, ({expect}) => {
      let result: Type.Token.t = lexOne(text);

      expect.bool(result.kind == expected.kind).toBe(true);
      expect.int(result.start).toBe(expected.start);
      expect.int(result.end_).toBe(expected.end_);
      expect.bool(matchOptions(result.value, expected.value)).toBe(true);
    });
  };

  describe("disallows uncommon control characters", ({test}) => {
    expectSyntaxError(
      "\007",
      "Cannot contain the invalid character \"\\u0007\".",
      1, 1
    );
  });

  describe("accepts BOM header", ({test}) => {
    compare("\xef\xbb\xbf foo", {
      kind: Type.Token.Name,
      start: 4,
      end_: 7,
      value: Some("foo"),
    });
  });

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

  describe("skips whitespace and comments", ({test}) => {
    compare({|

    foo


|}, {
      kind: Type.Token.Name,
      start: 6,
      end_: 9,
      value: Some("foo"),
    });

    compare({|
    #comment
    foo#comment
|}, {
      kind: Type.Token.Name,
      start: 18,
      end_: 21,
      value: Some("foo"),
    });

    compare(",,,foo,,,", {
      kind: Type.Token.Name,
      start: 3,
      end_: 6,
      value: Some("foo"),
    });
  });

  describe("lexes strings", ({test}) => {
    compare("\"simple\"", {
      kind: Type.Token.String,
      start: 0,
      end_: 8,
      value: Some("simple"),
    });

    compare("\" white space \"", {
      kind: Type.Token.String,
      start: 0,
      end_: 15,
      value: Some(" white space "),
    });

    compare("\"quote \\\"\"", {
      kind: Type.Token.String,
      start: 0,
      end_: 10,
      value: Some("quote \""),
    });

    compare("\"escaped \\n\\r\\b\\t\\f\"", {
      kind: Type.Token.String,
      start: 0,
      end_: 20,
      value: Some("escaped \n\r\b\t\012"),
    });

    compare("\"slashes \\\\ \\/\"", {
      kind: Type.Token.String,
      start: 0,
      end_: 15,
      value: Some("slashes \\ /"),
    });

    compare("\"unicode \\uac00\\u3042\\u798F\"", {
      kind: Type.Token.String,
      start: 0,
      end_: 28,
      value: Some("unicode 가あ福"),
    });
  });

  describe("lex reports useful string errors", ({test}) => {
    expectSyntaxError("\"", "Unterminated string.", 1, 2);
    expectSyntaxError("\"no end quote", "Unterminated string.", 1, 14);
    expectSyntaxError(
      "'single quotes'",
      "Unexpected single quote character ('), " ++
        "did you mean to use a double quote (\")?",
      1, 1
    );

    expectSyntaxError(
      "\"contains unescaped \007 control char\"",
      "Invalid character within String: \"\\u0007\".",
      1, 21
    );

    expectSyntaxError(
      "\"null-byte is not \000 end of file\"",
      "Invalid character within String: \"\\u0000\".",
      1, 19
    );

    expectSyntaxError("\"multi\nline\"", "Unterminated string.", 1, 7);

    expectSyntaxError("\"multi\rline\"", "Unterminated string.", 1, 7);

    expectSyntaxError(
      "\"bad \\z esc\"",
      "Invalid character escape sequence: \\z.",
      1, 7
    );

    expectSyntaxError(
      "\"bad \\x esc\"",
      "Invalid character escape sequence: \\x.",
      1, 7
    );
    
    expectSyntaxError(
      "\"bad \\u0XX1 esc\"",
      "Invalid character escape sequence: \\u.",
      1, 7
    );

    expectSyntaxError(
      "\"bad \\uXXXX esc\"",
      "Invalid character escape sequence: \\u.",
      1, 7
    );

    expectSyntaxError(
      "\"bad \\uXXXF esc\"",
      "Invalid character escape sequence: \\u.",
      1, 7
    );

    /*
     * NOTE: commented out because current lexer cannot detect these errors. 
     *
    expectSyntaxError(
      "\"bad \\u1 esc\"",
      "Invalid character escape sequence: \\u.",
      1, 7
    );

    expectSyntaxError(
      "\"bad \\uFXXX esc\"",
      "Invalid character escape sequence: \\u.",
      1, 7
    );*/
  });

  describe("lexes block strings", ({test}) => {
    compare("\"\"\"simple\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 12,
      value: Some("simple"),
    });

    compare("\"\"\" white space \"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 19,
      value: Some(" white space "),
    });

    compare("\"\"\"contains \" quote\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 22,
      value: Some("contains \" quote"),
    });

    compare("\"\"\"contains \\\"\"\" triplequote\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 31,
      value: Some("contains \"\"\" triplequote"),
    });

    compare("\"\"\"multi\nline\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 16,
      value: Some("multi\nline"),
    });

    compare("\"\"\"multi\rline\r\nnormalized\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 28,
      value: Some("multi\nline\nnormalized"),
    });

    compare("\"\"\"unescaped \\n\\r\\b\\t\\f\\u1234\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 32,
      value: Some("unescaped \\n\\r\\b\\t\\f\\u1234"),
    });

    compare("\"\"\"slashes \\\\ \\/\"\"\"", {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 19,
      value: Some("slashes \\\\ \\/"),
    });

    compare({|"""

        spans
          multiple
            lines

        """|}, {
      kind: Type.Token.BlockString,
      start: 0,
      end_: 68,
      value: Some("spans\n  multiple\n    lines"),
    });
  });

  describe("advance line after lexing multiline block string", ({test}) => {
    // NOTE: It is copied here because it uses lexSecond.
    let compare = (text, expected: moreTokenExpected) => {
      test("Text: " ++ text, ({expect}) => {
        let result: Type.Token.t = lexSecond(text);
        
        expect.bool(result.kind == expected.kind).toBe(true);
        expect.int(result.start).toBe(expected.start);
        expect.int(result.end_).toBe(expected.end_);
        expect.int(result.line).toBe(expected.line);
        expect.int(result.column).toBe(expected.column);
        expect.bool(matchOptions(result.value, expected.value)).toBe(true);      
      })
    };

    compare({|"""

        spans
          multiple
            lines

        
 """ second_token|}, {
      kind: Type.Token.Name,
      start: 71,
      end_: 83,
      line: 8,
      column: 6,
      value: Some("second_token"),
    });

    compare(
      String.concat("", [
        "\"\"\" \n",
        "spans \r\n",
        "multiple \n\r",
        "lines \n\n",
        "\"\"\"\n second_token",
      ]), {
      kind: Type.Token.Name,
      start: 37,
      end_: 49,
      line: 8,
      column: 2,
      value: Some("second_token"),
    });
  });

  describe("lex reports useful block string errors", ({test}) => {
    expectSyntaxError("\"\"\"", "Unterminated string.", 1, 4);
    expectSyntaxError("\"\"\"no end quote", "Unterminated string.", 1, 16);
    expectSyntaxError(
      "\"\"\"contains unescaped \007 control char\"\"\"",
      "Invalid character within String: \"\\u0007\".",
      1, 23
    );
    expectSyntaxError(
      "\"\"\"null-byte is not \000 end of file\"\"\"",
      "Invalid character within String: \"\\u0000\".",
      1, 21
    );
  });
  
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
