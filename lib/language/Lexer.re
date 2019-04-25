/*
NOTE: Token definitions are moved to type/Token to avoid circular dependency.
*/
module Token = Type.Token;

/**
 * The return type of createLexer.
 */
type t = {
  source: Util.Source.t,
  options: Option.Parse.t,

  /**
   * The previously focused non-ignored token.
   */
  lastToken: ref(Token.t),

  /**
   * The currently focused non-ignored token.
   */
  token: ref(Token.t),

  /**
   * The (1-indexed) line containing the current token.
   */
  line: ref(int),

  /**
   * The character offset at which the current line begins.
   */
  lineStart: ref(int),

  /* NOTE: advance and lookahead are removed because ReasonML doesn't have "this". */
};

let make = (
  source, 
  ~options: option(Option.Parse.t) =?,
  ()
): t => {
  let option = 
    switch(options) {
    | Some(o) => o
    | None => Option.Parse.make(())
    }
  let startOfFileToken = Token.make(Token.SOF, 0, 0, 0, 0, None, None);

  {
    source: source,
    options: option,
    lastToken: ref(startOfFileToken),
    token: ref(startOfFileToken),
    line: ref(1),
    lineStart: ref(0),
  }
};

/** 
 * NOTE: provided for completeness. But it is recommended to use make().
 */
let createLexer = make; 

let printCharCode = code => {
  let values = [|
    "\"\\u0000\"", "\"\\u0001\"", "\"\\u0002\"", "\"\\u0003\"", "\"\\u0004\"", "\"\\u0005\"", "\"\\u0006\"", "\"\\u0007\"", "\"\\b\"", "\"\\t\"",
    "\"\\n\"", "\"\\u000b\"", "\"\\f\"", "\"\\r\"", "\"\\u000e\"", "\"\\u000f\"", "\"\\u0010\"", "\"\\u0011\"", "\"\\u0012\"", "\"\\u0013\"",
    "\"\\u0014\"", "\"\\u0015\"", "\"\\u0016\"", "\"\\u0017\"", "\"\\u0018\"", "\"\\u0019\"", "\"\\u001a\"", "\"\\u001b\"", "\"\\u001c\"", "\"\\u001d\"",
    "\"\\u001e\"", "\"\\u001f\"", "\" \"", "\"!\"", "\"\"\"", "\"#\"", "\"$\"", "\"%%\"", "\"&\"", "\"'\"",
    "\"(\"", "\")\"", "\"*\"", "\"+\"", "\",\"", "\"-\"", "\".\"", "\"/\"", "\"0\"", "\"1\"",
    "\"2\"", "\"3\"", "\"4\"", "\"5\"", "\"6\"", "\"7\"", "\"8\"", "\"9\"", "\":\"", "\";\"",
    "\"<\"", "\"=\"", "\">\"", "\"?\"", "\"@\"", "\"A\"", "\"B\"", "\"C\"", "\"D\"", "\"E\"",
    "\"F\"", "\"G\"", "\"H\"", "\"I\"", "\"J\"", "\"K\"", "\"L\"", "\"M\"", "\"N\"", "\"O\"",
    "\"P\"", "\"Q\"", "\"R\"", "\"S\"", "\"T\"", "\"U\"", "\"V\"", "\"W\"", "\"X\"", "\"Y\"",
    "\"Z\"", "\"[\"", "\"\\\"", "\"]\"", "\"^\"", "\"_\"", "\"`\"", "\"a\"", "\"b\"", "\"c\"",
    "\"d\"", "\"e\"", "\"f\"", "\"g\"", "\"h\"", "\"i\"", "\"j\"", "\"k\"", "\"l\"", "\"m\"",
    "\"n\"", "\"o\"", "\"p\"", "\"q\"", "\"r\"", "\"s\"", "\"t\"", "\"u\"", "\"v\"", "\"w\"",
    "\"x\"", "\"y\"", "\"z\"", "\"{\"", "\"|\"", "\"}\"", "\"~\"",
  |];

  switch(code < Array.length(values)) {
  | true => values[code]
  | false => Printf.sprintf("\\u%04x", code)
  }
};

/**
 * NOTE: port of JavaScript char.charCodeAt(position)
 */
let getCode = (text, pos) => int_of_char(String.get(text, pos));
/**
 * NOTE: port of JavaScript string.slice(start, end)
 */
let sliceString = (text, start, end_) => String.sub(text, start, end_ - start);

/**
 * Reads from body starting at startPosition until it finds a non-whitespace
 * character, then returns the position of that character for lexing.
 */
let rec positionAfterWhitespace = (body, startPosition, lexer) => {
  let bodyLength = String.length(body);
  let pos = startPosition;
  
  switch(pos < bodyLength) {
  | true => {
    let code = getCode(body, pos);
    let updateLineInfo = (newPosition) => {
      lexer.line := lexer.line^ + 1;
      lexer.lineStart := newPosition;
    };

    switch(code) {
    // tab | space | comma
    | 9 | 32 | 44 => positionAfterWhitespace(body, pos + 1, lexer)
    // BOM
    | 0xef => {
      switch(pos + 2 < bodyLength) {
      | true => {
        switch(getCode(body, pos + 1) == 0xbb && getCode(body, pos + 2) == 0xbf) {
        | true => positionAfterWhitespace(body, pos + 3, lexer)
        | false => pos
        }
      }
      | false => pos
      }
    }
    // newline
    | 10 => {
      let newPosition = pos + 1;
      updateLineInfo(newPosition);
      positionAfterWhitespace(body, newPosition, lexer);
    }
    // carriage return
    | 13 => {
      // Check for \r\n.
      let newPosition = 
        switch(pos + 1 < bodyLength && 
          int_of_char(String.get(body, pos + 1)) == 10) {
        | true => pos + 2
        | false => pos + 1
        }
      updateLineInfo(newPosition);
      positionAfterWhitespace(body, newPosition, lexer);
    }
    | _ => pos;
    }
  };
  | false => pos;
  }
};

let unexpectedCharacterMessage = code => {
  if (code < 0x0020 && code != 0x0009 && code != 0x000a && code != 0x000d) {
    "Cannot contain the invalid character " ++ printCharCode(code) ++ ".";
  } else if (code === 39) { // '
    "Unexpected single quote character ('), did you mean to use a double quote (\")?"
  } else {
    "Cannot parse the unexpected character " ++ printCharCode(code) ++ ".";
  }
};

/**
 * Reads a comment token from the source file.
 *
 * #[\u0009\u0020-\uFFFF]*
 */
let readComment = (
  source: Util.Source.t, 
  start: int, 
  line: int, 
  col: int, 
  prev: option(Type.Token.t)
) : Type.Token.t => {
  let body = source.body;
  let bodyLength = String.length(body);

  let rec findCommentEnd = (pos) => {
    switch(pos < bodyLength) {
    | false => pos
    | true => {
      let code = getCode(body, pos);
      // Source Character or TAB(0x0009)
      switch(code > 0x001f || code == 0x0009) {
      | false => pos + 1
      | true => findCommentEnd(pos + 1)
      }
    }
    }
  };

  let end_ = findCommentEnd(start + 1);

  Type.Token.make(
    Type.Token.Comment,
    start,
    end_,
    line,
    col,
    prev,
    Some(sliceString(body, start + 1, end_))
  );
};

/**
 * Reads an alphanumeric + underscore name from the source.
 *
 * [_A-Za-z][_0-9A-Za-z]*
 */
let rec readName = (
  source: Util.Source.t, 
  start: int, 
  line: int, 
  col: int, 
  prev: option(Type.Token.t)
) : Type.Token.t => {
  let body = source.body;
  let bodyLength = String.length(body);
  let pos = start + 1;

  readNameInternal(body, bodyLength, start, line, col, prev, pos);
}
and readNameInternal = (
  body: string,
  bodyLength: int,
  start: int,
  line: int,
  col: int,
  prev: option(Type.Token.t),
  pos: int
) : Type.Token.t => {
  let makeToken = () => Type.Token.make(Type.Token.Name, start, pos, line, col, prev, Some(sliceString(body, start, pos)))

  switch(pos < bodyLength) {
  | false => makeToken();
  | true => {
    let code = getCode(body, pos);

    switch(code == 95 || //_
    (code >= 48 && code <= 57) || // 0-9
    (code >= 65 && code <= 90) || // A-Z
    (code >= 97 && code <= 122)) { // a-z
    | false => makeToken();
    | true => readNameInternal(body, bodyLength, start, line, col, prev, pos + 1);
    }
  }
  }
};

/**
 * Reads a number token from the source file, either a float
 * or an int depending on whether a decimal point appears.
 *
 * Int:   -?(0|[1-9][0-9]*)
 * Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
 */
let rec readNumber = (
  source: Util.Source.t, 
  start: int, 
  line: int, 
  col: int, 
  prev: option(Type.Token.t)
) : Type.Token.t => {
  let body = source.body;
  let bodyLength = String.length(body);
  let isFloat = ref(false);
  
  let getCode_ = pos => {
    switch(pos < bodyLength) {
    | true => getCode(body, pos)
    | false => 0
    }
  };
  let isNumber = code => code >= 48 && code <= 57;
  let rec readDigits = (pos: int) => {
    let code = getCode_(pos);
    switch(isNumber(code)) {
    | true => readDigitsInternal(pos)
    | false => Error.API.syntaxError(
      source, 
      pos,
      "Invalid number, expected digit but got: " ++ 
        switch(pos < bodyLength) {
        | true => printCharCode(code)
        | false => "<EOF>"
        } ++ "."
    )
    }
  }
  and readDigitsInternal = (pos: int) => {
    let next = getCode_(pos + 1);
    switch(isNumber(next)) {
    | true => readDigitsInternal(pos + 1)
    | false => pos + 1
    }
  };

  // 1. Does it start with -?
  let code = getCode_(start);
  let pos = switch(code) {
  | 45 /* - */ => start + 1
  | _ => start
  };

  // 2. Handle integer part.
  let code = getCode_(pos);
  let pos = switch(code) {
  | 48 /* 0 */ => {
    let next = getCode_(pos + 1);
    switch(next >= 48 && next <= 57) {
    | false => pos + 1
    | true => Error.API.syntaxError(
      source, 
      pos + 1, 
      "Invalid number, unexpected digit after 0: " ++ printCharCode(next) ++ ".")
    }
  }
  | _ => readDigits(pos)
  }

  // 3. Handle fraction part.
  let code = getCode_(pos);
  let pos = switch(code) {
  | 46 /* . */ => {
    isFloat := true;
    readDigits(pos + 1);
  }
  | _ => pos
  }

  // 4. Handle E/e and exponents.
  let code = getCode_(pos);
  let pos = switch(code) {
  | 69 /* E */ | 101 /* e */ => {
    isFloat := true;
    let next = getCode_(pos + 1);
    switch(next) {
    | 43 /* + */ | 45 /* - */ => readDigits(pos + 2)
    | _ => readDigits(pos + 1)
    }
  }
  | _ => pos
  }

  Type.Token.make(
    isFloat^ ? Type.Token.Float : Type.Token.Int,
    start, 
    pos, 
    line,
    col,
    prev,
    Some(sliceString(body, start, pos))
  );
};

/**
 * Reads a block string token from the source file.
 *
 * """("?"?(\\"""|\\(?!=""")|[^"\\]))*""" 
 *
 * NOTE: " <= Double quote is added here to fix unfinished string bug in reason 3.4.0.
 */
let readBlockString = (
  source: Util.Source.t,
  start: int, 
  line: int, 
  col: int, 
  prev: option(Type.Token.t),
  lexer: t
): Type.Token.t => {
  let body = source.body;
  let bodyLength = String.length(body);

  let rec readBlockStringInternal = (pos, chunkStart, raw) => 
    switch (pos < bodyLength) {
    | true => {
      let code = getCode(body, pos);
      let updateLineInfo = (newPosition) => {
        lexer.line := lexer.line^ + 1;
        lexer.lineStart := newPosition;
      };

      switch(code) {
      // "
      | 34 => {
        // Can we close tag? (""")
        switch(pos + 2 < bodyLength && getCode(body, pos + 1) == 34 && getCode(body, pos + 2) == 34) {
        | true => {
          let final = Array.concat([raw, [|sliceString(body, chunkStart, pos)|]]);
          Type.Token.make(
            Type.Token.BlockString,
            start, 
            pos + 3,
            line,
            col,
            prev,
            Some(BlockString.dedent(String.concat("", Array.to_list(final))))
          )
        }
        | false => readBlockStringInternal(pos + 1, chunkStart, raw)
        }
      }
      // new line
      | 10 => {
        let next = pos + 1;
        updateLineInfo(next);
        readBlockStringInternal(next, chunkStart, raw);
      }
      // carriage return
      | 13 => {
        let next = 
          switch(pos + 1 < bodyLength && getCode(body, pos + 1) == 10) {
          | true => pos + 2
          | false => pos + 1
          }
        updateLineInfo(next);
        readBlockStringInternal(next, chunkStart, raw);
      }
      // \ 
      | 92 => {
        // Can we escape tag? (\""")
        switch(pos + 3 < bodyLength &&
          getCode(body, pos + 1) == 34 &&
          getCode(body, pos + 2) == 34 &&
          getCode(body, pos + 3) == 34 
        ) {
        | true => {
          let next = pos + 4;
          let newRaw = Array.concat([raw, [|sliceString(body, chunkStart, pos), "\"\"\""|]])
          readBlockStringInternal(next, next, newRaw);
        }
        | false => readBlockStringInternal(pos + 1, chunkStart, raw);
        }
      }
      | _ => {
        switch(code < 0x0020 &&
          code != 0x0009 && // TAB
          code != 0x000a && // new line
          code != 0x000d // carriage return
        ) {
        | true => Error.API.syntaxError(source, pos, "Invalid character within String: " ++ printCharCode(code) ++ ".")
        | false => readBlockStringInternal(pos + 1, chunkStart, raw)
        }
      }
      }
    }
    | false => Error.API.syntaxError(source, pos, "Unterminated string.")
    }
  
  let pos = start + 3;
  readBlockStringInternal(pos, pos, [||]);
};

/**
 * Converts a hex character to its integer value.
 * '0' becomes 0, '9' becomes 9
 * 'A' becomes 10, 'F' becomes 15
 * 'a' becomes 10, 'f' becomes 15
 *
 * Returns -1 on error.
 */
let char2hex = a => {
  let code = Char.code(a);
  code >= 48 && code <= 57
    ? code - 48 // 0-9
    : code >= 65 && code <= 70
    ? code - 55 // A-F
    : code >= 97 && code <= 102
    ? code - 87 // a-f
    : -1;
};

/**
 * Converts four hexadecimal chars to the integer that the
 * string represents. For example, uniCharCode('0','0','0','f')
 * will return 15, and uniCharCode('0','0','f','f') returns 255.
 *
 * Returns a negative number on error, if a char was invalid.
 *
 * This is implemented by noting that char2hex() returns -1 on error,
 * which means the result of ORing the char2hex() will also be negative.
 */
let uniCharCode = (a, b, c, d) => {
  char2hex(a) * 4096 + 
  char2hex(b) * 256 + 
  char2hex(c) * 16 +
  char2hex(d)
};

/**
 * NOTE: We need this function because Reason Native strings are just sequence of 8bit data.
 * This function creates a string. So, the length of one unicode character string isn't 1. 
 * (i.e. String.length("가") != 1 => It's actually 3.)
 */
let encode2utf8 = code => {
  if (code < 0x80) {
    Bytes.to_string(Bytes.make(1, Char.chr(code)));
  } else if (code < 0x800) {
    let a = [|192 + code / 64, 128 + code mod 64|];
    Bytes.to_string(Bytes.init(2, (i => Char.chr(a[i]))));
  } else {
    let a = [|224 + code / (64*64), 128 + (code / 64) mod 64, 128 + code mod 64|];
    Bytes.to_string(Bytes.init(3, (i => Char.chr(a[i]))));
  }
};

/**
 * Reads a string token from the source file.
 *
 * "([^"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|["\\/bfnrt])))*"
 * 
 * NOTE: As OCaml doesn't support unicode natively, I've currently removed \uXXXX syntax. 
 */
let readString = (
  source: Util.Source.t,
  start: int,
  line: int,
  col: int,
  prev: option(Type.Token.t)
): Type.Token.t => {
  let body = source.body;
  let bodyLength = String.length(body);

  let rec readStringInternal = (pos, chunkStart, raw) => {
    let error = () => 
      Error.API.syntaxError(source, pos, "Unterminated string.")

    switch(pos < bodyLength) {
    | true => {
      let code = getCode(body, pos);
      switch(
        code != 0x000a && // new line
        code != 0x000d // carriage return
      ) {
      | true => {
        switch(code) {
        // "
        | 34 => {
          let final = Array.concat([raw, [|sliceString(body, chunkStart, pos)|]]);
          Type.Token.make(
            Type.Token.String,
            start, 
            pos + 1,
            line,
            col,
            prev,
            Some(String.concat("", Array.to_list(final)))
          );
        }
        // \
        | 92 => {
          switch(pos + 1 < bodyLength) {
          | true => {
            let appendAndReadOn = str => {
              let next = pos + 2;
              readStringInternal(next, next, 
                Array.concat([raw, [|sliceString(body, chunkStart, pos), str|]]));
            };

            let next = getCode(body, pos + 1);
            let invalidCharError = () => 
              Error.API.syntaxError(
                source, 
                pos + 1, 
                "Invalid character escape sequence: " 
                  ++ "\\" ++ Char.escaped(Char.chr(next)) ++ "."
              );

            switch(next) {
            | 34 => appendAndReadOn("\"")
            | 47 => appendAndReadOn("/")
            | 92 => appendAndReadOn("\\")
            | 98 => appendAndReadOn("\b")
            | 102 => appendAndReadOn("\012") // \f form feed.
            | 110 => appendAndReadOn("\n")
            | 114 => appendAndReadOn("\r")
            | 116 => appendAndReadOn("\t")
            | 117 => {
              switch(pos + 5 < bodyLength) {
              | true => {
                let code = uniCharCode(
                  String.get(body, pos + 2),
                  String.get(body, pos + 3),
                  String.get(body, pos + 4),
                  String.get(body, pos + 5)
                );
                switch(code >= 0) {
                | true => {
                  let next = pos + 6;
                  readStringInternal(next, next, 
                    Array.concat([raw, [|sliceString(body, chunkStart, pos), encode2utf8(code)|]]))
                }
                | false => invalidCharError()
                }
              }
              | false => invalidCharError()
              }
            }
            | _ => invalidCharError();
            }
          }
          | false => error()
          }
        }
        | _ => {
          switch(code < 0x0020 &&
            code != 0x0009 // TAB
          ) {
          | true => Error.API.syntaxError(source, pos, "Invalid character within String: " ++ printCharCode(code) ++ ".")
          | false => readStringInternal(pos + 1, chunkStart, raw);
          }
        }
        }
      }
      | false => error();
      }
    }
    | false => error();
    }
  };


  let pos = start + 1;
  let chunkStart = pos;
  readStringInternal(pos, chunkStart, [||]);
};

/**
 * Gets the next token from the source starting at the given position.
 *
 * This skips over whitespace until it finds the next lexable token, then lexes
 * punctuators immediately or calls the appropriate helper function for more
 * complicated tokens.
 */
let readToken: (t, Token.t) => Token.t = (lexer, prev) => {
  let source = lexer.source;
  let body = source.body;
  let bodyLength = String.length(body);

  let pos = positionAfterWhitespace(body, prev.end_, lexer);
  let line = lexer.line^;
  let col = 1 + pos - lexer.lineStart^;
  let syntaxError = code =>
    Error.API.syntaxError(source, pos, unexpectedCharacterMessage(code));

  switch(pos >= bodyLength) {
  | true => Token.make(Token.EOF, bodyLength, bodyLength, line, col, Some(prev), None);
  | false => {
    let code = getCode(body, pos);

    switch(code) {
    // !
    | 33 => Token.make(Token.Bang, pos, pos + 1, line, col, Some(prev), None);
    // #
    | 35 => readComment(source, pos, line, col, Some(prev));
    // $
    | 36 => Token.make(Token.Dollar, pos, pos + 1, line, col, Some(prev), None);
    // &
    | 38 => Token.make(Token.Amp, pos, pos + 1, line, col, Some(prev), None);
    // (
    | 40 => Token.make(Token.ParenLeft, pos, pos + 1, line, col, Some(prev), None);
    // )
    | 41 => Token.make(Token.ParenRight, pos, pos + 1, line, col, Some(prev), None);
    // .
    | 46 => {
      switch(pos + 2 < bodyLength && getCode(body, pos + 1) == 46 && getCode(body, pos + 2) == 46) {
      | true => Token.make(Token.Spread, pos, pos + 3, line, col, Some(prev), None)
      | false => syntaxError(code)
      }
    }
    // : 
    | 58 => Token.make(Token.Colon, pos, pos + 1, line, col, Some(prev), None);
    // =
    | 61 => Token.make(Token.Equals, pos, pos + 1, line, col, Some(prev), None);
    // @
    | 64 => Token.make(Token.At, pos, pos + 1, line, col, Some(prev), None);
    // [
    | 91 => Token.make(Token.BracketLeft, pos, pos + 1, line, col, Some(prev), None);
    // ]
    | 93 => Token.make(Token.BracketRight, pos, pos + 1, line, col, Some(prev), None);
    // {
    | 123 => Token.make(Token.BraceLeft, pos, pos + 1, line, col, Some(prev), None);
    // |
    | 124 => Token.make(Token.Pipe, pos, pos + 1, line, col, Some(prev), None);
    // }
    | 125 => Token.make(Token.BraceRight, pos, pos + 1, line, col, Some(prev), None);
    // A-Z _ a-z
    | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78  | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90 
    | 95 
    | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122
    => readName(source, pos, line, col, Some(prev));
    // - 0-9
    | 45
    | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 
    => readNumber(source, pos, line, col, Some(prev));
    // "
    | 34 => {
      switch (pos + 2 < bodyLength && getCode(body, pos + 1) == 34 && getCode(body, pos + 2) == 34) { 
      | true => readBlockString(source, pos, line, col, Some(prev), lexer)
      | false => readString(source, pos, line, col, Some(prev));
      }
    }
    | _ => syntaxError(code)
    }
  }
  }
};

/* NOTE: advance and lookahead are here because there is no "this" in ReasonML. */
let lookahead = lexer => {
  let token = lexer.token^;

  switch(token.kind) {
  | Token.EOF => token
  | _ => {
    let rec lookaheadInternal = (lexer: t, token:Type.Token.t) => {
      let next = switch(token.next^) {
      | Some(t) => t
      | None => {
        let next = readToken(lexer, token);
        token.next := Some(next);
        next;
      }
      };

      switch(next.kind) {
      | Token.Comment => lookaheadInternal(lexer, next);
      | _ => next;
      }
    };

    lookaheadInternal(lexer, token);
  }
  }
};

let advance = lexer => {
  lexer.lastToken := lexer.token^;
  let next = lookahead(lexer);
  lexer.token := next;
  
  next;
};
