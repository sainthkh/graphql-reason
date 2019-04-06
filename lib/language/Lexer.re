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
) => {
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

/**
 * Reads from body starting at startPosition until it finds a non-whitespace
 * character, then returns the position of that character for lexing.
 */
let rec positionAfterWhitespace = (body, startPosition, lexer) => {
  let bodyLength = String.length(body);
  let position = startPosition;
  
  switch(position < bodyLength) {
  | true => {
    let code = int_of_char(String.get(body, position));
    let updateLineInfo = (newPosition) => {
      lexer.line := lexer.line^ + 1;
      lexer.lineStart := newPosition;
    };

    switch(code) {
    // tab | space | comma | BOM
    | 9 | 32 | 44 | 0xfeff => positionAfterWhitespace(body, position + 1, lexer)
    // newline
    | 10 => {
      let newPosition = position + 1;
      updateLineInfo(newPosition);
      positionAfterWhitespace(body, newPosition, lexer);
    }
    // carriage return
    | 13 => {
      // Check for \r\n.
      let newPosition = 
        switch(position + 1 < bodyLength && 
          int_of_char(String.get(body, position + 1)) == 10) {
        | true => position + 2
        | false => position + 1
        }
      updateLineInfo(newPosition);
      positionAfterWhitespace(body, newPosition, lexer);
    }
    | _ => position;
    }
  };
  | false => position;
  }
};

let unexpectedCharacterMessage = code => {
  "";
};

/**
 * NOTE: port of JavaScript char.charCodeAt(position)
 */
let getCode = (text, pos) => int_of_char(String.get(text, pos));

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
  let makeToken = () => Type.Token.make(Type.Token.Name, start, pos, line, col, prev, Some(String.sub(body, start, pos - start)))

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

  switch(pos >= bodyLength) {
  | true => Token.make(Token.EOF, bodyLength, bodyLength, line, col, Some(prev), None);
  | false => {
    let code = getCode(body, pos);

    switch(code) {
    // !
    | 33 => Token.make(Token.Bang, pos, pos + 1, line, col, Some(prev), None);
    // #
    //| 35 => readComment(source, pos, line, col, Some(prev), None);
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
      | false => Error.API.syntaxError(source, pos, "Not enough dots for spread.")
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
    //| 45
    //| 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 
    //=> readNumber(source, pos, code, line, col, Some(prev), None);
    // "
    //| 34 => if (body.charCodeAt(pos + 1) === 34 && body.charCodeAt(pos + 2) === 34) { return readBlockString(source, pos, line, col, prev, lexer); }  return readString(source, pos, line, col, Some(prev), None);
    | _ => Error.API.syntaxError(source, pos, unexpectedCharacterMessage(code))
    }
  }
  }
};

/* NOTE: advance and lookahead are here because there is no "this" in ReasonML. */
/* NOTE2: lookahead changed a lot to use recursive function and pattern matching */
let rec lookahead: (t, Token.t) => Token.t = (lexer, token) => {
  switch(token.kind) {
  | Token.EOF => token
  | _ => 
    switch(token.next^) {
    | Some(next) => next
    | None => {
      let next = readToken(lexer, token);
      token.next := Some(next);
      switch(next.kind) {
      | Token.Comment => lookahead(lexer, next);
      | _ => next;
      }
    }
    }
  }
};

let advance = lexer => {
  lexer.lastToken := lexer.token^;
  let next = lookahead(lexer, lexer.token^);
  lexer.token := next;
  
  next;
};
