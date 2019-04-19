/*
NOTE: In graphql-js, the content below is in /language/lexer.js. But to avoid cicular dependency in ReasonML, it is moved here. 
*/

type kind = 
  | SOF
  | EOF
  | Bang
  | Dollar
  | Amp
  | ParenLeft
  | ParenRight
  | Spread
  | Colon
  | Equals
  | At
  | BracketLeft
  | BracketRight
  | BraceLeft
  | Pipe
  | BraceRight
  | Name
  | Int
  | Float
  | String
  | BlockString
  | Comment
  ;

let string_of_kind = kind => {
  switch (kind) {
  | SOF => "<SOF>"
  | EOF => "<EOF>"
  | Bang => "!"
  | Dollar => "$"
  | Amp => "&"
  | ParenLeft => "("
  | ParenRight => ")"
  | Spread => "..."
  | Colon => ":"
  | Equals => "="
  | At => "@"
  | BracketLeft => "["
  | BracketRight => "]"
  | BraceLeft => "{"
  | Pipe => "|"
  | BraceRight => "}"
  | Name => "Name"
  | Int => "Int"
  | Float => "Float"
  | String => "String"
  | BlockString => "BlockString"
  | Comment => "Comment"
  }
};

/**
 * Represents a range of characters represented by a lexical token
 * within a Source.
 */
type t = {
  /**
   * The kind of Token.
   */
  kind: kind,

  /**
   * The character offset at which this Node begins.
   */
  start: int,

  /**
   * The character offset at which this Node ends.
   */
  end_: int,

  /**
   * The 1-indexed line number on which this Token appears.
   */
  line: int,

  /**
   * The 1-indexed column number at which this Token begins.
   */
  column: int,

  /**
   * For non-punctuation tokens, represents the interpreted value of the token.
   */
  value: option(string),

  /**
   * Tokens exist as nodes in a double-linked-list amongst all tokens
   * including ignored tokens. <SOF> is always the first node and <EOF>
   * the last.
   */
  prev: option(t),
  next: ref(option(t)),
};

let make = (
  kind: kind,
  start: int,
  end_: int,
  line: int,
  column: int,
  prev: option(t),
  value: option(string),
) => {
  kind,
  start,
  end_,
  line,
  column,
  prev,
  value,
  next: ref(None),
}

let desc = token => {
  switch(token.value) {
  | Some(value) => string_of_kind(token.kind) ++ " \"" ++ value ++ "\""
  | None => string_of_kind(token.kind)
  }
}

let unwrapValue = (
  token: t
): string => {
  switch(token.value) {
  | Some(v) => v
  | None => ""
  }
}