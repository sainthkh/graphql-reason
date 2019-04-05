/*
NOTE: In graphql-js, the content below is in /language/ast.js. But to avoid cicular dependency in ReasonML, it is moved here. 
*/

/**
 * Contains a range of UTF-8 character offsets and token references that
 * identify the region of the source from which the AST derived.
 */
type location = {
  /**
   * The character offset at which this Node begins.
   */
  start: int,

  /**
   * The character offset at which this Node ends.
   */
  end_: int,

  /**
   * The Token at which this Node begins.
   */
  startToken: Token.t,

  /**
   * The Token at which this Node ends.
   */
  endToken: Token.t,

  /**
   * The Source document the AST represents.
   */
  source: Util.Source.t,
};

type node = 
  | DocumentNode