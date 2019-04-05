/**
 * A GraphQLError describes an Error found during the parse, validate, or
 * execute phases of performing a GraphQL operation. In addition to a message
 * and stack trace, it also includes information about the locations in a
 * GraphQL document and/or execution result that correspond to the Error.
 */
type t = {
  /**
   * An array of { line, column } locations within the source GraphQL document
   * which correspond to this error.
   *
   * Errors during validation often contain multiple locations, for example to
   * point out two things with the same name. Errors during execution include a
   * single location, the field which produced the error.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   */
  // locations: option(array(Language.Source.location)),

  /**
   * An array describing the JSON-path into the execution response which
   * corresponds to this error. Only included for errors during execution.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   * NOTE: In graphql-js, the type is string | number. 
   * With the help of string_of_int and string_of_float, we can convert numbers
   * into strings, so I dropped numbers. 
   */
  // path: option(array(string)),

  /**
   * An array of GraphQL AST Nodes corresponding to this error.
   */
  // nodes: option(array(Type.Ast.node)), 

  /**
   * The source GraphQL document for the first location of this error.
   *
   * Note that if this Error represents more than one node, the source may not
   * represent nodes after the first node.
   */
  // source: option(Source.t),

  /**
   * An array of character offsets within the source GraphQL document
   * which correspond to this error.
   */
  // positions: option(array(int)),

  /**
   * The original error thrown from a field resolver during execution.
   * Commented it out for later.
   */
  //+originalError: ?Error;

  /**
   * Extension fields to add to the formatted error.
   * Commented it out for later.
   */
  //+extensions: { [key: string]: mixed } | void;

  /**
   * A message describing the Error for debugging purposes.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   *
   * Note: should be treated as readonly, despite invariant usage.
   */
  message: string,  
};

let make = (
  message: string//,
  // Commented them out for later implementation
  // ~nodes: option(array(Type.Ast.node)) =?,
  // ~source: option(Source.t) =?,
  // ~positions: option(array(int)) =?,
  // path: option(array(string)) =?,
  // Skipping originalError and extensions.
  //()
) : t => {
  message: message, 
};

exception Exception(t);