/**
 * NOTE: As ReasonML doesn't support I created this module. 
 * Because it sounds more natural. If you have better name, please tell me.
 */

/**
 * Produces a GraphQLError representing a syntax error, containing useful
 * descriptive information about the syntax error's position in the source.
 * 
 * NOTE: Originally in error/syntaxError.js. But moved here for easy-to-read code.
 */
let syntaxError = (
  source: Util.Source.t,
  position: int,
  description: string,
) => {
  raise(
    GraphQLError.Exception(
      GraphQLError.make(
        "Syntax Error: " ++ description,
        ~source=source,
        ~positions=[|position|],
        ()
      )
    )
  );
};