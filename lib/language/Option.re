/*
NOTE: In graphql-js, the content below is in /language/parser.js. But to avoid cicular dependency in ReasonML, it is moved here. 
*/

let noneToFalse: option(bool) => bool = v => 
  switch(v) {
  | Some(v) => v
  | None => false
  }
  ;

/**
 * Configuration options to control parser behavior
 */
module Parse {
  type t = {
    /**
    * By default, the parser creates AST nodes that know the location
    * in the source that they correspond to. This configuration flag
    * disables that behavior for performance or testing.
    *
    * NOTE: Removed this option because it makes location field harder to use 
    * in ReasonML. And performance gain is negligible.
    */
    //noLocation: option(bool),

    /**
    * If enabled, the parser will parse empty fields sets in the Schema
    * Definition Language. Otherwise, the parser will follow the current
    * specification.
    *
    * This option is provided to ease adoption of the final SDL specification
    * and will be removed in v16.
    */
    allowLegacySDLEmptyFields: bool,

    /**
    * If enabled, the parser will parse implemented interfaces with no `&`
    * character between each interface. Otherwise, the parser will follow the
    * current specification.
    *
    * This option is provided to ease adoption of the final SDL specification
    * and will be removed in v16.
    */
    allowLegacySDLImplementsInterfaces: bool,

    /**
    * EXPERIMENTAL:
    *
    * If enabled, the parser will understand and parse variable definitions
    * contained in a fragment definition. They'll be represented in the
    * `variableDefinitions` field of the FragmentDefinitionNode.
    *
    * The syntax is identical to normal, query-defined variables. For example:
    *
    *   fragment A($var: BOOLEAN = false) on T  {
    *     ...
    *   }
    *
    * Note: this feature is experimental and may change or be removed in the
    * future.
    */
    experimentalFragmentVariables: bool,
  };

  let make = (
    //~noLocation: option(bool) =?,
    ~allowLegacySDLEmptyFields: option(bool) =?,
    ~allowLegacySDLImplementsInterfaces: option(bool) =?,
    ~experimentalFragmentVariables: option(bool) =?,
    ()
  ) => {
    //noLocation,
    allowLegacySDLEmptyFields: noneToFalse(allowLegacySDLEmptyFields),
    allowLegacySDLImplementsInterfaces: noneToFalse(allowLegacySDLImplementsInterfaces),
    experimentalFragmentVariables: noneToFalse(experimentalFragmentVariables),
  }
}