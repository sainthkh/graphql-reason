/*
NOTE: In graphql-js, the content below is in /language/parser.js. But to avoid cicular dependency in ReasonML, it is moved here. 
*/

/**
 * Configuration options to control parser behavior
 */

module Parse {
  type t = {
    /**
    * By default, the parser creates AST nodes that know the location
    * in the source that they correspond to. This configuration flag
    * disables that behavior for performance or testing.
    */
    noLocation: option(bool),

    /**
    * If enabled, the parser will parse empty fields sets in the Schema
    * Definition Language. Otherwise, the parser will follow the current
    * specification.
    *
    * This option is provided to ease adoption of the final SDL specification
    * and will be removed in v16.
    */
    allowLegacySDLEmptyFields: option(bool),

    /**
    * If enabled, the parser will parse implemented interfaces with no `&`
    * character between each interface. Otherwise, the parser will follow the
    * current specification.
    *
    * This option is provided to ease adoption of the final SDL specification
    * and will be removed in v16.
    */
    allowLegacySDLImplementsInterfaces: option(bool),

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
    experimentalFragmentVariables: option(bool),
  };

  let make = (
    ~noLocation: option(bool) =?,
    ~allowLegacySDLEmptyFields: option(bool) =?,
    ~allowLegacySDLImplementsInterfaces: option(bool) =?,
    ~experimentalFragmentVariables: option(bool) =?,
    ()
  ) => {
    noLocation,
    allowLegacySDLEmptyFields,
    allowLegacySDLImplementsInterfaces,
    experimentalFragmentVariables,
  }
}