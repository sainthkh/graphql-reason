/**
 * The set of allowed directive location values.
 *
 * NOTE: We use variants here.
 */

type requestLocation =
  | Query
  | Mutation
  | Subscription
  | Field
  | FragmentDefinition
  | FragmentSpread
  | InlineFragment
  | VariableDefinition
  ;

type typeSystemLocation =
  | Schema
  | Scalar
  | Object
  | FieldDefinition
  | ArgumentDefinition
  | Interface
  | Union
  | Enum
  | EnumValue
  | InputObject
  | InputFieldDefinition
  ;

type t = 
  | RequestLocation(requestLocation)
  | TypeSystemLocation(typeSystemLocation)
  ;

exception UnexpectedLocation;

let requestLocation_of_string = loc => 
  switch(loc) {
  | "QUERY" => Query 
  | "MUTATION" => Mutation
  | "SUBSCRIPTION" => Subscription
  | "FIELD" => Field
  | "FRAGMENT_DEFINITION" => FragmentDefinition
  | "FRAGMENT_SPREAD" => FragmentSpread
  | "INLINE_FRAGMENT" => InlineFragment
  | "VARIABLE_DEFINITION" => VariableDefinition
  | _ => raise(UnexpectedLocation)
  }
  ;

let typeSystemLocation_of_string = loc => 
  switch(loc) {
  | "SCHEMA" => Schema
  | "SCALAR" => Scalar
  | "OBJECT" => Object
  | "FIELD_DEFINITION" => FieldDefinition
  | "ARGUMENT_DEFINITION" => ArgumentDefinition
  | "INTERFACE" => Interface
  | "UNION" => Union
  | "ENUM" => Enum
  | "ENUM_VALUE" => EnumValue
  | "INPUT_OBJECT" => InputObject
  | "INPUT_FIELD_DEFINITION" => InputFieldDefinition
  | _ => raise(UnexpectedLocation)
  }
  ;

let t_of_string = (loc: string) => 
  switch (loc) {
  | "QUERY" | "MUTATION" | "SUBSCRIPTION" | "FIELD" | "FRAGMENT_DEFINITION" 
  | "FRAGMENT_SPREAD" | "INLINE_FRAGMENT" | "VARIABLE_DEFINITION"
    => RequestLocation(requestLocation_of_string(loc))
  | "SCHEMA" | "SCALAR" | "OBJECT" | "FIELD_DEFINITION"
  | "ARGUMENT_DEFINITION" | "INTERFACE" | "UNION" | "ENUM" 
  | "ENUM_VALUE" | "INPUT_OBJECT" | "INPUT_FIELD_DEFINITION" 
    => TypeSystemLocation(typeSystemLocation_of_string(loc))
  | _ => raise(UnexpectedLocation)
  }
  ;
