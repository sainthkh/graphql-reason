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

/**
 * NOTE: In graphql-js, array fields like directives, arguments are optional. 
 * But in graphql-reason, they're non-optional because when there is no value,
 * we can simply show that with an empty array.
 * With this, we can remove meaningless switches.
 */

type nameNode = {
  loc: location,
  value: string,
};

type namedTypeNode = {
  loc: location,
  name: nameNode,
};

type complexTypeNode = 
  | ListTypeNode(typeNode)
  | NonNullTypeNode(typeNode)

and typeNode = 
  | NamedTypeNode(location, namedTypeNode)
  | ComplexTypeNode(location, complexTypeNode)
  ;

type variableNode = {
  loc: location,
  name: nameNode,
};

type intValueNode = {
  loc: location,
  // NOTE: Changed from string to int.
  // Because it is too tedious to convert it to int every time we use this.
  value: int,
};

type floatValueNode = {
  loc: location,
  // NOTE: Changed from string to float.
  // Because it is too tedious to convert it to float every time we use this.
  value: float,
};

type stringValueNode = {
  loc: location,
  value: string,
  // NOTE: In graphql-js, type is option(bool). 
  // But I ignored option here because we don't need the third value. 
  // In graphql-js, null/undefined isn't used for this value. 
  block: bool,
};

type booleanValueNode = {
  loc: location,
  value: bool,
};

type nullValueNode = {
  loc: location,
};

type enumValueNode = {
  loc: location,
  value: string,
};

type valueNode = 
  | VariableNode(location, variableNode)
  | IntValueNode(location, int)
  | FloatValueNode(location, float)
  | StringValueNode(location, string, bool)
  | BooleanValueNode(location, bool)
  | NullValueNode(location)
  | EnumValueNode(location, string)
  | ListValueNode(location, array(valueNode))
  | ObjectValueNode(location, array(objectFieldNode))

and objectFieldNode = {
  loc: location,
  name: nameNode,
  value: valueNode,
};

type listValueNode = {
  loc: location,
  values: array(valueNode),
};

type objectValueNode = {
  loc: location,
  fields: array(objectFieldNode),
};

type argumentNode = {
  loc: location,
  name: nameNode,
  value: valueNode,
};

type directiveNode = {
  loc: location,
  name: nameNode,
  arguments: array(argumentNode),
};

type variableDefinitionNode = {
  loc: location,
  variable: variableNode,
  type_: typeNode,
  defaultValue: option(valueNode),
  directives: array(directiveNode),
};

type selectionSetNode = {
  loc: location,
  selections: array(selectionNode),
}

and selectionNode = 
  | FieldNode(
    location, 
    option(nameNode), // alias
    nameNode, 
    array(argumentNode), // arguments
    array(directiveNode), // directives
    option(selectionSetNode)
  )
  | FragmentSpreadNode(
    location,
    nameNode,
    array(directiveNode)
  )
  | InlineFragmentNode(
    location,
    option(namedTypeNode),
    array(directiveNode),
    selectionSetNode
  )
  ;

type fieldNode = {
  loc: location,
  alias: option(nameNode),
  name: nameNode,
  arguments: array(argumentNode),
  directives: array(directiveNode),
  selectionSet: option(selectionSetNode),
};

type fragmentSpreadNode = {
  loc: location,
  name: nameNode,
  directives: array(directiveNode),
};

type inlineFragmentNode = {
  loc: location,
  typeCondition: option(namedTypeNode),
  directives: array(directiveNode),
  selectionSet: selectionSetNode,
};

type fragmentDefinitionNode = {
  loc: location,
  name: nameNode,
  // Note: fragment variable definitions are experimental and may be changed
  // or removed in the future.
  variableDefinitions: array(variableDefinitionNode),
  typeCondition: namedTypeNode,
  directives: array(directiveNode),
  selectionSet: selectionSetNode,
};

type operationTypeNode = 
  | Query
  | Mutation
  | Subscription
  ;

type operationDefinitionNode = {
  loc: location,
  operation: operationTypeNode,
  name: option(nameNode),
  variableDefinitions: array(variableDefinitionNode),
  directives: array(directiveNode),
  selectionSet: selectionSetNode,
};

type executableDefinitionNode = 
  | OperationDefinitionNode(operationDefinitionNode)
  | FragmentDefinitionNode(fragmentDefinitionNode)
  ;

type operationTypeDefinitionNode = {
  loc: location,
  operation: operationTypeNode,
  type_: namedTypeNode,
};

type schemaDefinitionNode = {
  loc: location,
  directives: array(directiveNode),
  operationTypes: array(operationTypeDefinitionNode),
};

type scalarTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: array(directiveNode),
};

type inputValueDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  type_: typeNode,
  defaultValue: option(valueNode),
  directives: array(directiveNode),
};

type fieldDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  arguments: option(array(inputValueDefinitionNode)),
  type_: typeNode,
  directives: array(directiveNode),
};

type objectTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  interfaces: option(array(namedTypeNode)),
  directives: array(directiveNode),
  fields: option(array(fieldDefinitionNode)),
};

type interfaceTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: array(directiveNode),
  fields: option(array(fieldDefinitionNode)),
};

type unionTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: array(directiveNode),
  types: option(array(namedTypeNode)),
};

type enumValueDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: array(directiveNode),
};

type enumTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: array(directiveNode),
  values: option(array(enumValueDefinitionNode)),
};

type inputObjectTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: array(directiveNode),
  fields: option(array(inputValueDefinitionNode)),
};

type typeDefinitionNode = 
  | ScalarTypeDefinitionNode(scalarTypeDefinitionNode)
  | ObjectTypeDefinitionNode(objectTypeDefinitionNode)
  | InterfaceTypeDefinitionNode(interfaceTypeDefinitionNode)
  | UnionTypeDefinitionNode(unionTypeDefinitionNode)
  | EnumTypeDefinitionNode(enumTypeDefinitionNode)
  | InputObjectTypeDefinitionNode(inputObjectTypeDefinitionNode)
  ;

type directiveDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  arguments: option(array(inputValueDefinitionNode)),
  locations: array(nameNode),
}

type typeSystemDefinitionNode = 
  | SchemaDefinitionNode(schemaDefinitionNode)
  | TypeDefinitionNode(typeDefinitionNode)
  | DirectiveDefinitionNode(directiveDefinitionNode)
  ;

type schemaExtensionNode = {
  loc: location,
  directives: array(directiveNode),
  operationTypes: option(array(operationTypeDefinitionNode)),
};

type scalarTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: array(directiveNode),
};

type objectTypeExtensionNode = {
  loc: location,
  name: nameNode,
  interfaces: option(array(namedTypeNode)),
  directives: array(directiveNode),
  fields: option(array(fieldDefinitionNode)),
};

type interfaceTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: array(directiveNode),
  fields: option(array(fieldDefinitionNode)),
};

type unionTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: array(directiveNode),
  types: option(array(namedTypeNode)),
};

type enumTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: array(directiveNode),
  values: option(array(enumValueDefinitionNode)),
};

type inputObjectTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: array(directiveNode),
  fields: option(array(inputValueDefinitionNode)),
};

type typeExtensionNode = 
  | ScalarTypeExtensionNode(scalarTypeExtensionNode)
  | ObjectTypeExtensionNode(objectTypeExtensionNode)
  | InterfaceTypeExtensionNode(interfaceTypeExtensionNode)
  | UnionTypeExtensionNode(unionTypeExtensionNode)
  | EnumTypeExtensionNode(enumTypeExtensionNode)
  | InputObjectTypeExtensionNode(inputObjectTypeExtensionNode)
  ;

type typeSystemExtensionNode = 
  | SchemaExtensionNode(schemaExtensionNode)
  | TypeExtensionNode(typeExtensionNode)
  ;

type definitionNode = 
  | ExecutableDefinitionNode(executableDefinitionNode)
  | TypeSystemDefinitionNode(typeSystemDefinitionNode)
  | TypeSystemExtensionNode(typeSystemExtensionNode)
  ;

type documentNode = {
  loc: location,
  definitions: array(definitionNode),
};

/**
 * NOTE: Unwrap exists to remove switchs. 
 */
exception UnwrapFailed;

let unwrap: option('a) => 'a = o => 
  switch(o) {
  | Some(v) => v
  | None => raise(UnwrapFailed)
  }
  ;

/**
 * NOTE: The functions below are utility functions to cast a node
 * to a certain type at once. 
 * They're created to remove excessive switch statements. 
 */
exception CastFailed(string);

let toExecutableDefinitionNode = definitionNode => 
  switch(definitionNode) {
  | ExecutableDefinitionNode(node) => node
  | _ => raise(CastFailed("It's not an ExecutableDefinitionNode"))
  }
  ;

let toOperationDefinitionNode = executableDefinitionNode => 
  switch(executableDefinitionNode) {
  | OperationDefinitionNode(node) => node
  | _ => raise(CastFailed("It's not an OperationDefinitionNode"))
  }
  ;

let toFieldNode = selectionNode => 
  switch(selectionNode) {
  | FieldNode(loc, alias, name, arguments, directives, selectionSet) => {
    loc,
    alias,
    name,
    arguments,
    directives,
    selectionSet,
  }
  | _ => raise(CastFailed("It's not a FieldNode"))
  }
  ;

let toFragmentSpreadNode = selectionNode => 
  switch(selectionNode) {
  | FragmentSpreadNode(loc, name, directives) => {
    loc,
    name,
    directives
  }
  | _ => raise(CastFailed("It's not a FragmentSpreadNode"))
  }
  ;

let toInlineFragmentNode = selectionNode => 
  switch(selectionNode) {
  | InlineFragmentNode(loc, typeCondition, directives, selectionSet) => {
    loc,
    typeCondition,
    directives,
    selectionSet,
  }
  | _ => raise(CastFailed("It's not an InlineFragmentNode"))
  }
  ;

let toVariableNode = valueNode => 
  switch(valueNode) {
  | VariableNode(_loc, variableNode) => variableNode
  | _ => raise(CastFailed("It's not a VariableNode"))
  }
  ;

let toIntValueNode: valueNode => intValueNode = valueNode => 
  switch(valueNode) {
  | IntValueNode(loc, value) => {
    loc,
    value,
  }
  | _ => raise(CastFailed("It's not an IntValueNode"))
  }
  ;

let toFloatValueNode: valueNode => floatValueNode = valueNode => 
  switch(valueNode) {
  | FloatValueNode(loc, value) => {
    loc,
    value,
  }
  | _ => raise(CastFailed("It's not a FloatValueNode"))
  }
  ;

let toStringValueNode: valueNode => stringValueNode = valueNode => 
  switch(valueNode) {
  | StringValueNode(loc, value, block) => {
    loc,
    value,
    block,
  }
  | _ => raise(CastFailed("It's not a StringValueNode"))
  }
  ;

let toBooleanValueNode: valueNode => booleanValueNode = valueNode => 
  switch(valueNode) {
  | BooleanValueNode(loc, value) => {
    loc,
    value,
  }
  | _ => raise(CastFailed("It's not a BooleanValueNode"))
  }
  ;

let toNullValueNode: valueNode => nullValueNode = valueNode => 
  switch(valueNode) {
  | NullValueNode(loc) => {
    loc: loc,
  }
  | _ => raise(CastFailed("It's not a NullValueNode"))
  }
  ;

let toEnumValueNode: valueNode => enumValueNode = valueNode => 
  switch(valueNode) {
  | EnumValueNode(loc, value) => {
    loc,
    value,
  }
  | _ => raise(CastFailed("It's not an EnumValueNode"))
  }
  ;

let toListValueNode: valueNode => listValueNode = valueNode => 
  switch(valueNode) {
  | ListValueNode(loc, values) => {
    loc,
    values,
  }
  | _ => raise(CastFailed("It's not a ListValueNode"))
  }
  ;

let toObjectValueNode: valueNode => objectValueNode = valueNode => 
  switch(valueNode) {
  | ObjectValueNode(loc, fields) => {
    loc,
    fields,
  }
  | _ => raise(CastFailed("It's not an ObjectValueNode"))
  }
  ;
