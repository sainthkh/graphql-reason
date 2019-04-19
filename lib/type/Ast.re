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
  value: string,
};

type floatValueNode = {
  loc: location,
  value: string,
};

type stringValueNode = {
  loc: location,
  value: string,
};

type booleanValueNode = {
  loc: location,
  value: string,
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
  | IntValueNode(location, string)
  | FloatValueNode(location, string)
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

type argumentNode = {
  loc: location,
  name: nameNode,
  value: valueNode,
};

type directiveNode = {
  loc: location,
  name: nameNode,
  arguments: option(array(argumentNode)),
};

type variableDefinitionNode = {
  loc: location,
  variable: variableNode,
  type_: typeNode,
  defaultValue: option(valueNode),
  directives: option(array(directiveNode)),
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
    option(array(argumentNode)), // arguments
    option(array(directiveNode)), // directives
    option(selectionSetNode)
  )
  | FragmentSpreadNode(
    location,
    nameNode,
    option(array(directiveNode))
  )
  | InlineFragmentNode(
    location,
    option(namedTypeNode),
    option(array(directiveNode)),
    selectionSetNode
  )
  ;

type fragmentDefinitionNode = {
  loc: location,
  name: nameNode,
  // Note: fragment variable definitions are experimental and may be changed
  // or removed in the future.
  variableDefinitions: option(array(variableDefinitionNode)),
  typeCondition: namedTypeNode,
  directives: option(array(directiveNode)),
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
  variableDefinitions: option(array(variableDefinitionNode)),
  directives: option(array(directiveNode)),
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
  directives: option(array(directiveNode)),
  operationTypes: array(operationTypeDefinitionNode),
};

type scalarTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: option(array(directiveNode)),
};

type inputValueDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  type_: typeNode,
  defaultValue: option(valueNode),
  directives: option(array(directiveNode)),
};

type fieldDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  arguments: option(array(inputValueDefinitionNode)),
  type_: typeNode,
  directives: option(array(directiveNode)),
};

type objectTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  interfaces: option(array(namedTypeNode)),
  directives: option(array(directiveNode)),
  fields: option(array(fieldDefinitionNode)),
};

type interfaceTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: option(array(directiveNode)),
  fields: option(array(fieldDefinitionNode)),
};

type unionTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: option(array(directiveNode)),
  types: option(array(namedTypeNode)),
};

type enumValueDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: option(array(directiveNode)),
};

type enumTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: option(array(directiveNode)),
  values: option(array(enumValueDefinitionNode)),
};

type inputObjectTypeDefinitionNode = {
  loc: location,
  description: option(stringValueNode),
  name: nameNode,
  directives: option(array(directiveNode)),
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
  directives: option(array(directiveNode)),
  operationTypes: option(array(operationTypeDefinitionNode)),
};

type scalarTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: option(array(directiveNode)),
};

type objectTypeExtensionNode = {
  loc: location,
  name: nameNode,
  interfaces: option(array(namedTypeNode)),
  directives: option(array(directiveNode)),
  fields: option(array(fieldDefinitionNode)),
};

type interfaceTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: option(array(directiveNode)),
  fields: option(array(fieldDefinitionNode)),
};

type unionTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: option(array(directiveNode)),
  types: option(array(namedTypeNode)),
};

type enumTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: option(array(directiveNode)),
  values: option(array(enumValueDefinitionNode)),
};

type inputObjectTypeExtensionNode = {
  loc: location,
  name: nameNode,
  directives: option(array(directiveNode)),
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
