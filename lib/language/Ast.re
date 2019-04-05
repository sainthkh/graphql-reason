/*
NOTE: ast type definitions have been moved to type/AST.re

Because it's used in error/GraphQLError and language/Parser. 
It causes circular dependency. 

As they are type definitions for GraphQL, they're moved to type. 
*/