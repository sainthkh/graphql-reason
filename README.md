# graphql-reason: ReasonML/OCaml port for graphql-js


## Goal

Focus on creating parser and related tests. Do others later. 

## FAQ

### Why didn't you bind the graphql-js and create the port? 

graphql-reason project was born almost right after I released the 0.1.x version of [ReasonQL](https://github.com/sainthkh/reasonql). At first, I thought it was impossible to create GraphQL type-generation library with ppx. But before long, I realized that I was wrong. 

But the problem is that we don't have decent port of official [graphql-js](https://github.com/graphql/graphql-js). [graphql_ppx](https://github.com/mhallin/graphql_ppx) doesn't compile schema files. [reason-graphql](https://github.com/sikanhe/reason-graphql) and [ocaml-graphql-server](https://github.com/andreas/ocaml-graphql-server) don't parse interface. 

So, I decided to create my own library. 

### Why are you using ReasonML? Not OCaml? 

Personally, I'm more familiar with ReasonML than OCaml. 

And GraphQL is more frontend-y than backend-y. And frontend developers are more familiar with ReasonML and OCaml. 

So, to make it easy for them to participate, I decided to use ReasonML. 

### Why aren't you using OCamllex and Menhir? 

First of all, I'm not familiar with it, unfortunately. 

Second, we're not making something new from scratch. We have proven robust code we can copy. Then, when GraphQL spec changes, we don't have to change our own code base and write code based on official library. 