type variable = string
type constant = string
type symbol = string
type term = V of variable | C of constant | Node of symbol * (term list)
type clause = term * (term list)
type database = (term * (term list)) list
type substitution = (variable * term) list
