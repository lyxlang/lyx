
(* This file was auto-generated based on "lib/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 207 ->
        "Expected another declaration or the end of the file.\n"
    | 200 ->
        "Expected a comma for more tuple elements or closing parenthesis.\n"
    | 198 ->
        "Expected a closing parenthesis for the tuple.\n"
    | 196 ->
        "Expected an expression for the next list element.\n"
    | 195 ->
        "Expected a comma for more list elements or closing square bracket.\n"
    | 193 ->
        "Expected a comma for more list elements or closing square bracket.\n"
    | 189 ->
        "Expected another case or the end of the match expression.\n"
    | 186 ->
        "Expected an expression for the case body.\n"
    | 184 ->
        "Expected an expression for the guarded case.\n"
    | 183 ->
        "Expected an arrow after the guard expression.\n"
    | 182 ->
        "Expected a guard expression.\n"
    | 181 ->
        "Expected an arrow or \226\128\152if\226\128\153 for a guard clause.\n"
    | 178 ->
        "Expected an arrow or more constructor parameters.\n"
    | 176 ->
        "Expected a comma for more tuple elements or closing parenthesis.\n"
    | 174 ->
        "Expected a closing parenthesis for the tuple pattern.\n"
    | 170 ->
        "Expected another pattern for the list.\n"
    | 169 ->
        "Expected a comma for more list elements, closing bracket, or arrow.\n"
    | 164 ->
        "Expected another pattern after the OR operator.\n"
    | 163 ->
        "Expected an arrow or \226\128\152if\226\128\153 for a guard clause.\n"
    | 161 ->
        "Expected a closing square bracket for the spread pattern.\n"
    | 160 ->
        "Expected a comma for more list elements or closing square bracket.\n"
    | 155 ->
        "Expected a pattern or closing square bracket.\n"
    | 154 ->
        "Expected a pattern or closing parenthesis.\n"
    | 152 ->
        "Expected an arrow or parameters for the constructor pattern.\n"
    | 151 ->
        "Expected a pattern after the backslash.\n"
    | 150 ->
        "Expected a pattern to match.\n"
    | 149 ->
        "Expected a \226\128\152with\226\128\153 keyword after the match expression.\n"
    | 147 ->
        "Expected another binding.\n"
    | 146 ->
        "Expected an \226\128\152in\226\128\153 keyword or semicolon for more bindings.\n"
    | 144 ->
        "Expected an expression for the body of the let binding.\n"
    | 140 ->
        "Expected an expression for the \226\128\152else\226\128\153 branch.\n"
    | 139 ->
        "Expected an \226\128\152else\226\128\153 keyword to complete the if expression.\n"
    | 138 ->
        "Expected an expression for the \226\128\152then\226\128\153 branch.\n"
    | 137 ->
        "Expected a \226\128\152then\226\128\153 keyword after the condition.\n"
    | 134 ->
        "Expected an expression for the logical AND.\n"
    | 128 ->
        "Expected an expression to compare for inequality.\n"
    | 117 ->
        "Expected an expression for the pipe operator.\n"
    | 113 ->
        "Expected an expression after the subtraction operator.\n"
    | 109 ->
        "Expected an expression to concatenate.\n"
    | 106 ->
        "Expected an expression after the modulo operator.\n"
    | 99 ->
        "Expected the end of the expression or an operator.\n"
    | 97 ->
        "Expected an expression for the exponent.\n"
    | 94 ->
        "Expected the end of the expression or another operator.\n"
    | 92 ->
        "Expected an expression after the binary operator.\n"
    | 85 ->
        "Expected an expression after the unary operator.\n"
    | 80 ->
        "Expected a condition expression for the if statement.\n"
    | 79 ->
        "Expected an expression for the binding value.\n"
    | 78 ->
        "Expected an equals sign after the type annotation.\n"
    | 76 ->
        "Expected an equals sign for the binding.\n"
    | 75 ->
        "Expected a binding for the let expression.\n"
    | 74 ->
        "Expected an expression to match on.\n"
    | 73 ->
        "Expected the body expression for the lambda function.\n"
    | 70 ->
        "Expected a dot or more parameters for the lambda function.\n"
    | 69 ->
        "Expected a parameter for the lambda function.\n"
    | 68 ->
        "Expected an expression or closing square bracket.\n"
    | 66 ->
        "Expected an expression or closing parenthesis.\n"
    | 60 ->
        "Expected an expression for the function body.\n"
    | 59 ->
        "Expected an equals sign to begin the function body.\n"
    | 57 ->
        "Expected a type annotation after the colon.\n"
    | 54 ->
        "Expected more parameters, a type annotation, or an equals sign.\n"
    | 52 ->
        "Expected another parameter after the comma.\n"
    | 51 ->
        "Expected a comma for more parameters or a closing parenthesis.\n"
    | 46 ->
        "Expected a comma for more parameters or a closing parenthesis.\n"
    | 45 ->
        "Expected a parameter or closing parenthesis.\n"
    | 42 ->
        "Expected parameters, a type annotation, or an equals sign.\n"
    | 40 ->
        "Expected an identifier for the function or value name.\n"
    | 39 ->
        "Expected a type definition after the equals sign.\n"
    | 38 ->
        "Expected an equals sign after the type alias name.\n"
    | 32 ->
        "Expected an uppercase identifier for the type alias name.\n"
    | 29 ->
        "Expected the end of the variant definition or another backslash.\n"
    | 25 ->
        "Expected the end of the type constructor application.\n"
    | 24 ->
        "Expected another type in the tuple or a closing parenthesis.\n"
    | 22 ->
        "Expected a comma or closing parenthesis in the tuple type.\n"
    | 20 ->
        "Expected a closing square bracket to end the list type.\n"
    | 18 ->
        "Expected a type after the arrow in the function type.\n"
    | 17 ->
        "Expected the end of the type definition or an arrow for a function type.\n"
    | 11 ->
        "Expected a type for the list elements.\n"
    | 9 ->
        "Expected a type or closing parenthesis.\n"
    | 8 ->
        "Expected type parameters or the end of the type definition.\n"
    | 7 ->
        "Expected type annotations or another variant definition.\n"
    | 6 ->
        "Expected an uppercase identifier for the variant name.\n"
    | 3 ->
        "Expected a backslash to begin variant definitions after type parameters.\n"
    | 2 ->
        "Expected a backslash followed by variant definitions for the union type.\n"
    | 1 ->
        "Expected an uppercase identifier for the union type name.\n"
    | 0 ->
        "Unexpected token. Expected a declaration or definition to start the program.\n"
    | _ ->
        raise Not_found
