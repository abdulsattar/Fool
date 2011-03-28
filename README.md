FOOL is a new Functional and Object-Oriented Language targeting the .NET Framework.

Here are a few of it's features:

 1. Type Inference
 2. Functions as first-class citizens
 3. Tuples
 4. Slices
 5. Unicode source-code

Grammar:

    Prog 
            Stmts EOF
    Block
            { Stmts }
    Stmts
            Stmts Stmt
            Stmt
    Stmt
            Type Identifier = Value
            read Identifier;
        write Value;
            Block
        Value
    Value
        Identifier = Value
        Identifier
        Literal
    Literal
        INTLIT
        FLOATLIT
        CHARLIT
        STRINGLIT
        BoolLiteral

    BoolLiteral
        TRUE
        FALSE
        
    Identifier
            ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*
    Type
        int
        float
        char
        string
        bool
    Comment
            //

