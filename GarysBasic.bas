DECLARE FUNCTION GETTOK() AS STRING
DIM SHARED BUFF AS STRING
CONST OPS = 8
DIM SHARED OP(OPS) AS STRING
DIM I AS INTEGER
DIM J AS INTEGER
FOR I = 1 TO OPS 
    READ OP(I)
NEXT

DATA "+", "-", "*", "/", "(", ")", "=", "^"

FUNCTION GETTOK() AS STRING
    DIM COLLECT AS STRING
    DIM B AS STRING
    DIM B1 AS STRING
    DIM I AS INTEGER
    
    DO WHILE LEFT$(BUFF, 1) = " "
        BUFF = MID(BUFF, 2)
    LOOP
    
    IF BUFF = "" THEN
        GETTOK = "~"
        EXIT FUNCTION
    END IF
    
    COLLECT = ""
    
    B = LEFT(BUFF,1)
    B1 = MID$(BUFF, 2, 1)
    
    IF B = "<" AND B1 = "=" THEN
        BUFF = MID$(BUFF, 3)
        GETTOK = B + B1
        EXIT FUNCTION
    END IF
    
    IF B = "<" AND B1 = ">" THEN
        BUFF = MID$(BUFF, 3)
        GETTOK = B + B1
        EXIT FUNCTION
    END IF
    
    IF B = "<" THEN
        BUFF = MID$(BUFF, 2)
        GETTOK = B
        EXIT FUNCTION
    END IF
    
    IF B = ">" AND B1 = "=" THEN
        BUFF = MID$(BUFF, 3)
        GETTOK = B + B1
        EXIT FUNCTION
    END IF
    
    IF B = ">" THEN
        BUFF = MID$(BUFF, 2)
        GETTOK = B
        EXIT FUNCTION
    END IF
    
    FOR I = 1 TO OPS
        IF B = OP(I) THEN
            GETTOK = B
            BUFF = MID(BUFF, 2)
            EXIT FUNCTION
        END IF
    NEXT I
    
    
    
    IF B >= "A" AND B <= "Z" THEN
        DO WHILE B >= "A" AND B <= "Z" OR B >= "0" AND B <= "9"
            COLLECT = COLLECT + B
            BUFF = MID$(BUFF, 2)
            B = LEFT$(BUFF, 1)
        LOOP
        GETTOK = COLLECT
        EXIT FUNCTION
    END IF
    
    IF B >= "0" AND B <= "9" THEN
        DO WHILE B >= "0" AND B <= "9"
            COLLECT = COLLECT + B
            BUFF = MID$(BUFF, 2)
            B = LEFT$(BUFF, 1)
        LOOP
        GETTOK = COLLECT
        EXIT FUNCTION
    END IF
    
    GETTOK = "~":  REM END of file token
    
END FUNCTION

DECLARE FUNCTION PARSE_EXPR () AS STRING
DECLARE FUNCTION PARSE_OR() AS STRING

FUNCTION PARSE_FACTOR() AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    TOK = GETTOK()

    IF TOK= "(" THEN
        
        RPN = PARSE_OR()
        TOK = GETTOK()
        
        IF TOK <> ")" THEN
            PRINT "MISSING )";BUFF
            BUFF = ""
            PARSE_FACTOR = "~"
            EXIT FUNCTION
        END IF
        PARSE_FACTOR = RPN
        EXIT FUNCTION
    END IF
    
    IF TOK = "-" THEN
        RPN = PARSE_OR()
        PARSE_FACTOR = RPN + "UNM "
        EXIT FUNCTION
    END IF
    
    RPN = TOK + " "
    PARSE_FACTOR = RPN

END FUNCTION

FUNCTION PARSE_EXPONENT() AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    RPN = PARSE_FACTOR()
    TOK = GETTOK()

    DO WHILE TOK = "^"
        RPN = RPN + PARSE_FACTOR () + TOK
        TOK = GETTOK()
    LOOP

    BUFF = TOK + " " + BUFF
    PARSE_EXPONENT = RPN    
END FUNCTION

FUNCTION PARSE_UNARY_MINUS() AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    DIM NEG AS INTEGER
    
    NEG = FALSE
    
    TOK = GETTOK()
    DO WHILE TOK = "-" OR TOK = "+"
        IF TOK = "-" THEN
            IF NEG = FALSE THEN
                NEG = TRUE
            ELSEIF NEG = TRUE THEN
                NEG = FALSE
            END IF
        END IF
        TOK = GETTOK()
    LOOP
    
    BUFF = TOK + " " + BUFF
    
    RPN = PARSE_EXPONENT()
    
    IF NEG = TRUE THEN
        RPN = RPN + "UNM "
    END IF
    
    PARSE_UNARY_MINUS = RPN
    
END FUNCTION

FUNCTION PARSE_TERM ()AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    RPN = PARSE_UNARY_MINUS()
    TOK = GETTOK()
    
    DO WHILE TOK = "*" OR TOK = "/"
        RPN = RPN + PARSE_UNARY_MINUS() + TOK
        TOK = GETTOK()
    LOOP
    
    BUFF = TOK + " " + BUFF
    PARSE_TERM = RPN
    
END FUNCTION



FUNCTION PARSE_EXPR () AS STRING
    DIM PAUSE AS INTEGER
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    RPN = PARSE_TERM()
    TOK = GETTOK()
    
    DO WHILE TOK = "+" OR TOK = "-"
        RPN = RPN + PARSE_TERM() + TOK
        TOK = GETTOK()
    LOOP
    
    BUFF = TOK + " " + BUFF
    
    PARSE_EXPR = RPN
    
END FUNCTION

FUNCTION PARSE_CONDITION () AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    RPN = PARSE_EXPR()
    TOK = GETTOK()
    
    IF LEFT$(TOK, 1) = "=" OR LEFT$(TOK, 1) = ">" OR LEFT$(TOK, 1) = "<" THEN
        RPN = RPN + PARSE_EXPR() + TOK
        TOK = GETTOK()
    END IF
    
    BUFF = TOK + " " + BUFF
    PARSE_CONDITION = RPN
    
END FUNCTION

FUNCTION PARSE_AND () AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    RPN = PARSE_CONDITION()
    TOK = GETTOK()
    
    DO WHILE TOK = "AND"
        RPN = RPN + PARSE_CONDITION() + TOK + " "
        TOK = GETTOK()
    LOOP
    
    BUFF = TOK + " " + BUFF
    PARSE_AND = RPN
    
END FUNCTION

FUNCTION PARSE_OR () AS STRING
    DIM TOK AS STRING
    DIM RPN AS STRING
    
    RPN = PARSE_AND()
    TOK = GETTOK()
    
    DO WHILE TOK = "OR"
        RPN = RPN + PARSE_AND() + TOK + " "
        TOK = GETTOK()
    LOOP
    
    BUFF = TOK + " " + BUFF
    PARSE_OR = RPN
    
END FUNCTION

DO WHILE TRUE
    PRINT "ANOTHER";
    INPUT BUFF
    PRINT "FINAL ";PARSE_OR()    
LOOP





