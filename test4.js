/* Test lexer edge cases */

    /* Numbers */
    0
42
32767        /* max i16 */
    32768        /* overflow */
    123.456
0.0
1.234567890123456789  /* float precision test */
    123.            /* invalid float */

    /* Strings */
    "hello"
"hello\nworld"       /* escaped newline */
    "unterminated
"inv\qescape"
"this_is_a_very_long_string_exceeding_the_max_length_limit_of_the_lexer_to_trigger_str_overflow_diagnostic"

/* Identifiers */
    foo
_bar
keyword_if           /* assume 'if' is a keyword */
    _123abc

/* Operators */
    +
    -
    *
    */
    %
    =
    ==
    !
    !=
    <
    <=
    >
    >=
    &
    && 
    |=
    /*=      /* stray characters */

    /* Comments */
    /* valid comment */
    /*/   /* unterminated */
