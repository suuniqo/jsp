if (cond) {
    /* this string has an invalid escape sequence */
    read "hola\q"
}


/* this characters are illegal */
$& |

/* this string is unterminated */
let dop = "lasdljkljdfsalkj

/* this literal is out of range for i16 */
let a = 029348028348230948092840923094809238409238409238;

if (dop == a * cond) {
    do {
        write "death";
    } while (cond);
}

dop &= a;

let invf = 42342492834723948723432739487.87238972349837249872984

/* this float has invalid format */
let b = 38284239823948.

/* this comment is unterminated */
/* iasdlkhj
