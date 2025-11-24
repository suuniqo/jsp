/* This function computes the nth fibonacci number */
function int fib(int n) {
    if (n == 0)
        return 0;

    if (n == 1)
        return 1;

    let int a = 0;
    let int b = 1;

    do {
        let int c = a + b;

        a = b;
        b = c;

        n = n + -1;
    } while (!(n < 2));

    return b;
}
