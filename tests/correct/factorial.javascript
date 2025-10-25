/*********************************************/
/*          * / * N FACTORIAL * / *          */
/*********************************************/

/*
 * This function computes n! (n factorial)
 */
function int factorial(int n) {
    if(n==0){return 1;}
    if(n<=2){return n;}
    let int res=n;
    do{n=n-1;res=res*n;}while(n>2);
    return res;
}

/* read n and write n! */
let int n=0;read(n);let int res=factorial(n);write("n! = ", res);

/*** eof ***/
