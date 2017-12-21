def main() {
    a= [];
    print(#a);      /* prints 0*/
    a = a + [1];
    print(a[0]);    /* prints 1 */
    print(#a);      /* prints 1 */
    a = a + [2];
    print(#a);      /* prints 2 */
    print(a[0]);    /* prints 1 */
    print(a[1]);    /* prints 2 */
    a = [];         
    print(#a);      /* prints 0 */
    (0<=i<5) { a = a + [i]; }
    print(#a);      /* prints 5 */
    (0 <= i < #a) print(a[i]); /* prints 0 1 2 3 4 */ 
}
