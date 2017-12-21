def main() {
    a = 6;
    b = [1,2,3,4]; 
    c = [5,6,7,8]; 
    print(a?b); /* prints 0 */
    print(a?c); /* prints 1 */
    print(3?b); /* prints 1 */
    print(3?c); /* prints 0 */
    e = 1.1;
    f = [1.1, 2.2];
    print(e?f); /* prints 1 */
}
