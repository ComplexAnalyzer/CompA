def main() 
{
    a = "hello";
    b = "hello";
    c = "hell"; 
    print(a == b);      /* prints 1 */ 
    print(a == c);      /* prints 0 */
    d = ["hello", "bye", "hi", "see", "ya"];
    e = ["hi", "see", "what"];
    f = d + e;
    (0<=i<#f) print(f[i]); /* prints hello bye hi see ya hi see what */
    print("hello"?f);      /* prints 1 */
    g = set(f);
    (0<=i<#g) print(g[i]);  /* prints hello bye hi see ya what */ 
    f = d * e;
    (0<=i<#f) print(f[i]);  /* prints hi see */
    f = d & e;
    (0<=i<#f) print(f[i]);  /* hello bye hi see ya what */
    f = d - e;
    (0<=i<#f) print(f[i]);  /* hello by ya */

}
