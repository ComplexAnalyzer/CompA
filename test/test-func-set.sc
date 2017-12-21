def main() {
    a = [1,2,3];
    d = func(a); /* passes by reference */ 
    (0<=i<#d) print(d[i]);

}

def func(a) {
    b = [4,5];
    c = a + b; /* makes a copy */
    return c; /* returns a reference */ 
}

