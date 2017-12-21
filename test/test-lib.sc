def main()
{ 
    a = [1,2,3];
    b = [7,3,10];
    c = [1.1, 2.2, 3.3];
    d = [4.4, 1.1, 6.6];
    f = a * b;
    (0<=i<#f) print(f[i]);
    f = a + b;
    (0<=i<#f) print(f[i]);
    f = a & b;
    (0<=i<#f) print(f[i]);
    e = c * d;
    (0<=i<#e) print(e[i]);
    e = c + d;
    (0<=i<#e) print(e[i]);
    e = c & d; 
    (0<=i<#e) print(e[i]);

}
