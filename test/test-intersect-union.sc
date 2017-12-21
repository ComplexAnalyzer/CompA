def main() {
    a = [1,2,3,4];
    b = [3,4,7,8,9];
    c = a * b;
    print("intersect");
    (0<=i<#c) print(c[i]);
    d = a & b;
    print("union");
    (0<=i<#d) print(d[i]);
}
