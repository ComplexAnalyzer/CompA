def main() {
    a = [true, true, true];
    print("Len: ");
    print(#a);
    print("false in a");
    print(false?a);
    print("a:");
    (0<=i<#a) print(a[i]);
    a[1] = false;
    print("new a: ");
    (0<=i<#a) print(a[i]);
    print("false in a");
    print(false?a);
    a = set(a);
    print("new a: ");
    (0<=i<#a) print(a[i]);
    print(#a);
}
