def main()
{
    a = [1,9,6,2,2,3,9];
    d = set_fun(a);
    print("set_fun");
    (0<=i<#d) print(d[i]);
    d = set(a);
    print("set");
    (0<=i<#d) print(d[i]);

}

def set_fun(a) {
    tmp = []; 
    (0<=i<#a |  a[i]?tmp == false) tmp = tmp + [a[i]];
    return tmp;
}
