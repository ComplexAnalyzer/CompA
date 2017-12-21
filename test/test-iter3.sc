
def boo(a) {
    return  a;
}

def main() {
    (1<i<=5, 1<j<= boo(i) | true) {print(i); print(j);}
    a = [1,boo(3),3,boo(3)];
    print("hi");
    print(a[boo(3)]); /* prints 3 */
    print(boo(a[1])); /* prints 3 */
}
