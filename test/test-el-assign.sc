def main() {
    a = [1,2,3,4];
    a = [1,2,3];
    b = a;
    print(b[1]);
    print(a[1]);
    b[0] = 100;
    print(a[0]);
    func(a);
    print(b[0]);
}


def func(a) {
    a[0] = 4;
}
