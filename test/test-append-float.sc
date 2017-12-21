def main() {
    a = [];
    print(1.1?a); /* print 0 */
    a = a + [1.1];
    print(1.1?a); /* print 1 */
    func(a);
}


def func(a) {
    b = 1.2;
    a = a + [b];
    print(a[1]); /* prints 1.2 */
    return b;
}
