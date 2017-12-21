def foo(a, b) {
 return a == 1 && b == 1;
}

def main() {
(0 < x < 2, 0 < y < 2 | foo(x, y)) {
print(x); /* prints 1 */
print(y); /* prints 1 */
} return 0;
}
