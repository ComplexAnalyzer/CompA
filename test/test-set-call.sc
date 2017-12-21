def main() {
    a = [foo(1), bar(2), cat(3)];
    (0<=i<#a) print(a[i]);
}

def foo(a) {
    return a *2;
}

def bar(b) {
    return b + 2;
}

def cat(b) {
    return b + 3;
}
