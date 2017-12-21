/*def appendi(arr1, arr2, arr, len1, len2) {
    tmp1 = len1;
    tmp2 = len2; 
    (0<=i<len1) arr[i] = arr1[i];
    (0<=i<len2) { j = i + len1; arr[j] = arr2[i]; }
    return arr;
}*/

def main() {
    a = [1,2,3];
    b = [4,5];
    c = [0, 0, 0, 0, 0];
    print(a[1]);
    (0<=i<3) print(a[i]);
    (0<=i<2) print(b[i]);
    (0<=i<5) print(c[i]);
    print("call");
    appendi(a, b, c, 3, 2);
    print("after");
    (0<=i<5) print(c[i]);
    print("here");
    d = a + b;
    (0<=i<5) print(c[i]); 
}
