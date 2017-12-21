def main() {
    a= [1,2,3,4];
    (0 <= i < 4| true) print(a[i]); 
    print(#a);  
    a = [1,2,3];
    print("here");
    print(#a); 
    a[1] = a[2];
    print(a[1]); 
    b =  a + [9,10];
    (0 <= i < 5| true) print(b[i]);
}
