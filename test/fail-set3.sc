def foo(a, b, c){
set3 = a + b;
set4 = set3 + c; /* should fail, set + bool*/
}

def main(){
set1 = ["a", "b", "c", "d"];
set2 = [1, 2, 3, 4, 5];
c = true;
foo(set1, set2, c);
return 0;
}
