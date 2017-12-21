def foo(a){
b = [a];
a = b[0] + a;
return a;
}

def main(){
arr = [1, 2, 3, 4, 5];
res = foo(#arr);
print(res);
}
