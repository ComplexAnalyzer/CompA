def main(){
   a = [1,2,3,4,5,6,7,8,9,10];
   b = a;
   c = b;
   d = a[1:4];
   (0<=i<#d) print(d[i]); 
   e = [1.1, 3.3, 4.4, 5.5, 6.6];
   f = e[0:2];
   (0<=i<#f) print(f[i]); 
   g = ["hi", "see", "ya", "later"];
   h = g[1:#g];
   (0<=i<#h) print(h[i]); 

}

