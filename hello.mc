

int main()
{   
    float a1;
    float a2;
    float a3;
    float a4;
    float a5;

    cx c1;
    cx c2;
    cx c3;
    cx c4;
    cx c5;
    cx c6;

    int i1;
    int i2;
    int i3；
    int i4；
    int i5;
  
    print("VBbigidiot");
   
    a1= sqrt(93.2);
    print(a1);
    
    a2 = sin(32.2);
    print(a2);
    
    a3 = cos(98.32);
    print(a3);
    
    a4 = exp(2.3);
    print(a4);
    
    a5 = pow(a4,a3);
    print(a5);
    
    a5 = powi(a4,2);
    print(a5);
    
    a1 = exp(2.4);
    print(a1);
    
    a1 = log(2.2);
    print(a1);

    a1 = log10(2.2);
    print(a1);

    a1 = fabs(3.9);
    print(a1);
    
    a2 = min(2.0,3.0);
    print(a2);
    
    a2 = max(2.0,3.0);
    print(a2);
    
    a3 = rnd(a5);
    print(a3);




    c1 =vbBigidiot((9.0,11.2),(9.0,11.5));
    print(c1);
    a1 = vbsuperidiot(13.2, (7.0,2.5));
    print(a1);
    i2 = vblovesCOR( 2, 4);
    print(i2);

    print(s2);
    

    return 0;
}

cx vbBigidiot(cx x, cx y){
    cx result;
    result=(0.0,0.0);
    result[0]=x[0]+y[0];
    result[1]=y[1]+x[1];
    return result;
}

float vbsuperidiot(float x, cx y){
    float result;
    result= x+ y[0];
    return result;
}

int vblovesCOR( int a, int b){
    int result;
    result = a + b;
    return result;


} 


