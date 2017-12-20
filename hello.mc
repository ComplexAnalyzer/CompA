int main()
{   
    float a1;
    float a2;
    float a3;
    float e;
    float l;
    cx c;
    cx result;


    result = (0.0,0.0);
    c = (2.2,2.3);
   
   
    
    a1 = sin(c[1]);
    a2 = cos(c[1]);

    a3 = pow(exp(1.0), c[0]);

    result[0]= min(2.0,3.0);
    result[1]= max(5.0,2.0);
    l= fabs(0.2);
    a3=rnd(9.4);
   
    print(l);
    print(result);
    print(a3);
    

    return 0;
}
