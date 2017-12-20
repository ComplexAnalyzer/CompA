cx euler(cx c){
    float a1;
    float a2;
    float a3;
    int i;
    cx result;

    result = (0.0,0.0);
    a1 = sin(c[1]);
    a2 = cos(c[1]);
    a3 = pow(exp(1.0), c[0]);

    result[0]= a3*a1;
    result[1]= a3*a2;
    return result;
}