/* Euler function */
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


/* Complex addition */
cx add_complex(cx a, cx b){
    cx result;
    result = (0.0,0.0);
    result[0] = a[0]+b[0];
    result[1] = a[1]+b[1];
    return result;
}


/* Complex subtraction */
cx sub_complex(cx a, cx b){
    cx result;
    result = (0.0,0.0);
    result[0] = a[0]-b[0];
    result[1] = a[1]-b[1];
    return result;
}


/* Complex multiplication */
cx mult_complex(cx a, cx b){
    cx result;
    result = (0.0,0.0);
    result[0] = a[0]*b[0]-a[1]*b[1];
    result[1] = a[0]*b[1]-a[1]*b[0];
    return result;
}


/* Complex division */
cx div_complex(cx a, cx b){
    cx result;
    result = (0.0,0.0);
    result[0] = (a[0]*b[0] + a[1]*b[1]) / (b[0]*b[0] + b[1]*b[1]);
    result[1] = (a[1]*b[0] - a[0]*b[1]) / (b[0]*b[0] + b[1]*b[1]);
    return result;
}


/* Complex power */
cx pow_complex(cx a, int n){
    cx result;
    int i;
    result = a;

    for (i = 0; i<(n-1); i=i+1){
        result =  mult_complex(result,a);
    }
    return result;
}


/* Complex magnitude */
float mag_complex(cx a){
    float result;
    result = sqrt(a[0]*a[0]+a[1]*a[1]);
    return result;
}


/* Complex conjugate */
cx conj_complex(cx a){
    cx result;
    float temp;
    temp = a[1];
    result = (0.0,0.0);
    result[0] = a[0];
    result[1] = 0.0-temp;
    return result;
}
