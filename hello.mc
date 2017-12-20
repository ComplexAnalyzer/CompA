

int main()
{   
    float a1;
    cx s2;
  

   
    a1= 93.2;
    s2 =vbigidiot((9.0,11.2),(9.0,11.5));

    print(s2);
    

    return 0;
}

cx vbigidiot(cx x, cx y){
    cx result;
    result=(0.0,0.0);
    result[0]=x[0]+y[0];
    result[1]=y[1]+x[1];

    return result;
}

