n = 4; 

def main() 
{
    a = open("input.txt", "r"); 
    f = get_input(a); 
    close(a); 
    w = PLA(f);
    output_results(w);
}

def output_results(w) {
    a = open("results.txt", "c");
    write(a, "Optimal Weights: ");
    (0<=i<#w) write(a, w[i]);
    close(a);
}

/* Returns the data from file in a set */
def get_input(a) {
    file = read(a);
    b = split(file, ",");
    c = ["1"]; 
    d = [];
    (0<=i<#b) {
        e = split(b[i], " ");
        new = c + e;
        d = d + new;
    }
    f = [];
    (0<=i<#d) f = f + [str_to_int(d[i])]; 
    return f; 
}

/* Performs PLA on the set a */ 
def PLA(a) {
    w = [0, 0, 0];
    change = true;
    while(change) {
        change = false;
        (0<=i<#a/n | f(a[n*i:(n*(i+1)-1)], w) * a[n*(i+1)-1] <= 0) {
            change = true;
            adjust(a[n*i:(n*(i+1))], w);
        }
    }
    print("Optimal Weights");
    (0<=j<#w) print(w[j]);
    return w;
}

/* Heuristic function for PLA */
def f(data, w) {
    sum = 0; 
    (0<=i<#data) sum = sum + w[i] * data[i];
    if (sum > 0) return 1; 
    else {if (sum < 0) return -1; else return 0; }
}

/* Adjusts the weights */
def adjust(data, w) {
    (0<=i<#w) w[i] = w[i] + data[#data-1] * data[i];
}

