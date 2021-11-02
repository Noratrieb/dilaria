fn fizzbuzz() {
    let i = 0;
    while i < 100 {
        let out = single_fizzbuzz(i);
        print(out);
    }
}

fn single_fizzbuzz(n) {
    if n % 15 == 0 {
        return "FizzBuzz";
    } else if n % 5 == 0 {
        return "Buzz";
    } else if n % 3 == 0 {
        return "Fizz";
    } else {
        return n;
    }
}
