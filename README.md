# decibelle
Programming language named after [Decibelle](https://wiki.cassettebeasts.com/wiki/Decibelle).

I made this after reading [Crafting Interpreters](https://craftinginterpreters.com/). It is similar to Lox (language wise and implementation wise), but has additional static typing.

## Example
```
let a = 1;
print(a);

let b: string = "Hello world";
print(b);

let i = 0;
while i < 10 {
    print(i);
    i = i + 1;
}

fn hello_world() {
    print("Hello world");
}
hello_world();

fn add(a: number) -> fn(number) -> number {
    fn add_fn(b: number) -> number {
        return a + b;
    }

    return add_fn;
}
print(add(2)(3));
```

## Running the interpreter
You can run a file using:
```
cargo run --release <filename>
```
You can run the REPL using:
```
cargo run --release
```
