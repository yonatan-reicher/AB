# I dont have a name yet yeah yeah :duck:
A very low level language which compiles directly into readable TASM to run on DOSBOX

This project was started for learning about tasm, about assmbly and about code translation.
I'm very proud of it in it's current state and will keep updating it until it is usable!

## Downloads and dependencies
You can download the .exe from [This link](https://drive.google.com/drive/folders/1VJPPnGVQNaFvDaarGW5-Mz5avbziCB5r?usp=sharing)

To use it you have to download and install DOSBOX and TASM from the interwebs

## Using The compiler 
Once you have downloaded it you need to have the .exe in a folder with the dlls and open the cmd there

Use command: 
`Asmb <file-name>` OR `Asmb <dosbox-path> <tasm-path> <file-name>` to compile the code and open it in dosbox. 

dosbox-path is the path to your dosbox.exe (defaults to "C:\Program Files (x86)\DOSBox-0.74-3\DOSBox.exe"), 
tasm-path is the path for your tasm folder where you put your .amb files (defaults to "C:\TASM"),
file-name is the path for the .ab you want to compile.

## Language and Syntax
Here is an example program: 
```
func void print(x byte) {
    push# x;
    ### pop dx      ###
    ### mov ah, 2h  ###
    ### int 21h     ###
    return;
}

func byte read_char() {
    ret byte = 0;	
    ### mov ah,00h      ###
    ### int 16h         ###
    ### mov ah, 0       ###
    ### push ax         ###
    pop# ret;
    return ret;
}

func byte main() {
  user_char byte;
  i byte = 10;
  
  user_char <- read_char(); 
  
  while i > 0 {
    print(user_char);
    i <- i - 1;
  }
  
  return 0;
}
```
As you can see, this program takes a char and prints it 10 times. 
Note that when running this a 0 will be added because that's the exit code.

This is translated into:
```
mov ax, @data
mov ds, ax
call main
pop dx
mov ah, 2
add dl, '0'
int 21h
exit:
        mov ax, 4C00h
        int 21h

proc print
        mov bp, sp

        ;       push# x
        mov al, [byte ptr bp+2]
        mov ah, 0
        push ax

        pop dx
        mov ah, 2h
        int 21h

        ;       return
        add sp, 0
        ret 2
endp print

proc read_char
        mov bp, sp

        ;       ret byte = 0
        push 0
        
        mov ah,00h
        int 16h
        mov ah, 0
        push ax

        ;       pop# ret
        pop ax
        mov [byte ptr bp-2], al

        ;       return ret
        mov al, [byte ptr bp-2]
        mov ah, 0
        add sp, 2
        pop dx
        add sp, 0
        mov ah, 0
        push ax
        push dx
        ret
endp read_char

proc main
        mov bp, sp

        ;       user_char byte
        push 0

        ;       i byte = 10
        push 10

        ;       user_char <- read_char()
        push bp
                call read_char
                pop ax
        pop bp
        mov ah, 0
        mov [byte ptr bp-2], al

        ;       while (i Greater 0)
        @@LoopLabel__i_Greater_0__:
        mov al, [byte ptr bp-4]
        mov ah, 0
        push ax
        mov ax, 0
        mov dx, ax
        pop ax
        cmp al, dl
        jng @@FalseLabel__i_Greater_0__
        push 1
        jmp @@TrueLabel__i_Greater_0__
        @@FalseLabel__i_Greater_0__:
        push 0
        @@TrueLabel__i_Greater_0__:
        pop ax
        cmp al, 0
        je @@EndLabel__i_Greater_0__

                ;       print(user_char)
                push bp
                        ;       parameter (user_char as byte)
                        mov al, [byte ptr bp-2]
                        mov ah, 0
                        push ax
                        call print
                pop bp
                add sp, 0

                ;       i <- (i Sub 1)
                mov al, [byte ptr bp-4]
                mov ah, 0
                push ax
                mov ax, 1
                mov dx, ax
                pop ax
                sub al, dl
                mov ah, 0
                mov [byte ptr bp-4], al
                jmp @@LoopLabel__i_Greater_0__
        @@EndLabel__i_Greater_0__:

        ;       return 0
        mov ax, 0
        add sp, 4
        pop dx
        add sp, 0
        mov ah, 0
        push ax
        push dx
        ret
endp main
```

This is not the best translation but the translator will be improved 

#### The language itself has many constructs: 
while, if-else (no regular if yet..), variables (declared at top of function), 
4 different sizes (byte, word, dword and void), assembly interop, parameters and return values and more.

`###` is used for interop. For example, `### mov ax, 0 ###` would move 0 to ax. And `push# var` would push `var` to the stack. 

Using this you can copy and paste assembly right into your program and use `int` instructions.

You can also use the +, -, *, /, % operators as you would in any language and also =, !=, >=, >, <=, < (note that it uses = and not ==).
The size of the expression for +, -, *, /, % is determined by the bigger size of both sides, and the size of =, !=, >=, >, <=, < is always byte.

Another feature is `pushpop` that allows you to store values and retrieve them later. 
For example after executing
```
pushpop var1, var2 {
  var1 <- read_char();
  var2 <- var1 + 1;
  print(var2);
}
```
the variables `var1` and `var2` will keep their original values.

