# The Tic-Tac-Toe Multiverse [*TicTacToe-Verse*]

![Visitors](https://hits.seeyou.space/v2/counts/to/github.com/srijan-76448/tictactoe-verse/week.svg?style=plastic&color=blue)

## 🌌 Project Overview

Welcome to the **TicTacToe-Verse**! This repository is a unique collection of the classic Tic-Tac-Toe game, implemented in an ever-expanding array of programming languages. It serves as a fun and educational exploration of different language paradigms, syntax, and runtime environments, all through the lens of a simple, familiar game.

The goal is to showcase how a single problem can be solved across a wide spectrum of languages, from _high-level_, _object-oriented_, and _functional languages_ to _system-level_ and even _esoteric_ ones.

## ✨ Features

- **Diverse Implementations:** Tic-Tac-Toe written in numerous programming languages.
- **Console-Based:** All implementations are command-line applications (except for the HTML/CSS/JS version, which is web-based hosted live at [tttv.76448.org](https://tttv.76448.in)).
- **Basic Game Logic:** Each version includes core game mechanics: _board initialization_, _display_, _player turns_, _input handling_, _move validation_, and _checks for win and draw conditions_.
- **Educational Resource:** Great for comparing _language syntax_, _typical coding patterns_, and _setup processes across different ecosystems_.

## 🚀 Languages Included

This ~~multiverse~~ repository currently spans the following languages (and more to come!):

<details>
    <summary><b>Assembly (x86-64 Linux)</b> (<code>TicTacToe.asm</code>)</summary>

Steps to run:

- Install NASM assembler and a linker (e.g., `nasm`, `ld`).
- Compile:

```bash
  nasm -f elf64 TicTacToe.asm -o TicTacToe.o
```

- Link:

```bash
ld TicTacToe.o -o TicTacToe_asm_game
```

- Execute:

```bash
./TicTacToe_asm_game
```

</details>

<details>
    <summary><b>Batch Script</b> (<code>TicTacToe.bat</code>)</summary>

Steps to run:

- On Windows, simply double-click `TicTacToe.bat` in File Explorer.
- Alternatively, open Command Prompt, navigate to the file's directory, and run:

```bash
TicTacToe.bat
```

</details>

<details>
    <summary><b>Brainfuck</b> (<code>TicTacToe.bf</code>)</summary>

Steps to run:

- Install a Brainfuck interpreter (e.g., `bfi`, `bf`).
- Execute:

```bash
bfi TicTacToe.bf
```

> NOTE: <br>
> This version only displays a static board due to language limitations.

</details>

<details>
    <summary><b>C</b> (<code>TicTacToe.c</code>)</summary>

Steps to run:

- Install a C compiler (e.g., `gcc`).
- Compile:

```bash
gcc TicTacToe.c -o TicTacToe_c_game
```

- Execute:

```bash
./TicTacToe_c_game
```

</details>

<details>
    <summary><b>C++</b> (<code>TicTacToe.cpp</code>)</summary>

Steps to run:

- Install a C++ compiler (e.g., `g++`).
- Compile:

```bash
g++ TicTacToe.cpp -o TicTacToe_cpp_game
```

- Execute:

```bash
./TicTacToe_cpp_game
```

</details>

<details>
    <summary><b>C\#</b> (<code>TicTacToe.cs</code>)</summary>

Steps to run:

- Install .NET SDK.
- Execute:

```bash
dotnet run TicTacToe.cs
```

</details>

<details>
    <summary><b>Dart</b> (<code>TicTacToe.dart</code>)</summary>

Steps to run:

- Install Dart SDK.
- Execute:

```bash
dart run TicTacToe.dart
```

</details>

<details>
    <summary><b>Erlang</b> (<code>TicTacToe.erl</code>)</summary>

Steps to run:

- Install Erlang/OTP.
- Compile:

```bash
erlc TicTacToe.erl
```

- Start Erlang shell:

```bash
erl
```

- In Erlang shell, run:

```erlang
1> tic_tac_toe:start().
```

</details>

<details>
    <summary><b>Elixir</b> (<code>TicTacToe.ex</code>)</summary>

Steps to run:

- Install Elixir (which requires Erlang/OTP).
- Start IEx shell:

```bash
iex
```

- In IEx, compile and run:

```elixir
iex> c "TicTacToe.ex"
iex> TicTacToe.start()
```

</details>

<details>
    <summary><b>Fortran</b> (<code>TicTacToe.f90</code>)</summary>

Steps to run:

- Install a Fortran compiler (e.g., `gfortran`).
- Compile:

```bash
gfortran TicTacToe.f90 -o TicTacToe_f90_game
```

- Execute:

```bash
./TicTacToe_f90_game
```

</details>

<details>
    <summary><b>Go</b> (<code>TicTacToe.go</code>)</summary>

Steps to run:

- Install Go.
- Execute:

```bash
go run TicTacToe.go
```

</details>

<details>
    <summary><b>Groovy</b> (<code>TicTacToe.groovy</code>)</summary>

Steps to run:

- Install Groovy (requires JDK).
- Execute:

```bash
groovy TicTacToe.groovy
```

</details>

<details>
    <summary><b>Haskell</b> (<code>TicTacToe.hs</code>)</summary>

Steps to run:

- Install GHC (Glasgow Haskell Compiler).
- Compile:

```bash
ghc TicTacToe.hs
```

- Execute:

```bash
./TicTacToe
```

</details>

<details>
    <summary><b>HTML/CSS/JavaScript</b> (<code>TicTacToe.html</code>)</summary>

Steps to run:

- Open `TicTacToe.html` directly in your web browser.
- Or visit the live hosted version at [tttv.76448.org](https://tttv.76448.in).
</details>
<details>
    <summary><b>Java</b> (<code>TicTacToe.java</code>)</summary>

Steps to run:

- Install Java Development Kit (JDK).
- Compile:

```bash
javac TicTacToe.java
```

- Execute:

```bash
java TicTacToe
```

</details>

<details>
    <summary><b>Lisp (Common Lisp)</b> (<code>TicTacToe.lisp</code>)</summary>

Steps to run:

- Install a Common Lisp implementation (e.g., SBCL, CLISP).
- Start Lisp interpreter (e.g., `sbcl` or `clisp`).
- In the Lisp prompt, load and run:

```lisp
(load "TicTacToe.lisp")
(play-game)
```

</details>

<details>
    <summary><b>LOLCODE</b> (<code>TicTacToe.lol</code>)</summary>

Steps to run:

- Install a LOLCODE interpreter (e.g., `lci`).
- Execute:

```bash
lci TicTacToe.lol
```

> NOTE: <br>
> This version has simplified win/draw checks due to language verbosity.

</details>

<details>
    <summary><b>Lua</b> (<code>TicTacToe.lua</code>)</summary>

Steps to run:

- Install Lua.
- Execute:

```bash
lua TicTacToe.lua
```

</details>

<details>
    <summary><b>Nim</b> (<code>TicTacToe.nim</code>)</summary>

Steps to run:

- Install Nim.
- Compile and run:

```bash
nim c -r TicTacToe.nim
```

</details>

<details>
    <summary><b>PHP</b> (<code>TicTacToe.php</code>)</summary>

Steps to run:

- Install PHP.
- Execute:

```bash
php TicTacToe.php
```

</details>

<details>
    <summary><b>Prolog</b> (<code>TicTacToe.pl</code>)</summary>

Steps to run:

- Install a Prolog interpreter (e.g., `swipl`).
- Start Prolog interpreter:

```bash
swipl
```

- In Prolog prompt, consult and run:

```prolog
?- consult('TicTacToe.pl').
?- start_game.
```

</details>

<details>
    <summary><b>Python</b> (<code>TicTacToe.py</code>)</summary>

Steps to run:

- Install Python.
- Execute:

```bash
python TicTacToe.py
```

</details>

<details>
    <summary><b>R</b> (<code>TicTacToe.r</code>)</summary>

Steps to run:

- Install R.
- Execute:

```bash
Rscript TicTacToe.r
```

</details>

<details>
    <summary><b>Ruby</b> (<code>TicTacToe.rb</code>)</summary>

Steps to run:

- Install Ruby.
- Execute:

```bash
ruby TicTacToe.rb
```

</details>

<details>
    <summary><b>Rust</b> (<code>TicTacToe.rs</code>)</summary>

Steps to run:

- Install Rust (via `rustup`).
- Compile:

```bash
rustc TicTacToe.rs
```

- Execute:

```bash
./TicTacToe
```

</details>

<details>
    <summary><b>Bash/Shell Script</b> (<code>TicTacToe.sh</code>)</summary>

Steps to run:

- Make executable:

```bash
chmod +x TicTacToe.sh
```

- Execute:

```bash
./TicTacToe.sh
```

</details>

## 🤝 Contributing to the TicTacToe-Verse

Have a **Tic-Tac-Toe** implementation in another language? We welcome contributions to expand this verse\!

1.  Fork the repository.
2.  Create your feature branch (`git checkout -b feature/NewLanguageTicTacToe`).
3.  Implement Tic-Tac-Toe in your chosen language (keep it simple and console-based where possible).
4.  Add a brief comment block at the top of your file explaining how to compile/run it.
5.  Add your language to the `LANGUAGES INCLUDED` list in this `README.md`.
6.  Commit your changes (`git commit -m 'Add Tic-Tac-Toe in [New Language]'`).
7.  Push to the branch (`git push origin feature/NewLanguageTicTacToe`).
8.  Open a Pull Request\!

## 📜 License

This project is open-sourced under the [MIT License](/LICENSE).

---

Made with ❤️ by [Srijan Bhattacharyya/Srijan-76448]
