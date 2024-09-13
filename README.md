# TIL compiler

Project of the 2023/24 Compilers Course. Consists of a compiler for the language TIL.

The steps to develop a full TIL compiler imply the adaptation of:
* the scanner (`til_scanner.l`)
* the parser (`til_parser.y`)
* the symbol (`targets/symbol.h`)
* the type checker (`targets/type_checker.cpp`)
* the XML writer (for the middle delivery: `targets/xml_writer.cpp`)
* the Postfix writer (for the final delivery: `targets/postfix_writer.cpp`)

TIL specifications (Portuguese):
https://web.tecnico.ulisboa.pt/~david.matos/w/pt/index.php/Compiladores/Projecto_de_Compiladores/Projecto_2023-2024/Manual_de_Refer%C3%AAncia_da_Linguagem_TIL

