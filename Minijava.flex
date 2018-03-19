/*
 * JFlex specification for the lexical analyzer for a simple demo language.
 * Change this into the scanner for your implementation of MiniJava.
 * CSE 401/P501 Au11
 */


package Scanner;

import java_cup.runtime.*;
import Parser.sym;

%%

%public
%final
%class scanner
%unicode
%cup
%line
%column

/* Code copied into the generated scanner class.  */
/* Can be referenced in scanner action code. */
%{
  // Return new symbol objects with line and column numbers in the symbol 
  // left and right fields. This abuses the original idea of having left 
  // and right be character positions, but is   // is more useful and 
  // follows an example in the JFlex documentation.
  private Symbol symbol(int type) {
    return new Symbol(type, yyline+1, yycolumn+1);
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline+1, yycolumn+1, value);
  }

  // Return a readable representation of symbol s (aka token)
  public String symbolToString(Symbol s) {
    String rep;
    switch (s.sym) {
      case sym.BECOMES: return "BECOMES";
      case sym.SEMICOLON: return "SEMICOLON";
      case sym.PLUS: return "PLUS";
      case sym.LPAREN: return "LPAREN";
      case sym.RPAREN: return "RPAREN";
      case sym.EOF: return "<EOF>";
      case sym.error: return "<ERROR>";
      case sym.INTEGER: return "INTEGER("+(String)s.value+")";
      case sym.MINUS: return "MINUS";
      case sym.TIMES: return "TIMES";
      case sym.AND: return "AND";
      case sym.LOWER: return "LOWER";
      case sym.IF: return "IF";
      case sym.ELSE: return "ELSE";
      case sym.WHILE: return "WHILE";
      case sym.SYSTEM_OUT_PRINTLN: return "SYSTEM_OUT_PRINTLN";
      case sym.CLASS: return "CLASS";
      case sym.PUBLIC: return "PUBLIC";
      case sym.STATIC: return "STATIC";
      case sym.VOID: return "VOID";
      case sym.MAIN: return "MAIN";
      case sym.EXTENDS: return "EXTENDS";
      case sym.RETURN: return "RETURN";
      case sym.INT: return "INT";
      case sym.BOOLEAN: return "BOOLEAN";
      case sym.LENGTH: return "LENGTH";
      case sym.TRUE: return "TRUE";
      case sym.FALSE: return "FALSE";
      case sym.THIS: return "THIS";
      case sym.NEW: return "NEW";
      case sym.DOT: return "DOT";
      case sym.COMMA: return "COMMA";
      case sym.STRING: return "STRING"; 
      case sym.LBRACKET: return "LBRACKET"; 
      case sym.RBRACKET: return "RBRACKET"; 
      case sym.LCURL: return "LCURL"; 
      case sym.RCURL: return "RCURL";
      case sym.NOT: return "NOT";        
      case sym.IDENTIFIER: return "ID(" + (String)s.value + ")";
      default: return "<UNEXPECTED TOKEN " + s.toString() + ">";
    }
  }
%}

/* Helper definitions */
letter = [a-zA-Z]
digit = [0-9]
pre_digit = [1-9]
eol = [\r\n]
white = {eol}|[ \t]

%%

/* Token definitions */

/* reserved words */
/* (put here so that reserved words take precedence over identifiers) */

"if" {return symbol(sym.IF); }
"else" { return symbol(sym.ELSE); }
"while" {return symbol(sym.WHILE); }
"System.out.println" {return symbol(sym.SYSTEM_OUT_PRINTLN);}
"class" { return symbol(sym.CLASS); }
"public" { return symbol(sym.PUBLIC); }
"static" { return symbol(sym.STATIC); }
"void" { return symbol(sym.VOID); }
"main" { return symbol(sym.MAIN); }
"extends" { return symbol(sym.EXTENDS); }
"return" { return symbol(sym.RETURN); }
"int" { return symbol(sym.INT); }
"boolean" { return symbol(sym.BOOLEAN); }
"length" { return symbol(sym.LENGTH); }
"true" { return symbol(sym.TRUE); }
"false" { return symbol(sym.FALSE); }
"this" { return symbol(sym.THIS); }
"new" { return symbol(sym.NEW); }
"." { return symbol(sym.DOT); }
"," { return symbol(sym.COMMA); }
"String" {return symbol(sym.STRING);}


/* operators */
"+" { return symbol(sym.PLUS); }
"=" { return symbol(sym.BECOMES); }
"-" { return symbol(sym.MINUS); }
"*" { return symbol(sym.TIMES); }
"&&" { return symbol(sym.AND); }
"<" { return symbol(sym.LOWER); }
"!" { return symbol(sym.NOT);}

/* delimiters */
"(" { return symbol(sym.LPAREN); }
")" { return symbol(sym.RPAREN); }
";" { return symbol(sym.SEMICOLON); }
"[" { return symbol(sym.LBRACKET);}
"]" { return symbol(sym.RBRACKET);}
"{" { return symbol(sym.LCURL); }
"}" { return symbol(sym.RCURL); }


/* identifiers */
{letter} ({letter}|{digit}|_)* { return symbol(sym.IDENTIFIER, yytext()); }

/* INTEGER */
{digit}|{pre_digit}{digit}* {return symbol(sym.INTEGER, yytext());}

/* whitespace */
{white}+ { /* ignore whitespace */ }

/* lexical errors (put last so other matches take precedence) */
. { System.err.println(
	"\nunexpected character in input: '" + yytext() + "' at line " +
	(yyline+1) + " column " + (yycolumn+1));
  }