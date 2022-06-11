#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace std;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,
    // commands
    tok_def = -2,
    tok_extern = -3,

    tok_identifier = -4,
    tok_number = -5,
};

static string IdentifierStr; // Filled in if tok_identifier
static double NumVal;        // Filled in if tok_number

/// gettok - Return the next token from standard input.
static int gettok() { // static修饰的函数表示只能在这个cpp文件中调用
    static int LastChar = ' ';
    // Skip any whitespace, LastChar是第一个不为空格的字符
    while (isspace(LastChar)) { 
        LastChar = getchar();
    }
    // 第一个字符是字母
    if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        // 后续字符可以是字母和数字，直到不是
        while (isalnum(LastChar = getchar())) {
            IdentifierStr += LastChar;
        }
        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern") {
            return tok_extern;
        }
        return tok_identifier;
    }
    // 第一个字符书数字或者小数点，那么这个变量是数字
    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
        string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }
    // 井号开头的是注释，直到行末尾
    if (LastChar == '#') {
        // Comment until end of line.
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF) { // 这一行判断结束，如果没有到文件结尾，那么继续识别（递归）
            return gettok();
        }
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF) {
        return tok_eof;
    }
    // Otherwise, just return the character as its ascii value.
    // 如果是操作符，e.g.‘+’，则返回操作符，并且静态的字符变量为下一个字符
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}


//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

// parser用到的算法为 1.递归下降语法分析 2.算符优先分析法（用于二元运算符）
// parser的输出为抽象语法数(AST)
namespace {
/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
    virtual ~ExprAST() = default; // 定义虚函数是为了允许基类的指针调用子类的这个函数
};                                // = default 显示声明为默认的析构函数

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST: public ExprAST {
private:
    double val;
public:
    NumberExprAST(double val): val(val) {}
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST: public ExprAST {
private:
    string name;
public:
    VariableExprAST(const string &name): name(name) {}
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST: public ExprAST {
private:
    char op;
    unique_ptr<ExprAST> lhs, rhs;
public:
    BinaryExprAST(char op, unique_ptr<ExprAST> lhs, unique_ptr<ExprAST> rhs):
        op(op), lhs(move(lhs)), rhs(move(rhs)) {}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST: public ExprAST {
private:
    string callee;
    vector<unique_ptr<ExprAST> > args;
public:
    CallExprAST(const string &callee, vector<unique_ptr<ExprAST> > args):
        callee(callee), args(move(args)) {}
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
private:
    string name;
    vector<string> args;
public:
    PrototypeAST(const string &name, vector<string> args):
        name(name), args(move(args)) {}
    const string &getName() const {
        return name;
    }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
private:
    unique_ptr<PrototypeAST> proto;
    unique_ptr<ExprAST> body;
public:
    FunctionAST(unique_ptr<PrototypeAST> proto, unique_ptr<ExprAST> body):
        proto(move(proto)), body(move(body)) {}
};
 
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken()
{
    return CurTok = gettok();
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence()
{
    if (!isascii(CurTok)) {
        return -1;
    }
    // Make sure it's a declared binop.
    int tokPrec = BinopPrecedence[CurTok];
    if (tokPrec <= 0) {
        return -1;
    }
    return tokPrec;
}

/// LogError* - These are little helper functions for error handling.
unique_ptr<ExprAST> LogError(const char *str)
{
    fprintf(stderr, "LogError: %s\n", str);
    return nullptr;
}
unique_ptr<PrototypeAST> LogErrorP(const char *str)
{
    LogError(str);
    return nullptr;
}

static unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static unique_ptr<ExprAST> ParseNumberExpr()
{
    auto result = make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consume the number
    return move(result);
}

/// parenexpr ::= '(' expression ')'
static unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken(); // eat (.
    auto v = ParseExpression();
    if (!v) {
        return nullptr;
    }
    if (CurTok != ')') {
        return LogError("expected ')'");
    }

    getNextToken(); // eat ).
    return v;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static unique_ptr<ExprAST> ParseIdentifierExpr()
{
    string idName = IdentifierStr;
    getNextToken(); // eat identifier.
    if (CurTok != '(') { // Simple variable ref.
        return make_unique<VariableExprAST>(idName);
    }
    // Call.
    getNextToken(); // eat (
    vector<unique_ptr<ExprAST> > args;
    if (CurTok != ')') {
        while (1) {
            if (auto arg = ParseExpression()) {
                args.push_back(move(arg));
            } else {
                return nullptr;
            }

            if (CurTok == ')') {
                break;
            }

            if (CurTok != ',') {
                return LogError("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }
    // Eat the ')'.
    getNextToken();

    return make_unique<CallExprAST>(idName, move(args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        default:
            return LogError("Unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseIdentifierExpr();
    }
}

/// binoprhs
///   ::= ('+' primary)*
static unique_ptr<ExprAST> ParseBinOpRHS(int exprPrec, unique_ptr<ExprAST> lhs)
{
    // If this is a binop, find its precedence.
    while (true) {
        int tokPrec = GetTokPrecedence();
        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (tokPrec < exprPrec) {
            return lhs;
        }
        // Okay, we know this is a binop.
        int binOp = CurTok;
        getNextToken(); // eat binop
        // Parse the primary expression after the binary operator.
        auto rhs = ParsePrimary();
        if (!rhs) {
            return nullptr;
        }
        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int nextPrec = GetTokPrecedence();
        if (tokPrec < nextPrec) {
            // If BinOp binds less tightly with RHS than the operator after RHS, let
            // the pending operator take RHS as its LHS.
            int NextPrec = GetTokPrecedence();
            if (tokPrec < nextPrec) {
                rhs = ParseBinOpRHS(tokPrec + 1, std::move(rhs));
                if (!rhs)
                    return nullptr;
            }
        }
        // Merge LHS/RHS.
        lhs = make_unique<BinaryExprAST>(binOp, std::move(lhs),
            std::move(rhs));
    }
}

/// expression
///   ::= primary binoprhs
///
static unique_ptr<ExprAST> ParseExpression() {
    auto lhs = ParsePrimary();
    if (!lhs) {
        return nullptr;
    }
    return ParseBinOpRHS(0, move(lhs));
}

/// prototype
///   ::= id '(' id* ')'
static unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier) {
        return LogErrorP("Expected function name in prototype");
    }
    string fnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(') {
        return LogErrorP("Expected '(' in prototype");
    }
    // Read the list of argument names.
    vector<string> argNames;
    while (getNextToken() == tok_identifier) {
        argNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') {
        return LogErrorP("Expected ')' in prototype");
    }
    // success
    getNextToken(); // eat ')'

    return make_unique<PrototypeAST>(fnName, move(argNames));
}

/// definition ::= 'def' prototype expression
static unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken(); // eat def.
    auto proto = ParsePrototype();
    if (!proto) {
        return nullptr;
    }

    if (auto e = ParseExpression()) {
        return make_unique<FunctionAST>(move(proto), move(e));
    }
    return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto e = ParseExpression()) {
        // Make an anonymous proto.
        auto proto = make_unique<PrototypeAST>("__anon_expr", vector<string>());
        return make_unique<FunctionAST>(move(proto), move(e));
    }
    return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // eat extern.
    return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleDefinition()
{
    if (ParseDefinition()) {
        fprintf(stderr, "Parsed a function definition.\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern()
{
    if (ParseExtern()) {
        fprintf(stderr, "Parsed an extern\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    if (ParseTopLevelExpr()) {
        fprintf(stderr, "Parsed a top-level expr\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop()
{
    while (true) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
        case tok_eof:
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    // Run the main "interpreter loop" now.
    MainLoop();

    return 0;
}