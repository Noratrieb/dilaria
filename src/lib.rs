pub fn process_ast(program: &str, ast: &Program) {
    dbg(ast);
}

pub fn dbg(x: impl dbg_pls::DebugPls) {
    eprintln!("{}", dbg_pls::pretty(&x))
}

#[derive(dbg_pls::DebugPls)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

type Symbol = usize;

#[derive(dbg_pls::DebugPls)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

pub type Program<'ast> = Block<'ast>;

#[derive(dbg_pls::DebugPls)]
pub struct Block<'ast> {
    pub stmts: &'ast [Stmt<'ast>],
    pub span: Span,
}

#[derive(dbg_pls::DebugPls)]
pub enum Stmt<'ast> {
    FnDecl(FnDecl<'ast>),
    Loop(Block<'ast>, Span),
    While(WhileStmt<'ast>),
    Break(Span),
    Return(Option<Expr<'ast>>, Span),
}

#[derive(dbg_pls::DebugPls)]
pub struct FnDecl<'ast> {
    pub span: Span,
    pub name: Ident,
    pub params: &'ast [Ident],
    pub body: Block<'ast>,
}

#[derive(dbg_pls::DebugPls)]
pub struct WhileStmt<'ast> {
    pub span: Span,
    pub cond: Expr<'ast>,
    pub body: Block<'ast>,
}

#[derive(dbg_pls::DebugPls)]
pub enum Expr<'ast> {
    Ident(Ident),
    Literal(Literal<'ast>),
    UnaryOp(&'ast UnaryOp<'ast>),
    BinaryOp(&'ast BinaryOp<'ast>),
    Call(&'ast Call<'ast>),
}

impl Expr<'_> {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal(lit) => lit.span(),
            Expr::UnaryOp(unary) => unary.span,
            Expr::BinaryOp(binary) => binary.span,
            Expr::Ident(Ident { span, .. }) => *span,
            Expr::Call(call) => call.span,
        }
    }
}

#[derive(dbg_pls::DebugPls)]
pub enum Literal<'ast> {
    String(Symbol, Span),
    Number(f64, Span),
    Array(&'ast [Expr<'ast>], Span),
    Object(Span),
    Boolean(bool, Span),
    Null(Span),
}

impl Literal<'_> {
    pub fn span(&self) -> Span {
        match self {
            Literal::String(_, span)
            | Literal::Number(_, span)
            | Literal::Array(_, span)
            | Literal::Object(span)
            | Literal::Boolean(_, span)
            | Literal::Null(span) => *span,
        }
    }
}

#[derive(dbg_pls::DebugPls)]
pub struct UnaryOp<'ast> {
    pub span: Span,
    pub expr: Expr<'ast>,
    pub kind: UnaryOpKind,
}

#[derive(dbg_pls::DebugPls)]
pub enum UnaryOpKind {
    Not,
    Neg,
}

#[derive(dbg_pls::DebugPls)]
pub struct BinaryOp<'ast> {
    pub span: Span,
    pub lhs: Expr<'ast>,
    pub rhs: Expr<'ast>,
    pub kind: BinaryOpKind,
}

#[derive(dbg_pls::DebugPls)]
pub enum BinaryOpKind {
    And,
    Or,
    Equal,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    NotEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(dbg_pls::DebugPls)]
pub struct Call<'ast> {
    pub callee: Expr<'ast>,
    pub span: Span,
    pub kind: CallKind<'ast>,
}

#[derive(dbg_pls::DebugPls)]
pub enum CallKind<'ast> {
    Field(Ident),
    Fn(&'ast [Expr<'ast>]),
}
